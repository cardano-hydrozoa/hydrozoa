package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import cats.syntax.all.*
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.Initialization
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import org.scalacheck.Gen.choose
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.annotation.nowarn
import scala.collection.immutable.SortedMap
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.cardano.txbuilder.{SomeBuildError, TxBalancingError}
import test.*
import test.TestPeer.*

// The minimum ada required for the initial treasury utxo
val minInitTreasuryAda: Coin = {
    val mockTreasury = Babbage(
      // This is a pubkey address, but the script address should be the same size
      address = genPubkeyAddress().sample.get,
      value = Value(
        Coin(0L),
        assets = MultiAsset(
          SortedMap(
            (
              genPolicyId.sample.get,
              SortedMap(
                (CIP67.TokenNames(genAdaOnlyPubKeyUtxo(Alice).sample.get._1).headTokenName, 1L)
              )
            )
          )
        )
      ),
      datumOption = Some(Inline(TreasuryUtxo.mkInitMultisigTreasuryDatum.toData)),
      scriptRef = None
    )
    ensureMinAda(mockTreasury, blockfrost544Params).value.coin
}

def genInitTxRecipe(
    estimatedFee: Coin = Coin(5_000_000L)
): Gen[InitializationTx.Recipe] =
    for {
        peers <- genTestPeers
        // Seed UTxOs all belong to the first peer. Each utxo has minAda; the sum of their values must
        // exceed the minInitTreasuryAda value + an estimated fee, plus minAda for the change output.
        seedUtxos <- Gen
            .nonEmptyListOf(genAdaOnlyPubKeyUtxo(peers.head))
            .map(NonEmptyList.fromList(_).get)
            .suchThat(x =>
                sumUtxoValues(
                  x.toList
                ).coin.value > minInitTreasuryAda.value + estimatedFee.value + minPubkeyAda().value
            )

        // Initial deposit must be at least enough for the minAda of the treasury, and no more than the
        // sum of the seed utxos, while leaving enough left for the estimated fee and the minAda of the change
        // output
        initialDeposit <- choose(
          minInitTreasuryAda.value,
          sumUtxoValues(seedUtxos.toList).coin.value - estimatedFee.value - minPubkeyAda().value
        ).map(Coin(_))

    } yield InitializationTx.Recipe(
      seedUtxos = seedUtxos,
      initialDeposit = initialDeposit,
      peers = peers.map(_.wallet.exportVerificationKeyBytes),
      network = testNetwork,
      protocolParams = testProtocolParams,
      evaluator = testEvaluator,
      validators = testValidators,
      changeAddress = peers.head.address(testNetwork)
    )

// NOTE: This was just for practice with scalacheck shrinkers. Don't use this -- it is not a good shrinker.
//
//
//implicit def shrinkInitTxRecipe: Shrink[InitializationTx.Recipe] = Shrink { recipe =>
//    (for (shrunk <- shrink(recipe.seedUtxos))
//        yield (recipe.copy(seedUtxos = shrunk))) lazyAppendedAll
//        (for (shrunk <- Shrink.shrink(recipe.initialDeposit.value))
//            yield (recipe.copy(initialDeposit = Coin(shrunk)))) lazyAppendedAll(
//        (for (shrunk <- Shrink.shrink(recipe.peers)) yield (recipe.copy(peers = shrunk)))
//    )
//
//}

@nowarn("msg=unused value")
class InitializationTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)

    val anyAddr: ShelleyAddress = Arbitrary.arbitrary[ShelleyAddress].sample.get

    // TODO: this test doesn't work since the new builder set minAda in all outputs by default
    ignore("Test minAda violation in the treasury") {

        // Seed UTxO with 100 ADA
        val seedUtxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
            .focus(_._2.value.coin.value)
            .replace(100_000_000L)

        val peers = NonEmptyList.fromListUnsafe(List(Alice, Bob))
        val peerVKeys = peers.map(_.wallet.exportVerificationKeyBytes)

        val recipeMinAda = InitializationTx.Recipe(
          seedUtxos = NonEmptyList.one(seedUtxo),
          // This recipe should have exactly the min ADA in the treasury
          initialDeposit = minInitTreasuryAda,
          peers = peerVKeys,
          network = testNetwork,
          protocolParams = testProtocolParams,
          evaluator = testEvaluator,
          validators = testValidators,
          changeAddress = Alice.address(testNetwork)
        )

        InitializationTx.build(recipeMinAda) match {
            case Left(e) => fail("Build failed but should have succeeded")
            case Right(tx) =>
                println(HexUtil.encodeHexString(tx.tx.toCbor))
        }

        // This recipe should have 1 lovelace less than the minimum acceptable ADA
        val recipeLessThanMinAda = recipeMinAda.focus(_.initialDeposit).modify(_ - Coin(1L))

        InitializationTx.build(recipeLessThanMinAda) match {
            case Left(
                  SomeBuildError.ValidationError(
                    e: TransactionException.OutputsHaveNotEnoughCoinsException
                  )
                ) =>
                ()
            case Right(tx) =>
                println(HexUtil.encodeHexString(tx.tx.toCbor))
                fail("Build succeeded, but should have failed")
            case Left(e) =>
                fail(s"Build failed, but for the wrong reason: $e")
        }
    }

//    test("MinAda incoherence between l1 and l2") {
//        ???
//    }

    test("Enough ada for minAda in treasury and change utxo, but insufficient ada to pay for fee") {
        val seedUtxo = {
            val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
            utxo.focus(_._2.value.coin).replace(minPubkeyAda() + minInitTreasuryAda)
        }

        val recipe = InitializationTx.Recipe(
          seedUtxos = NonEmptyList.one(seedUtxo),
          initialDeposit = minInitTreasuryAda,
          peers = NonEmptyList.one(Alice.wallet.exportVerificationKeyBytes),
          network = testNetwork,
          protocolParams = testProtocolParams,
          evaluator = testEvaluator,
          validators = testValidators,
          changeAddress = Alice.address(testNetwork)
        )

        InitializationTx.build(recipe) match {
            case Left(
                  SomeBuildError.BalancingError(
                    e: TxBalancingError.InsufficientFunds
                  )
                ) =>
                ()
            case Right(_) => fail("Build succeeded, but should have failed")
            case Left(e)  => fail(s"Build failed, but for the wrong reason: $e")
        }
    }

    test("Build initialization tx") {
        forAll(genInitTxRecipe()) { recipe =>
            InitializationTx.build(recipe) match {
                case Left(e) => fail(s"Build failed $e")
                case Right(tx) =>
                    val headMultisigScript = HeadMultisigScript(recipe.peers)
                    val headTokenName =
                        CIP67.TokenNames(recipe.seedUtxos.map(_._1).head).headTokenName

                    val bytes = tx.tx.toCbor
                    given OriginalCborByteArray = OriginalCborByteArray(bytes)

                    // println(HexUtil.encodeHexString(bytes))

                    assertResult(tx.tx, "Cbor round-tripping failed")(
                      Cbor.decode(bytes).to[Transaction].value
                    )
                    assert(tx.tx.body.value.fee.value != 0L, "Tx Fee should not be 0")
                    assert(
                      tx.tx.body.value.outputs.size == 2,
                      "Initialization tx should have a treasury output and change output"
                    )
                    assert(
                      tx.treasuryProduced.asUtxo._2 == tx.tx.body.value.outputs.head.value,
                      "treasury output in InitializationTx value not coherent with actual transaction produced"
                    )
                    assert(
                      tx.tx.witnessSet.nativeScripts.head == headMultisigScript.script,
                      "Head multisig script not as expected"
                    )
                    assert(
                      tx.treasuryProduced.headTokenName == headTokenName,
                      "Unexpected head token name in treasury output"
                    )
                    assert(
                      tx.treasuryProduced.asUtxo._2.value.assets.assets
                          .get(headMultisigScript.policyId)
                          .get(headTokenName) == 1L,
                      "treasury output does not contain correct head token"
                    )

                    val actual = tx.treasuryProduced.asUtxo._2.value
                    val expected = Value(
                      coin = recipe.initialDeposit,
                      assets = MultiAsset(assets =
                          SortedMap(
                            (headMultisigScript.policyId, SortedMap((headTokenName, 1L)))
                          )
                      )
                    )
                    assert(
                      actual == expected,
                      s"Unexpected treasury value. Actual: $actual, expected: $expected"
                    )

                    val actualMeta = tx.tx.auxiliaryData.map(_.value)
                    val expectedMeta =
                        MD.apply(Initialization, headMultisigScript.mkAddress(testNetwork))
                    assert(
                      actualMeta.contains(expectedMeta),
                      s"Unexpected metadata value. Actual: $actualMeta, expected: $expectedMeta"
                    )
            }
        }
    }

}
