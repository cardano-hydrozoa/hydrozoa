package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.lib.tx.TransactionBuilder.setMinAda
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.Token.mkHeadTokenName
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.Initialization
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scalus.builtin.Data.toData
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.*
import scalus.cardano.ledger.txbuilder.TxBalancingError
import cats.data.NonEmptyList
import cats.syntax.all.*
import hydrozoa.lib.tx.BuildError

import scala.collection.immutable.SortedMap
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import org.scalacheck.Gen.choose
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Test as ScalaCheckTest}
import test.TestPeer.*
import test.*

// The minimum ada required for the initial treasury utxo
val minInitTreasuryAda: Coin = {
    val mockTreasury = Babbage(
      // This is a pubkey address, but the script address should be the same size
      address = genPubkeyAddr().sample.get,
      value = Value(
        Coin(0L),
        multiAsset = MultiAsset(
          SortedMap(
            (
              genPolicyId.sample.get,
              SortedMap(
                (mkHeadTokenName(NonEmptyList.one(genAdaOnlyPubKeyUtxo(Alice).sample.get._1)), 1L)
              )
            )
          )
        )
      ),
      datumOption = Some(Inline(TreasuryUtxo.mkInitMultisigTreasuryDatum.toData)),
      scriptRef = None
    )
    setMinAda(mockTreasury, blockfrost544Params).value.coin
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
      context = unsignedTxBuilderContext(Map.from(seedUtxos.toList)),
      changeAddress = peers.head.address
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

class InitializationTxTest extends munit.ScalaCheckSuite {
    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(10_000)
    }

    // TODO: replace with variant that is not generated
    val dummyAddr: ShelleyAddress = genPubkeyAddr().sample.get

    test("Test minAda violation in the treasury") {
        ////
        // General data setup

        // Seed UTxO with 100 ADA
        val seedUtxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
            .focus(_._2.value.coin.value)
            .replace(100_000_000L)

        val peers = NonEmptyList.fromListUnsafe(List(Alice, Bob))
        val peerVKeys = peers.map(_.wallet.exportVerificationKeyBytes)

        // This recipe should have exactly the min ADA
        val recipeSucceed = InitializationTx.Recipe(
          seedUtxos = NonEmptyList.one(seedUtxo),
          initialDeposit = minInitTreasuryAda,
          peers = peerVKeys,
          context = unsignedTxBuilderContext(Map(seedUtxo)),
          changeAddress = Alice.address
        )

        // This recipe should have 1 lovelace less than the minimum acceptable ADA
        val recipeFail = recipeSucceed.focus(_.initialDeposit).modify(_ - Coin(1L))

        InitializationTx.build(recipeFail) match {
            case Left(
                  BuildError.ValidationError(
                    e: TransactionException.OutputsHaveNotEnoughCoinsException
                  )
                ) =>
                ()
            case Right(_) => throw RuntimeException("Build succeeded, but should have failed")
            case Left(e)  => throw RuntimeException(s"Build failed, but for the wrong reason: $e")
        }

        InitializationTx.build(recipeSucceed) match {
            case Left(e)  => throw RuntimeException("Build failed but should have succeeded")
            case Right(_) => ()
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
          context = unsignedTxBuilderContext(Map.from(List(seedUtxo))),
          changeAddress = Alice.address
        )

        InitializationTx.build(recipe) match {
            case Left(
                  BuildError.BalancingError(
                    e: TxBalancingError.InsufficientFunds
                  )
                ) =>
                ()
            case Right(_) => throw RuntimeException("Build succeeded, but should have failed")
            case Left(e)  => throw RuntimeException(s"Build failed, but for the wrong reason: $e")
        }
    }

    property("Build initialization tx")(
      Prop.forAll(genInitTxRecipe()) { recipe =>
          InitializationTx.build(recipe) match {
              case Left(e) => throw RuntimeException(s"Build failed $e")
              case Right(tx) =>
                  val headMultisigScript = HeadMultisigScript(recipe.peers)
                  val headTokenName = mkHeadTokenName(recipe.seedUtxos.map(_._1))

                  val bytes = tx.tx.toCbor
                  given OriginalCborByteArray = OriginalCborByteArray(bytes)
                  (tx.tx == Cbor
                      .decode(bytes)
                      .to[Transaction]
                      .value) :| "Cbor round-tripping failed"
                  &&
                  (tx.tx.body.value.fee.value != 0L) :| "Tx Fee should not be 0"
                  && (tx.tx.body.value.outputs.size === 2) :| "Initialization tx should have a treasury output and" +
                      "change output"
                      &&
                      (tx.treasuryProduced.toUtxo._2 ==
                          tx.tx.body.value.outputs.head.value) :|
                      "treasury output in InitializationTx value not coherent with actual transaction produced"
                      && (
                        tx.tx.witnessSet.nativeScripts.head == headMultisigScript.script
                      ) :| "Head multisig script not as expected"
                      && (tx.treasuryProduced.headTokenName == headTokenName) :| "Unexpected head token name in treasury output"
                      && (tx.treasuryProduced.toUtxo._2.value.assets.assets
                          .get(headMultisigScript.policyId)
                          .get(
                            headTokenName
                          ) === 1L) :| "treasury output does not contain correct head token"
                      && {
                          val actual = tx.treasuryProduced.toUtxo._2.value
                          val expected = Value(
                            coin = recipe.initialDeposit,
                            multiAsset = MultiAsset(assets =
                                SortedMap(
                                  (headMultisigScript.policyId, SortedMap((headTokenName, 1L)))
                                )
                            )
                          )
                          (actual == expected) :| s"Unexpected treasury value. Actual: $actual, expected: $expected"
                      }
                      && tx.tx.auxiliaryData.contains(
                        MD.apply(Initialization, headMultisigScript.address(Mainnet))
                      )
                      :| "Unexpected metadata"
          }
      }
    )

}
