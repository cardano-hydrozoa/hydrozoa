package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import cats.syntax.all.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.cardano.txbuilder.TransactionUnspentOutput
import test.*
import test.TestPeer.*

import scala.collection.immutable.SortedMap

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

val genInitTxRecipe: Gen[InitializationTx.Recipe] =
    for {
        peers <- genTestPeers

        spentUtxos <- Gen
            .nonEmptyListOf(genAdaOnlyPubKeyUtxo(peers.head)).map(NonEmptyList.fromListUnsafe)

//        // Initial deposit must be at least enough for the minAda of the treasury, and no more than the
//        // sum of the seed utxos, while leaving enough left for the estimated fee and the minAda of the change
//        // output
//        initialDeposit <- choose(
//          minInitTreasuryAda.value,
//          sumUtxoValues(seedUtxos.toList).coin.value - estimatedFee.value - minPubkeyAda().value
//        ).map(Coin(_))

    }
        yield InitializationTx.Recipe(
          seedUtxo = spentUtxos.head._1,
          spentUtxos = spentUtxos.map(utxo => TransactionUnspentOutput(utxo._1, utxo._2)),
          initialDeposit = Coin(0L),
          fallbackCoin = Coin(0L),
          peers = peers.map(_.wallet.exportVerificationKeyBytes),
          env = testTxBuilderEnvironment,
          validators = testValidators,
           changeAddress =
                ShelleyAddress(network = testTxBuilderEnvironment.network,
                Key(AddrKeyHash.fromByteString(ByteString.fill(28, 1.toByte))),
                delegation = Null)
    ).get


object InitializationTxTest extends Properties("InitializationTx") {
    property("InitializationTx Happy Path") = Prop.forAll(genInitTxRecipe) {
        recipe =>
            InitializationTx.build(recipe).isRight :| "successful build" && {
                val iTx = InitializationTx.build(recipe).fromRight
                val headNativeScript = HeadMultisigScript(recipe.peers)
                val headTokenName = CIP67.TokenNames(recipe.seedUtxo).headTokenName
                val mulitsigRegimeTokenName = CIP67.TokenNames(recipe.seedUtxo).multisigRegimeTokenName

                recipe.spentUtxos.toList.map(iTx.tx.body.value.inputs.toSeq.contains(_)).reduce(_ && _)
                    :| "Configured inputs are spent"
                    && iTx.tx.body.value.inputs.toSeq.contains(recipe.seedUtxo) :| "Seed input is spent"
                    && (iTx.tx.body.value.mint ==
                            Some(Mint(MultiAsset(SortedMap(
                                headNativeScript.policyId -> SortedMap(headTokenName -> 1L),
                                headNativeScript.policyId -> SortedMap(mulitsigRegimeTokenName -> 1L))))))
                    :| "Only Treasury token and mulReg tokens minted"
                    && {
                    // Treasury output checks
                  (iTx.tx.body.value.outputs.map(_.value)(iTx.treasuryProduced.asUtxo.input.index) ==
                    iTx.treasuryProduced.asUtxo.output)
                    :| "initialization tx contains treasury output at correct index"
                    && (iTx.tx.id == iTx.treasuryProduced.asUtxo.input.transactionId)
                    :| "initialization tx id coherent with produced treasury output"
                    && (iTx.treasuryProduced.asUtxo.output.value.assets ==
                    MultiAsset(SortedMap(headNativeScript.policyId -> SortedMap(headTokenName -> 1L))))
                    :| "treasury utxo only contains head token in multiassets"
                    && (iTx.treasuryProduced.asUtxo.output.value.coin >= recipe.initialDeposit)
                    :| "treasury utxo contains at least initial deposit"
                } && {
                  (iTx.tx.body.value.outputs.map(_.value)(iTx.multisigRegimeUtxo.input.index) ==
                    iTx.multisigRegimeUtxo.output)
                    :| "initialization tx contains MR output at correct index"
                    && (iTx.tx.id == iTx.multisigRegimeUtxo.input.transactionId)
                    :| "initialization tx id coherent with produced MR output"
                    && (iTx.multisigRegimeUtxo.output.value.assets ==
                    MultiAsset(SortedMap(headNativeScript.policyId -> SortedMap(headTokenName -> 1L))))
                    :| "MR utxo only contains MR token in multiassets"
                    && (iTx.multisigRegimeUtxo.output.value.coin >= recipe.fallbackCoin)
                    :| "MR utxo contains at least enough coin for fallback deposit"
                }
            }
    }

}

//    val anyAddr: ShelleyAddress = Arbitrary.arbitrary[ShelleyAddress].sample.get
//
//    // TODO: this test doesn't work since the new builder set minAda in all outputs by default
//    test("Test minAda violation in the treasury".ignore) {
//
//        // Seed UTxO with 100 ADA
//        val seedUtxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
//            .focus(_._2.value.coin.value)
//            .replace(100_000_000L)
//
//        val peers = NonEmptyList.fromListUnsafe(List(Alice, Bob))
//        val peerVKeys = peers.map(_.wallet.exportVerificationKeyBytes)
//
//        val recipeMinAda = InitializationTx.Recipe(
//          seedUtxos = NonEmptyList.one(seedUtxo),
//          // This recipe should have exactly the min ADA in the treasury
//          initialDeposit = minInitTreasuryAda,
//          peers = peerVKeys,
//          network = testNetwork,
//          protocolParams = testProtocolParams,
//          evaluator = testEvaluator,
//          validators = testValidators,
//          changeAddress = Alice.address
//        )
//
//        InitializationTx.build(recipeMinAda) match {
//            case Left(e) => throw RuntimeException("Build failed but should have succeeded")
//            case Right(tx) =>
//                println(HexUtil.encodeHexString(tx.tx.toCbor))
//        }
//
//        // This recipe should have 1 lovelace less than the minimum acceptable ADA
//        val recipeLessThanMinAda = recipeMinAda.focus(_.initialDeposit).modify(_ - Coin(1L))
//
//        InitializationTx.build(recipeLessThanMinAda) match {
//            case Left(
//                  SomeBuildError.ValidationError(
//                    e: TransactionException.OutputsHaveNotEnoughCoinsException
//                  )
//                ) =>
//                ()
//            case Right(tx) =>
//                println(HexUtil.encodeHexString(tx.tx.toCbor))
//                throw RuntimeException("Build succeeded, but should have failed")
//            case Left(e) =>
//                throw RuntimeException(s"Build failed, but for the wrong reason: $e")
//        }
//    }
//
////    test("MinAda incoherence between l1 and l2") {
////        ???
////    }
//
//    test("Enough ada for minAda in treasury and change utxo, but insufficient ada to pay for fee") {
//        val seedUtxo = {
//            val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
//            utxo.focus(_._2.value.coin).replace(minPubkeyAda() + minInitTreasuryAda)
//        }
//
//        val recipe = InitializationTx.Recipe(
//          seedUtxos = NonEmptyList.one(seedUtxo),
//          initialDeposit = minInitTreasuryAda,
//          peers = NonEmptyList.one(Alice.wallet.exportVerificationKeyBytes),
//          network = testNetwork,
//          protocolParams = testProtocolParams,
//          evaluator = testEvaluator,
//          validators = testValidators,
//          changeAddress = Alice.address
//        )
//
//        InitializationTx.build(recipe) match {
//            case Left(
//                  SomeBuildError.BalancingError(
//                    e: TxBalancingError.InsufficientFunds
//                  )
//                ) =>
//                ()
//            case Right(_) => throw RuntimeException("Build succeeded, but should have failed")
//            case Left(e)  => throw RuntimeException(s"Build failed, but for the wrong reason: $e")
//        }
//    }
//
//    property("Build initialization tx")(
//      Prop.forAll(genInitTxRecipe()) { recipe =>
//          InitializationTx.build(recipe) match {
//              case Left(e) => throw RuntimeException(s"Build failed $e")
//              case Right(tx) =>
//                  val headMultisigScript = HeadMultisigScript(recipe.peers)
//                  val headTokenName =
//                      CIP67.TokenNames(recipe.seedUtxos.map(_._1).head).headTokenName
//
//                  val bytes = tx.tx.toCbor
//                  given OriginalCborByteArray = OriginalCborByteArray(bytes)
//
//                  // println(HexUtil.encodeHexString(bytes))
//
//                  (tx.tx == Cbor
//                      .decode(bytes)
//                      .to[Transaction]
//                      .value) :| "Cbor round-tripping failed"
//                  &&
//                  (tx.tx.body.value.fee.value != 0L) :| "Tx Fee should not be 0"
//                  && (tx.tx.body.value.outputs.size === 2)
//                      :| "Initialization tx should have a treasury output and change output"
//                      &&
//                      (tx.treasuryProduced.asUtxo._2 ==
//                          tx.tx.body.value.outputs.head.value) :|
//                      "treasury output in InitializationTx value not coherent with actual transaction produced"
//                      && (
//                        tx.tx.witnessSet.nativeScripts.head == headMultisigScript.script
//                      ) :| "Head multisig script not as expected"
//                      && (tx.treasuryProduced.headTokenName == headTokenName)
//                      :| "Unexpected head token name in treasury output"
//                      && (tx.treasuryProduced.asUtxo._2.value.assets.assets
//                          .get(headMultisigScript.policyId)
//                          .get(
//                            headTokenName
//                          ) === 1L) :| "treasury output does not contain correct head token"
//                      && {
//                          val actual = tx.treasuryProduced.asUtxo._2.value
//                          val expected = Value(
//                            coin = recipe.initialDeposit,
//                            assets = MultiAsset(assets =
//                                SortedMap(
//                                  (headMultisigScript.policyId, SortedMap((headTokenName, 1L)))
//                                )
//                            )
//                          )
//                          (actual == expected) :| s"Unexpected treasury value. Actual: $actual, expected: $expected"
//                      }
//                      && {
//                          val actual = tx.tx.auxiliaryData.map(_.value)
//                          val expected =
//                              MD.apply(Initialization, headMultisigScript.mkAddress(Mainnet))
//                          actual.contains(expected)
//                              :| s"Unexpected metadata value. Actual: $actual, expected: $expected"
//                      }
//          }
//      }
//    )
//
//}

extension [E,A](e : Either[E,A])
    def fromRight : A = e match {
        case Right(a) => a
}