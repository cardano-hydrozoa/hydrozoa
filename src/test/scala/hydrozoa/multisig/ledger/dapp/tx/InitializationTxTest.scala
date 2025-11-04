package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import cats.syntax.all.*
import hydrozoa.maxNonPlutusTxFee
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.Initialization
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import io.bullet.borer.Cbor
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
import test.Generators.Hydrozoa.*
import test.TestPeer.Alice

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

        // We make sure that the seed utxo has at least enough for the treasury and multisig witness UTxO, plus
        // a max non-plutus fee
        seedUtxo <- genAdaOnlyPubKeyUtxo(peers.head, genCoinWithMinimum = Some(minInitTreasuryAda
          + Coin(maxNonPlutusTxFee(testProtocolParams).value * 2)))
        otherSpentUtxos <- Gen
          .listOf(genAdaOnlyPubKeyUtxo(peers.head, genCoinWithMinimum = Some(Coin(0))))

        spentUtxos = NonEmptyList(seedUtxo, otherSpentUtxos)

        // Initial deposit must be at least enough for the minAda of the treasury, and no more than the
        // sum of the seed utxos, while leaving enough left for the estimated fee and the minAda of the change
        // output
        initialDeposit <- Gen.choose(
          minInitTreasuryAda.value,
          sumUtxoValues(spentUtxos.toList).coin.value
            - maxNonPlutusTxFee(testTxBuilderEnvironment.protocolParams).value
            - minPubkeyAda().value
        ).map(Coin(_))

    }
        yield InitializationTx.Recipe(
          seedUtxo = spentUtxos.head._1,
          spentUtxos = spentUtxos.map(utxo => TransactionUnspentOutput(utxo._1, utxo._2)),
          initialDeposit = initialDeposit,
          peers = peers.map(_.wallet.exportVerificationKeyBytes),
          env = testTxBuilderEnvironment,
          validators = testValidators,
           changeAddress =
                ShelleyAddress(network = testTxBuilderEnvironment.network,
                Key(AddrKeyHash.fromByteString(ByteString.fill(28, 1.toByte))),
                delegation = Null)
    ).get


object InitializationTxTest extends Properties("InitializationTx") {
    // TODO: We only test building. We need to sign the tx and actually run it through the STS
    property("InitializationTx Happy Path") = Prop.forAll(genInitTxRecipe) {
        recipe =>
            InitializationTx.build(recipe).isRight :| "successful build" && {
                val iTx = InitializationTx.build(recipe).get
                val headNativeScript = HeadMultisigScript(recipe.peers)
                val headTokenName = CIP67.TokenNames(recipe.seedUtxo).headTokenName
                val mulitsigRegimeTokenName = CIP67.TokenNames(recipe.seedUtxo).multisigRegimeTokenName
                val txOutputs : Seq[TransactionOutput] = iTx.tx.body.value.outputs.map(_.value)

                recipe.spentUtxos.toList.map(utxo => iTx.tx.body.value.inputs.toSeq.contains(utxo.input)).reduce(_ && _)
                    :| "Configured inputs are spent"
                    && iTx.tx.body.value.inputs.toSeq.contains(recipe.seedUtxo) :| "Seed input is spent"
                    && iTx.tx.body.value.mint.contains(Mint(MultiAsset(SortedMap(
                  headNativeScript.policyId -> SortedMap(headTokenName -> 1L, mulitsigRegimeTokenName -> 1L)))))
                    :| "Only Treasury token and mulReg tokens minted"
                    && {

                    val expectedTreasuryIndex = iTx.treasuryProduced.asUtxo.input.index
                    // Treasury output checks
                    (txOutputs(expectedTreasuryIndex) ==
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
                  (txOutputs(iTx.multisigRegimeUtxo.input.index) ==
                    iTx.multisigRegimeUtxo.output)
                    :| "initialization tx contains MR output at correct index"
                    && (iTx.tx.id == iTx.multisigRegimeUtxo.input.transactionId)
                    :| "initialization tx id coherent with produced MR output"
                    && (iTx.multisigRegimeUtxo.output.value.assets ==
                    MultiAsset(SortedMap(headNativeScript.policyId -> SortedMap(mulitsigRegimeTokenName -> 1L))))
                    :| "MR utxo only contains MR token in multiassets"
                    && (iTx.multisigRegimeUtxo.output.value.coin >= maxNonPlutusTxFee(recipe.env.protocolParams))
                    :| "MR utxo contains at least enough coin for fallback deposit"
                } && {
                  val actual = iTx.tx.auxiliaryData.map(_.value)
                  val expected =
                    MD.apply(Initialization, iTx.treasuryProduced.address)
                  (actual.contains(expected) :| s"Unexpected metadata value. Actual: $actual, expected: $expected")
                } && {
                  val bytes = iTx.tx.toCbor

                  given OriginalCborByteArray = OriginalCborByteArray(bytes)

                  (iTx.tx == Cbor
                    .decode(bytes)
                    .to[Transaction]
                    .value) :| "Cbor round-tripping failed"
                }
            }
    }
}