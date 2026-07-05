package hydrozoa.multisig.ledger.eutxol2

import cats.effect.unsafe.implicits.global
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.ledger.withZeroFees
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.prebalancedLovelaceDiffHandler
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.eutxol2.tx.TransientOutputs
import hydrozoa.multisig.ledger.l1.token.CIP67
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, Mint as MintStep, ModifyAuxiliaryData, ReferenceOutput, Send, Spend}
import scalus.cardano.txbuilder.{NativeScriptWitness, PubKeyWitness, TransactionBuilder, TransactionBuilderStep}
import scalus.compiler.{Compile, compile}
import scalus.toUplc
import scalus.uplc.builtin.{ByteString, Data}

/** An always-succeeding minting policy: the V3 script takes the script context and returns unit.
  */
@Compile
object AlwaysValidMintPolicy {
    def validate(scriptContext: Data): Unit = ()
}

/** Shared fixture for transient-token tests: a deterministic multi-peer config, a single-key native
  * minting policy owned by peer 0, an always-succeeding compiled Plutus V3 policy, and a builder
  * for real, fully signed L2 transactions carrying the head-label metadata in either shape (legacy
  * marker list or the current map with optional `transientOutputs` declarations).
  */
object L2TxFixtures {

    /** A deterministic config sampled once from the default generator (fixed seed). */
    val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))

    val ledgerConfig: EutxoL2Ledger.Config =
        multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)

    val peerAddress: ShelleyAddress = multiNodeConfig.addressOf(HeadPeerNumber.zero)

    /** A single-key native minting policy: any tx minting under it needs peer 0's signature. */
    val nativeMintScript: Script.Native =
        Script.Native(Timelock.Signature(multiNodeConfig.addrKeyHashOf(HeadPeerNumber.zero)))

    val nativeMintPolicyId: PolicyId = nativeMintScript.scriptHash

    /** The compiled [[AlwaysValidMintPolicy]] as a Plutus V3 script. */
    val plutusMintScript: Script.PlutusV3 = {
        val program =
            compile((scriptContext: Data) => AlwaysValidMintPolicy.validate(scriptContext))
                .toUplc()
                .plutusV3
        Script.PlutusV3(program.cborByteString)
    }

    val plutusMintPolicyId: PolicyId = plutusMintScript.scriptHash

    /** The demo asset name used by the transient-token tests. */
    val demoAsset: AssetName = AssetName(ByteString.fromString("DEMO"))

    /** A bundle of `quantity` [[demoAsset]] under the native policy. */
    def mkDemoBundle(quantity: Long): MultiAsset =
        MultiAsset.asset(nativeMintPolicyId, demoAsset, quantity)

    /** A builder step minting `quantity` [[demoAsset]] under the native policy (attached). */
    def mkMintDemoStep(quantity: Long): MintStep =
        MintStep(
          nativeMintPolicyId,
          demoAsset,
          quantity,
          NativeScriptWitness.attached(nativeMintScript)
        )

    /** Wall-clock "now" quantized to the config's slots — the transaction creation time handed to
      * the mutator (fixture transactions carry no validity interval, so any instant works).
      */
    val time: QuantizedInstant =
        realTimeQuantizedInstant(ledgerConfig.slotConfig).unsafeRunSync()

    /** A deterministic fake utxo id. */
    def mkInput(seed: Int, index: Int = 0): TransactionInput =
        TransactionInput(Hash(ByteString.fromArray(Array.fill(32)(seed.toByte))), index)

    /** A pub-key utxo at peer 0's address. */
    def mkPeerUtxo(seed: Int, value: Value): (TransactionInput, TransactionOutput) =
        mkInput(seed) -> Babbage(peerAddress, value)

    /** Build the head-label auxiliary data: per-output L1/L2 markers plus optional transient
      * declarations, in the requested metadatum shape.
      */
    def mkHeadMetadata(
        markers: List[Int],
        transientOutputs: Map[Int, MultiAsset] = Map.empty,
        legacyShape: Boolean = false
    ): AuxiliaryData = {
        val markerList =
            Metadatum.List(markers.map(marker => Metadatum.Int(marker.toLong)).toIndexedSeq)
        val headMetadatum =
            if legacyShape then markerList
            else {
                val outputsField: Map[Metadatum, Metadatum] =
                    Map(Metadatum.Text("outputs") -> markerList)
                val transientField: Map[Metadatum, Metadatum] =
                    if transientOutputs.isEmpty then Map.empty
                    else
                        Map(
                          Metadatum.Text("transientOutputs") ->
                              TransientOutputs.encodeMetadatum(transientOutputs)
                        )
                Metadatum.Map(outputsField ++ transientField)
            }
        AuxiliaryData.Metadata(Map(Word64(CIP67.Tags.head) -> headMetadatum))
    }

    /** Build and multisign an L2 transaction: spend `spends` as pub-key inputs, produce `sends`
      * outputs with their L1(1)/L2(2) markers, apply `mints` steps, reference `references` utxos,
      * and declare `transientOutputs` in the metadata. Spend values must be the combined (main +
      * transient) view — the same values the ledger rules resolve.
      */
    def buildSignedL2Tx(
        spends: List[(TransactionInput, TransactionOutput)],
        sends: List[(TransactionOutput, Int)],
        mints: List[MintStep] = Nil,
        references: List[(TransactionInput, TransactionOutput)] = Nil,
        transientOutputs: Map[Int, MultiAsset] = Map.empty,
        legacyMetadataShape: Boolean = false
    ): Transaction = {
        val auxiliaryData =
            mkHeadMetadata(sends.map(_._2), transientOutputs, legacyMetadataShape)
        val steps: List[TransactionBuilderStep] =
            spends.map((input, output) => Spend(Utxo(input, output), PubKeyWitness))
                ++ references.map((input, output) => ReferenceOutput(Utxo(input, output)))
                ++ mints
                ++ sends.map((output, _) => Send(output))
                ++ List(Fee(Coin.zero), ModifyAuxiliaryData(_ => Some(auxiliaryData)))
        val unsigned = TransactionBuilder
            .build(multiNodeConfig.headConfig.cardanoNetwork.cardanoInfo.network, steps)
            .flatMap(
              _.finalizeContext(
                protocolParams = multiNodeConfig.headConfig.cardanoProtocolParams.withZeroFees,
                diffHandler = prebalancedLovelaceDiffHandler,
                evaluator = multiNodeConfig.headConfig.plutusScriptEvaluatorForTxBuild,
                validators = Seq.empty
              )
            )
            .fold(
              error => throw RuntimeException(s"Can't build fixture L2 tx: $error", error.reason),
              context => context.transaction
            )
        multiNodeConfig.multisignTx(unsigned)
    }
}
