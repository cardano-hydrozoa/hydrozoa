package hydrozoa.l1.multisig.tx.fallback

import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import hydrozoa.infra.{
    Piper,
    encodeHex,
    mkBuilder,
    numberOfSignatories,
    onlyOutputToAddress,
    txOutputToUtxo
}
import hydrozoa.l1.multisig.state.MultisigTreasuryDatum
import hydrozoa.l1.rulebased.onchain.{
    mkDefVoteDatum,
    mkTreasuryDatumUnresolved,
    mkVoteDatum,
    mkVoteTokenName
}
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import hydrozoa.{TxId, TxL1, UtxoIdL1}
import scalus.bloxbean.*
import scalus.builtin.Data.{fromData, toData}
import scalus.prelude.List.asScalus

import java.math.BigInteger
import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class BloxBeanFallbackTxBuilder(
    backendService: BackendService
) extends FallbackTxBuilder {

    private val builder = mkBuilder[Tx](backendService)

    override def buildFallbackTxDraft(r: FallbackTxRecipe): Either[String, TxL1] =

        // Parse Tx and calculate its hash - can be passed along with it since it's already known.
        val txBytes = r.multisigTx.bytes
        val tb = Transaction.deserialize(txBytes)
        val txHash = TransactionUtil.getTxHash(txBytes)

        // Find the treasury output
        // TODO: shall we fix the order of outputs? So we can just always take the first output?
        val Right(treasuryOutputIx, _, _, multisigTreasuryDatum) =
            onlyOutputToAddress(r.multisigTx, r.headAddressBech32)
        val treasuryOutput = tb.getBody.getOutputs.get(treasuryOutputIx.ix)

        // TODO: update txOutputToUtxo to support tokens
        // This is required only due to BB's peculiarities
        val multisigTreasuryUtxo: Utxo = txOutputToUtxo(txHash, treasuryOutputIx.ix, treasuryOutput)

        val multisigDatum = fromData[MultisigTreasuryDatum](multisigTreasuryDatum)

        val headNativeScript =
            NativeScript.deserializeScriptRef(r.headNativeScript.bytes)

        // Calculate dispute id
        val voteTokenName = mkVoteTokenName(UtxoIdL1(TxId(txHash), treasuryOutputIx))

        // Treasury datum
        val treasuryDatum = Interop.toPlutusData(
          mkTreasuryDatumUnresolved(
            headMp = r.headMintingPolicy,
            disputeId = voteTokenName,
            peers = r.peers.asScalus,
            // FIXME: we are going to remove it, votingDuration should go to the timer utxo
            deadlineVoting = r.votingDuration,
            versionMajor = multisigDatum.versionMajor,
            params = multisigDatum.params
          ).toData
        )

        val bbTokenName = "0x" + voteTokenName.tokenNameHex

        // Vote tokens and vote utxos
        val voteTokens = Asset.builder
            .name(bbTokenName)
            .value(BigInteger.valueOf(r.peers.length + 1))
            .build

        val peersN = r.peers.length

        def mkVoteOutput(datum: PlutusData) = {
            TransactionOutput.builder
                .address(r.disputeScript.bech32)
                .value(
                  Value.builder
                      // TODO: MinAda
                      .coin(BigInteger.valueOf(2_000_000))
                      .multiAssets(
                        List(
                          MultiAsset
                              .builder()
                              .policyId(encodeHex(r.headMintingPolicy.bytes))
                              .assets(
                                List(
                                  Asset.builder
                                      .name(bbTokenName)
                                      .value(BigInteger.ONE)
                                      .build()
                                ).asJava
                              )
                              .build()
                        ).asJava
                      )
                      .build
                )
                .inlineDatum(datum)
                .build
        }

        val defVoteUtxo =
            mkVoteOutput(Interop.toPlutusData(mkDefVoteDatum(peersN, ()).toData))

        val voteUtxos: List[TransactionOutput] = r.peers.zipWithIndex.map((peer, key) =>
            val datum = Interop.toPlutusData(mkVoteDatum(key + 1, peersN, peer).toData)
            mkVoteOutput(datum)
        )

        // FIXME: set validity range

        val txPartial = Tx()
            .collectFrom(List(multisigTreasuryUtxo).asJava)
            .payToContract(
              r.treasuryScript.bech32,
              multisigTreasuryUtxo.getAmount,
              treasuryDatum
            )
            .from(r.headAddressBech32.bech32)
            .mintAssets(headNativeScript, voteTokens)

        val fallbackTx = builder
            .apply(txPartial)
            // .preBalanceTx should be called only once
            .preBalanceTx((_, t) =>
                t.getWitnessSet.getNativeScripts.add(headNativeScript)
                val outputs = t.getBody.getOutputs
                // NB: utxo added automagically by .mintAssets
                // fails if treasuryScript == headAddress (should never be the case)
                outputs.remove(1)
                // proper set of utxos (def + voting)
                outputs.add(defVoteUtxo)
                outputs.addAll(voteUtxos.asJava)
            )
            .additionalSignersCount(numberOfSignatories(headNativeScript))
            // FIXME: Fails with "Not enough funds" (at least for fallback against init tx)
            // TODO: When building against a settlement tx, it pulls in the old treasury utxo :-(
            // .feePayer(r.headAddressBech32.bech32)
            .feePayer(
              "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
            )
            .build

        fallbackTx.serialize() |> TxL1.apply |> Right.apply
}
