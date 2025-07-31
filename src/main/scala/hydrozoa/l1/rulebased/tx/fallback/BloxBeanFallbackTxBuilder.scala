package hydrozoa.l1.rulebased.tx.fallback

import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.backend.api.{BackendService, DefaultUtxoSupplier}
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import hydrozoa.infra.{
    HydrozoaBuilderBackendService,
    Piper,
    decodeHex,
    encodeHex,
    mkBuilder,
    numberOfSignatories
}
import hydrozoa.l1.multisig.state.MultisigTreasuryDatum
import hydrozoa.l1.rulebased.onchain.{
    mkDefVoteDatum,
    mkTreasuryDatumUnresolved,
    mkVoteDatum,
    mkVoteTokenName
}
import hydrozoa.{TxId, TxIx, TxL1, UtxoIdL1}
import scalus.prelude.asScalus
import scalus.bloxbean.*
import scalus.builtin.Data.{fromData, toData}
import co.nstant.in.cbor.model.Array as CborArray

import java.math.BigInteger
import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class BloxBeanFallbackTxBuilder(
    backendService: BackendService
) extends FallbackTxBuilder {

    override def buildFallbackTxDraft(r: FallbackTxRecipe): Either[String, TxL1] =

        // Wrapped backend service that known only about the virtual treasury utxo
        val fallbackBuilderBackendService =
            HydrozoaBuilderBackendService(backendService, r.multisigTx)

        // This is not needed, since we pass it indirectly with
        // .withUtxoSelectionStrategy
        val builder = mkBuilder[Tx](fallbackBuilderBackendService)

        val multisigTreasuryUtxo: Utxo = fallbackBuilderBackendService.getUtxoService
            .getUtxos(r.headAddressBech32.bech32, 100, 1, OrderEnum.asc)
            .getValue
            .get(0)

        val multisigDatum = fromData[MultisigTreasuryDatum](
          Interop.toScalusData(
            PlutusData.deserialize(decodeHex(multisigTreasuryUtxo.getInlineDatum).toArray)
          )
        )

        // WARNING [Peter and Ilia]: This is a mess. There's some trickiness with going from
        // scalus's `Native` to BB's `NativeScript`.
        val hnsCborAsByteArray: Array[Byte] = r.headNativeScript.bytes
        val hnsCborArray =
            CborSerializationUtil.deserialize(hnsCborAsByteArray).asInstanceOf[CborArray]
        val headNativeScript =
            NativeScript.deserialize(hnsCborArray)

        // Calculate dispute id
        val voteTokenName = mkVoteTokenName(UtxoIdL1(TxId(multisigTreasuryUtxo.getTxHash), TxIx(0)))

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

        // TODO: magic number
        val voteUtxoAda = 4_000_000

        def mkVoteOutput(datum: PlutusData) = {
            TransactionOutput.builder
                .address(r.disputeAddress.bech32)
                .value(
                  Value.builder
                      // TODO: MinAda
                      .coin(BigInteger.valueOf(voteUtxoAda))
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
              r.treasuryAddress.bech32,
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
            // Remove the change and update coins in the treasury
            .postBalanceTx((_, t) =>
                val outputs = t.getBody.getOutputs
                val change = outputs.removeLast()
                val treasury = outputs.getFirst
                val treasuryValue = treasury.getValue
                treasuryValue.setCoin(
                  change.getValue.getCoin.subtract(BigInteger.valueOf(voteUtxoAda * (peersN + 1)))
                )
            )
            .additionalSignersCount(numberOfSignatories(headNativeScript))
            // FIXME: Fails with "Not enough funds" (at least for fallback against init tx)
            // TODO: When building against a settlement tx, it pulls in the old treasury utxo :-(
            .feePayer(r.headAddressBech32.bech32)
            .withUtxoSelectionStrategy(
              InclusiveUtxoSelectionStrategyImpl(
                DefaultUtxoSupplier(fallbackBuilderBackendService.getUtxoService)
              )
            )
            .build

        fallbackTx.serialize() |> TxL1.apply |> Right.apply
}
