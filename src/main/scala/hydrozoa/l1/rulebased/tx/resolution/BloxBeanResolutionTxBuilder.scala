package hydrozoa.l1.rulebased.tx.resolution

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.infra.{mkBuilder, toEither}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.VoteStatus.{NoVote, Vote}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.{DisputeRedeemer, VoteDatum}
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.{ResolvedDatum, TreasuryDatum, TreasuryRedeemer}
import hydrozoa.{TxL1, UtxoIdL1}
import scalus.bloxbean.*
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.FromData

import scala.jdk.CollectionConverters.*

class BloxBeanResolutionTxBuilder(
    backendService: BackendService,
    mbTreasuryScriptRefUtxoId: Option[UtxoIdL1],
    mbDisputeScriptRefUtxoId: Option[UtxoIdL1]
) extends ResolutionTxBuilder {

    private val builder = mkBuilder[ScriptTx](backendService, false)

    override def buildResolutionTx(r: ResolutionTxRecipe): Either[String, TxL1] =
        (mbTreasuryScriptRefUtxoId, mbDisputeScriptRefUtxoId) match
            case (Some(treasuryScriptRefUtxoId), Some(disputeScriptRefUtxoId)) =>
                def getUtxoWithDatum[T](using FromData[T])(utxoId: UtxoIdL1): (Utxo, T) =
                    val Right(utxo) = backendService.getUtxoService
                        .getTxOutput(utxoId.txId.hash, utxoId.outputIx.ix)
                        .toEither

                    val datum = fromData[T](
                      Interop.toScalusData(
                        PlutusData
                            .deserialize(HexUtil.decodeHexString(utxo.getInlineDatum))
                      )
                    )
                    (utxo, datum)

                val (voteInput, voteDatum: VoteDatum) = getUtxoWithDatum[VoteDatum](r.talliedVote)
                val vote = voteDatum.voteStatus match {
                    case Vote(vote) => vote
                    case NoVote     => throw RuntimeException("Tailled vote is NoVote")
                }

                val (treasuryInput, TreasuryDatum.Unresolved(treasuryDatum)) =
                    getUtxoWithDatum[TreasuryDatum](r.treasuryUtxoId)

                val outputDatum = Interop.toPlutusData(
                  TreasuryDatum
                      .Resolved(
                        ResolvedDatum(
                          treasuryDatum.headMp,
                          vote.utxosActive,
                          (treasuryDatum.versionMajor, vote.versionMinor),
                          treasuryDatum.params
                        )
                      )
                      .toData
                )

                // Rather wordy way to sum up two values
                val voteInputMap =
                    voteInput.getAmount.asScala.map(a => (a.getUnit, a.getQuantity)).toMap
                val treasuryMap =
                    treasuryInput.getAmount.asScala.map(a => (a.getUnit, a.getQuantity)).toMap
                val outputAmount: List[Amount] = voteInputMap
                    .foldLeft(treasuryMap) { case (acc, (k, v)) =>
                        acc.updatedWith(k) {
                            case Some(prev) => Some(prev.add(v))
                            case None       => Some(v)
                        }
                    }
                    .toList
                    .map(e => Amount.builder().unit(e._1).quantity(e._2).build())

                val voteRedeemer = Interop.toPlutusData(DisputeRedeemer.Resolve.toData)
                val treasuryRedeemer = Interop.toPlutusData(TreasuryRedeemer.Resolve.toData)
                //
                val txPartial = ScriptTx()
                    .collectFrom(voteInput, voteRedeemer)
                    .collectFrom(treasuryInput, treasuryRedeemer)
                    .payToContract(treasuryInput.getAddress, outputAmount.asJava, outputDatum)
                    .readFrom(
                      treasuryScriptRefUtxoId.txId.hash,
                      treasuryScriptRefUtxoId.outputIx.ix
                    )
                    .readFrom(disputeScriptRefUtxoId.txId.hash, disputeScriptRefUtxoId.outputIx.ix)

                val nodeAddress = r.nodeAccount.enterpriseAddress()
                val txSigner = SignerProviders.signerFrom(r.nodeAccount)

                val resolutionTx: Transaction = builder
                    .apply(txPartial)
                    // TODO: this should be LEQ than what an unresolved treasury datum contains
                    // see MajorBlockConfirmationActor.scala:210
                    .validTo(1024)
                    .withRequiredSigners(Address(nodeAddress))
                    .collateralPayer(nodeAddress)
                    .feePayer(nodeAddress)
                    .withSigner(txSigner)
                    .buildAndSign()

                Right(TxL1(resolutionTx.serialize))
            case _ => Left("Ref scripts are not set")
}
