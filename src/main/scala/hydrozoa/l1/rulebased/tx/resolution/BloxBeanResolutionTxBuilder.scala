package hydrozoa.l1.rulebased.tx.resolution

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import hydrozoa.infra.{getUtxoWithDatum, mkBuilder}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.VoteStatus.{NoVote, Vote}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.{DisputeRedeemer, VoteDatum}
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.{
    ResolvedDatum,
    TreasuryDatum,
    TreasuryRedeemer
}
import hydrozoa.{TxL1, UtxoIdL1}
import scalus.bloxbean.*
import scalus.builtin.Data.toData

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

class BloxBeanResolutionTxBuilder(
    backendService: BackendService,
    mbTreasuryScriptRefUtxoId: Option[UtxoIdL1],
    mbDisputeScriptRefUtxoId: Option[UtxoIdL1]
) extends ResolutionTxBuilder {

    private val builder = mkBuilder[ScriptTx](backendService, false)

    override def buildResolutionTx(r: ResolutionTxRecipe): Either[String, TxL1] =
        (mbTreasuryScriptRefUtxoId, mbDisputeScriptRefUtxoId) match
            case (Some(treasuryScriptRefUtxoId), Some(disputeScriptRefUtxoId)) =>
                for
                    voteInputAndDatum <- getUtxoWithDatum[VoteDatum](r.talliedVote, backendService)
                    voteInput = voteInputAndDatum._1
                    vote = voteInputAndDatum._2.voteStatus match {
                        case Vote(vote) => vote
                        case NoVote     => throw RuntimeException("Tailled vote is NoVote")
                    }

                    treasuryInputAndDatum <- getUtxoWithDatum[TreasuryDatum](
                      r.treasuryUtxoId,
                      backendService
                    )
                    treasuryInput = treasuryInputAndDatum._1
                    treasuryDatum <- treasuryInputAndDatum._2 match {
                        case TreasuryDatum.Unresolved(td) => Right(td)
                        case _ => Left("buildResolutionTx: Treasury Datum must be unresolved")
                    }

                    outputDatum = Interop.toPlutusData(
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
                    voteInputMap =
                        voteInput.getAmount.asScala.map(a => (a.getUnit, a.getQuantity)).toMap
                    treasuryMap =
                        treasuryInput.getAmount.asScala.map(a => (a.getUnit, a.getQuantity)).toMap
                    outputAmount: List[Amount] = voteInputMap
                        .foldLeft(treasuryMap) { case (acc, (k, v)) =>
                            acc.updatedWith(k) {
                                case Some(prev) => Some(prev.add(v))
                                case None       => Some(v)
                            }
                        }
                        .toList
                        .map(e => Amount.builder().unit(e._1).quantity(e._2).build())

                    voteRedeemer = Interop.toPlutusData(DisputeRedeemer.Resolve.toData)
                    treasuryRedeemer = Interop.toPlutusData(TreasuryRedeemer.Resolve.toData)
                    //
                    txPartial = ScriptTx()
                        .collectFrom(voteInput, voteRedeemer)
                        .collectFrom(treasuryInput, treasuryRedeemer)
                        .payToContract(treasuryInput.getAddress, outputAmount.asJava, outputDatum)
                        .readFrom(
                          treasuryScriptRefUtxoId.transactionId.toHex,
                          treasuryScriptRefUtxoId.index
                        )
                        .readFrom(
                          disputeScriptRefUtxoId.transactionId.toHex,
                          disputeScriptRefUtxoId.index
                        )

                    nodeAddress = r.nodeAccount.enterpriseAddress()
                    txSigner = SignerProviders.signerFrom(r.nodeAccount)

                    resolutionTx: Transaction = builder
                        .apply(txPartial)
                        // TODO: this should be LEQ than what an unresolved treasury datum contains
                        // see MajorBlockConfirmationActor.scala:210
                        .validTo(1024)
                        .withRequiredSigners(Address(nodeAddress))
                        .collateralPayer(nodeAddress)
                        .feePayer(nodeAddress)
                        .withSigner(txSigner)
                        .buildAndSign()
                yield (TxL1(resolutionTx.serialize))
            case _ => Left("Ref scripts are not set")
}
