package hydrozoa.l1.rulebased.tx.tally

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import hydrozoa.infra.{getUtxoWithDatum, mkBuilder}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.TallyRedeemer.{Continuing, Removed}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.{
    DisputeRedeemer,
    VoteDatum,
    maxVote
}
import hydrozoa.l1.rulebased.onchain.{DisputeResolutionScript, DisputeResolutionValidator}
import hydrozoa.TxL1
import scalus.bloxbean.*
import scalus.builtin.Data.toData
import scalus.prelude.Option.None as SNone

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

class BloxBeanTallyTxBuilder(
    backendService: BackendService
) extends TallyTxBuilder {

    private val builder = mkBuilder[ScriptTx](backendService)

    override def buildTallyTxDraft(r: TallyTxRecipe): Either[String, TxL1] =

        def tallyVotes(continuingDatum: VoteDatum, removedDatum: VoteDatum): VoteDatum = {
            assert(continuingDatum.link == removedDatum.key)
            val higherVote = maxVote(
              continuingDatum.voteStatus,
              removedDatum.voteStatus
            )
            VoteDatum(continuingDatum.key, removedDatum.link, SNone, higherVote)
        }
        for
            continuingInputAndDatum <- getUtxoWithDatum[VoteDatum](r.continuingVote, backendService)
            continuingInput = continuingInputAndDatum._1
            continuingDatum = continuingInputAndDatum._2

            removedInputAndDatum <- getUtxoWithDatum[VoteDatum](r.removedVote, backendService)
            removedInput = removedInputAndDatum._1
            removedDatum = removedInputAndDatum._2

            outputDatum = Interop.toPlutusData(tallyVotes(continuingDatum, removedDatum).toData)

            // Rather wordy way to sum up two values
            continuingOutputMap =
                continuingInput.getAmount.asScala.map(a => (a.getUnit, a.getQuantity)).toMap
            removedOutputMap =
                removedInput.getAmount.asScala.map(a => (a.getUnit, a.getQuantity)).toMap
            outputAmount: List[Amount] = continuingOutputMap
                .foldLeft(removedOutputMap) { case (acc, (k, v)) =>
                    acc.updatedWith(k) {
                        case Some(prev) => Some(prev.add(v))
                        case None       => Some(v)
                    }
                }
                .toList
                .map(e => Amount.builder().unit(e._1).quantity(e._2).build())

            continuingRedeemer = Interop.toPlutusData(DisputeRedeemer.Tally(Continuing).toData)
            removedRedeemer = Interop.toPlutusData(DisputeRedeemer.Tally(Removed).toData)

            txPartial = ScriptTx()
                .collectFrom(continuingInput, continuingRedeemer)
                .collectFrom(removedInput, removedRedeemer)
                .readFrom(r.treasuryUtxoId.transactionId.toHex, r.treasuryUtxoId.index)
                .payToContract(continuingInput.getAddress, outputAmount.asJava, outputDatum)
                .attachSpendingValidator(DisputeResolutionScript.plutusScript)

            nodeAddress = r.nodeAccount.enterpriseAddress()
            txSigner = SignerProviders.signerFrom(r.nodeAccount)

            depositTx: Transaction = builder
                .apply(txPartial)
                // TODO: this should be LEQ than what an unresolved treasury datum contains
                // see MajorBlockConfirmationActor.scala:210
                .validTo(1024)
                .withRequiredSigners(Address(nodeAddress))
                .collateralPayer(nodeAddress)
                .feePayer(nodeAddress)
                .withSigner(txSigner)
                .buildAndSign()
        yield (TxL1(depositTx.serialize))
}
