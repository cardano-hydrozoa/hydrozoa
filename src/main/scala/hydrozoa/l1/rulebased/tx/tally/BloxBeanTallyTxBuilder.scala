package hydrozoa.l1.rulebased.tx.tally

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.infra.{mkBuilder, toEither}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.TallyRedeemer.{Continuing, Removed}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.{
    DisputeRedeemer,
    VoteDatum,
    maxVote
}
import hydrozoa.l1.rulebased.onchain.{DisputeResolutionScript, DisputeResolutionValidator}
import hydrozoa.{TxL1, UtxoIdL1}
import scalus.bloxbean.*
import scalus.builtin.Data.{fromData, toData}
import scalus.prelude.Option.None as SNone

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

class BloxBeanTallyTxBuilder(
    backendService: BackendService
) extends TallyTxBuilder {

    private val builder = mkBuilder[ScriptTx](backendService)

    override def buildTallyTxDraft(r: TallyTxRecipe): Either[String, TxL1] =

        def getVoteUtxoWithDatum(utxoId: UtxoIdL1): (Utxo, VoteDatum) =
            val Right(utxo) = backendService.getUtxoService
                .getTxOutput(utxoId.transactionId.toHex, utxoId.index)
                .toEither

            val datum = fromData[VoteDatum](
              Interop.toScalusData(
                PlutusData
                    .deserialize(HexUtil.decodeHexString(utxo.getInlineDatum))
              )
            )
            (utxo, datum)

        def tallyVotes(continuingDatum: VoteDatum, removedDatum: VoteDatum): VoteDatum = {
            assert(continuingDatum.link == removedDatum.key)
            val higherVote = maxVote(
              continuingDatum.voteStatus,
              removedDatum.voteStatus
            )
            VoteDatum(continuingDatum.key, removedDatum.link, SNone, higherVote)
        }

        val (continuingInput, continuingDatum) = getVoteUtxoWithDatum(r.continuingVote)
        val (removedInput, removedDatum) = getVoteUtxoWithDatum(r.removedVote)

        val outputDatum = Interop.toPlutusData(tallyVotes(continuingDatum, removedDatum).toData)

        // Rather wordy way to sum up two values
        val continuingOutputMap =
            continuingInput.getAmount.asScala.map(a => (a.getUnit, a.getQuantity)).toMap
        val removedOutputMap =
            removedInput.getAmount.asScala.map(a => (a.getUnit, a.getQuantity)).toMap
        val outputAmount: List[Amount] = continuingOutputMap
            .foldLeft(removedOutputMap) { case (acc, (k, v)) =>
                acc.updatedWith(k) {
                    case Some(prev) => Some(prev.add(v))
                    case None       => Some(v)
                }
            }
            .toList
            .map(e => Amount.builder().unit(e._1).quantity(e._2).build())

        val continuingRedeemer = Interop.toPlutusData(DisputeRedeemer.Tally(Continuing).toData)
        val removedRedeemer = Interop.toPlutusData(DisputeRedeemer.Tally(Removed).toData)

        val txPartial = ScriptTx()
            .collectFrom(continuingInput, continuingRedeemer)
            .collectFrom(removedInput, removedRedeemer)
            .readFrom(r.treasuryUtxoId.transactionId.toHex, r.treasuryUtxoId.index)
            .payToContract(continuingInput.getAddress, outputAmount.asJava, outputDatum)
            .attachSpendingValidator(DisputeResolutionScript.plutusScript)

        val nodeAddress = r.nodeAccount.enterpriseAddress()
        val txSigner = SignerProviders.signerFrom(r.nodeAccount)

        val depositTx: Transaction = builder
            .apply(txPartial)
            // TODO: this should be LEQ than what an unresolved treasury datum contains
            // see MajorBlockConfirmationActor.scala:210
            .validTo(1024)
            .withRequiredSigners(Address(nodeAddress))
            .collateralPayer(nodeAddress)
            .feePayer(nodeAddress)
            .withSigner(txSigner)
            .buildAndSign()

        Right(TxL1(depositTx.serialize))
}
