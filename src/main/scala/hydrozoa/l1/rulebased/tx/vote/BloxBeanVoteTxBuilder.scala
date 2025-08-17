package hydrozoa.l1.rulebased.tx.vote

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import hydrozoa.TxL1
import hydrozoa.infra.{getUtxoWithDatum, mkBuilder}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionScript
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.VoteStatus.Vote
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.{
    DisputeRedeemer,
    MinorBlockL1Effect,
    VoteDatum,
    VoteDetails
}
import scalus.bloxbean.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.prelude.asScalus

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

class BloxBeanVoteTxBuilder(
    backendService: BackendService
) extends VoteTxBuilder {

    private val builder = mkBuilder[ScriptTx](backendService)

    override def buildVoteTxDraft(r: VoteTxRecipe): Either[String, TxL1] =
        for
            voteInputAndDatum <- getUtxoWithDatum[VoteDatum](r.voteUtxoId, backendService)
            voteUtxo = voteInputAndDatum._1
            inVoteDatum = voteInputAndDatum._2

            outVoteDatum = Interop.toPlutusData(
              inVoteDatum
                  .copy(voteStatus =
                      Vote(VoteDetails(r.blockHeader.utxosActive, r.blockHeader.versionMinor))
                  )
                  .toData
            )

            multisig = r.proof.effect.map(s => ByteString.fromArray(s)).toList.asScalus
            redeemer = Interop.toPlutusData(
              DisputeRedeemer.Vote(MinorBlockL1Effect(r.blockHeader, multisig)).toData
            )

            outputAmount: List[Amount] = voteUtxo.getAmount.asScala.toList

            // TODO: we want to pay fees from the vote utxo
            // FIXME: Subtract 1 ada to allow fees be pays from the vote utxo
//        outputAmount.foreach(a =>
//            if a.getUnit.equals("lovelace") then
//                a.setQuantity((a.getQuantity.subtract(BigInteger("1_000_000"))))
//        )

            txPartial = ScriptTx()
                .collectFrom(voteUtxo, redeemer)
                .readFrom(r.treasuryUtxoId.transactionId.toHex, r.treasuryUtxoId.index)
                .payToContract(voteUtxo.getAddress, outputAmount.asJava, outVoteDatum)
                .attachSpendingValidator(DisputeResolutionScript.plutusScript);

            nodeAddress = r.nodeAddress.toBech32.get

            txSigner = SignerProviders.signerFrom(r.nodeAccount)

            voteTx: Transaction = builder
                .apply(txPartial)
                // TODO: this should be LEQ than what an unresolved treasury datum contains
                // see MajorBlockConfirmationActor.scala:210
                .validTo(1024)
                .withRequiredSigners(Address(nodeAddress))
                .collateralPayer(nodeAddress)
                // TODO: we want to pay fees from the vote utxo
                // .feePayer(voteUtxo.getAddress)
                .feePayer(nodeAddress)
                .withSigner(txSigner)
                .buildAndSign()
        yield (TxL1(voteTx.serialize))
}
