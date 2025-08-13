package hydrozoa.l1.rulebased.tx.vote

import scala.language.implicitConversions
import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.TxL1
import hydrozoa.infra.{mkBuilder, toEither}
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
import scalus.builtin.Data.{fromData, toData}
import scalus.prelude.List.asScala
import scalus.prelude.asScalus

import scala.jdk.CollectionConverters.*

class BloxBeanVoteTxBuilder(
    backendService: BackendService
) extends VoteTxBuilder {

    private val builder = mkBuilder[ScriptTx](backendService)

    override def buildVoteTxDraft(r: VoteTxRecipe): Either[String, TxL1] =

        val Right(voteUtxo) = backendService.getUtxoService
            .getTxOutput(r.voteUtxoId.transactionId.toHex, r.voteUtxoId.index)
            .toEither

        val inVoteDatum = fromData[VoteDatum](
          Interop.toScalusData(
            PlutusData
                .deserialize(HexUtil.decodeHexString(voteUtxo.getInlineDatum))
          )
        )

        val outVoteDatum = Interop.toPlutusData(
          inVoteDatum
              .copy(voteStatus =
                  Vote(VoteDetails(r.blockHeader.utxosActive, r.blockHeader.versionMinor))
              )
              .toData
        )

        val multisig = r.proof.map(s => ByteString.fromArray(s)).toList.asScalus
        val redeemer = Interop.toPlutusData(
          DisputeRedeemer.Vote(MinorBlockL1Effect(r.blockHeader, multisig)).toData
        )

        val outputAmount: List[Amount] = voteUtxo.getAmount.asScala.toList

        // TODO: we want to pay fees from the vote utxo
        // FIXME: Subtract 1 ada to allow fees be pays from the vote utxo
//        outputAmount.foreach(a =>
//            if a.getUnit.equals("lovelace") then
//                a.setQuantity((a.getQuantity.subtract(BigInteger("1_000_000"))))
//        )

        val txPartial = ScriptTx()
            .collectFrom(voteUtxo, redeemer)
            .readFrom(r.treasuryUtxoId.transactionId.toHex, r.treasuryUtxoId.index)
            .payToContract(voteUtxo.getAddress, outputAmount.asJava, outVoteDatum)
            .attachSpendingValidator(DisputeResolutionScript.plutusScript);

        val nodeAddress = r.nodeAddress.toBech32.get

        val txSigner = SignerProviders.signerFrom(r.nodeAccount)

        val voteTx: Transaction = builder
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

        Right(TxL1(voteTx.serialize))
}
