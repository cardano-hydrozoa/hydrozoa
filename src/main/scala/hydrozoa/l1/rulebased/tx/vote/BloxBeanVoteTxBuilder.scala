package hydrozoa.l1.rulebased.tx.vote

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount.lovelace
import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.TxSigner
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.infra.{mkBuilder, toEither}
import hydrozoa.l1.rulebased.onchain.DisputeResolutionScript
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.VoteStatus.Vote
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.{DisputeRedeemer, MinorBlockL1Effect, VoteDatum, VoteDetails}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.{account, mkWallet}
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import hydrozoa.{TxIx, TxL1}
import scalus.bloxbean.*
import scalus.builtin.ByteString
import scalus.builtin.Data.{fromData, toData}
import scalus.prelude.List.asScalus

import java.math.BigInteger
import scala.jdk.CollectionConverters.*

class BloxBeanVoteTxBuilder(
    backendService: BackendService
) extends VoteTxBuilder {

    private val builder = mkBuilder[ScriptTx](backendService)

    override def buildVoteTxDraft(r: VoteTxRecipe): Either[String, TxL1] =

        val Right(voteUtxo) = backendService.getUtxoService
            .getTxOutput(r.voteUtxoId.txId.hash, r.voteUtxoId.outputIx.ix)
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

        val multisig = r.proof.map(s => ByteString.fromArray(s.signature.toArray)).toList.asScalus
        val redeemer = Interop.toPlutusData(
          DisputeRedeemer.Vote(MinorBlockL1Effect(r.blockHeader, multisig)).toData
        )

        val txPartial = ScriptTx()
            .collectFrom(voteUtxo, redeemer)
            .payToContract(voteUtxo.getAddress, voteUtxo.getAmount, outVoteDatum)
            .attachSpendingValidator(DisputeResolutionScript.plutusScript);

//            .from(depositorAddress)

        val nodeAddress = r.nodeAddress.bech32

        // FIXME: use real account, not it's Carol since DisputeSuite uses it
        val txSigner: TxSigner = SignerProviders.signerFrom(account(TestPeer.Carol))

        val depositTx: Transaction = builder
            .apply(txPartial)
            .collateralPayer(nodeAddress)
            .feePayer(nodeAddress)
            .withSigner(txSigner)
            .build()

        Right(TxL1(depositTx.serialize))
}
