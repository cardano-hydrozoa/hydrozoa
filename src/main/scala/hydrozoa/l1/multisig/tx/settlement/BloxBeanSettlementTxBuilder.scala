package hydrozoa.l1.multisig.tx.settlement

import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import hydrozoa.infra.{force, mkBuilder, toBloxBeanTransactionOutput}
import hydrozoa.l1.multisig.state.{given_ToData_MultisigTreasuryDatum, mkMultisigTreasuryDatum}
import hydrozoa.l1.multisig.tx.{MultisigTx, SettlementTx}
import hydrozoa.l2.ledger.state.unwrapTxOut
import hydrozoa.node.server.HeadStateReader
import hydrozoa.{AppCtx, TxL1}
import scalus.bloxbean.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData

import java.math.BigInteger
import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class BloxBeanSettlementTxBuilder(
    ctx: AppCtx,
    headStateReader: HeadStateReader
) extends SettlementTxBuilder {

    private val backendService = ctx.backendService
    private val builder = mkBuilder[Tx](ctx)

    override def mkSettlementTxDraft(
        r: SettlementRecipe
    ): Either[String, SettlementTx] =

        val inputsRefs = r.deposits.toSet + headStateReader.currentTreasuryRef

        val inputUtxos: Set[Utxo] =
            inputsRefs.map(r =>
                backendService.getUtxoService.getTxOutput(r.txId.hash, r.outputIx.ix.toInt).force
            )

        val outputsWithdrawn =
            r.utxosWithdrawn.map(w => toBloxBeanTransactionOutput(unwrapTxOut(w._2)))

        val withdrawnAda =
            outputsWithdrawn.foldLeft(BigInteger.ZERO)((s, w) => s.add(w.getValue.getCoin))

        // FIXME: factor out this calculation
        val treasuryValue: List[Amount] =
            inputUtxos.toList.flatMap(u => u.getAmount.asScala)

        // Subtract withdrawn lovelace
        treasuryValue.foreach(a =>
            if a.getUnit.equals("lovelace") then
                a.setQuantity((a.getQuantity.subtract(withdrawnAda)))
        )

        val headAddressBech32 = headStateReader.headBechAddress

        val treasuryDatum = Interop.toPlutusData(
          mkMultisigTreasuryDatum(r.majorVersion, ByteString.empty).toData
        )

        val txPartial = Tx()
            .collectFrom(inputUtxos.asJava)
            .payToContract(
              headAddressBech32.bech32,
              treasuryValue.asJava,
              treasuryDatum
            )
            .from(headAddressBech32.bech32)

        val headNativeScript = headStateReader.headNativeScript
        val nativeScript = NativeScript.deserializeScriptRef(headNativeScript.bytes)

        val settlementTx = builder
            .apply(txPartial)
            // Should be one
            .preBalanceTx((_, t) =>
                t.getWitnessSet.getNativeScripts.add(nativeScript)
                t.getBody.getOutputs.addAll(outputsWithdrawn.asJava)
            )
            // TODO: magic numbers
            .additionalSignersCount(3)
            .build

        Right(MultisigTx(TxL1(settlementTx.serialize())))
}
