package hydrozoa.l1.multisig.tx.settlement

import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.quicktx.Tx
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import hydrozoa.TxL1
import hydrozoa.infra.{force, mkBuilder, numberOfSignatories, toBloxBeanTransactionOutput}
import hydrozoa.l1.multisig.state.{given_ToData_MultisigTreasuryDatum, mkMultisigTreasuryDatum}
import hydrozoa.l1.multisig.tx.{MultisigTx, SettlementTx}
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import scalus.bloxbean.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData

import java.math.BigInteger
import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class BloxBeanSettlementTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader
) extends SettlementTxBuilder {

    private val builder = mkBuilder[Tx](backendService)

    override def mkSettlementTxDraft(
        r: SettlementRecipe
    ): Either[String, SettlementTx] =

        val inputsIds = r.deposits.toSet + reader.multisigRegime(_.currentTreasuryRef)

        val utxoInput: Set[Utxo] =
            inputsIds.map(r =>
                backendService.getUtxoService.getTxOutput(r.txId.hash, r.outputIx.ix.toInt).force
            )

        val outputsToWithdraw =
            r.utxosWithdrawn.map(w => toBloxBeanTransactionOutput(w._2))

        val withdrawnCoins =
            outputsToWithdraw.foldLeft(BigInteger.ZERO)((s, w) => s.add(w.getValue.getCoin))

        // FIXME: factor out this calculation
        val treasuryValue: List[Amount] =
            utxoInput.toList.flatMap(u => u.getAmount.asScala)

        // Subtract withdrawn lovelace
        treasuryValue.foreach(a =>
            if a.getUnit.equals("lovelace") then
                a.setQuantity((a.getQuantity.subtract(withdrawnCoins)))
        )

        val headAddressBech32 = reader.multisigRegime(_.headBechAddress).bech32

        val treasuryDatum = Interop.toPlutusData(
          mkMultisigTreasuryDatum(r.majorVersion, ByteString.empty).toData
        )

        val txPartial = Tx()
            .collectFrom(utxoInput.asJava)
            .payToContract(
              headAddressBech32,
              treasuryValue.asJava,
              treasuryDatum
            )
            .from(headAddressBech32)

        val headNativeScript =
            NativeScript.deserializeScriptRef(reader.multisigRegime(_.headNativeScript).bytes)

        val settlementTx = builder
            .apply(txPartial)
            // .preBalanceTx should be called only once
            .preBalanceTx((_, t) =>
                t.getWitnessSet.getNativeScripts.add(headNativeScript)
                t.getBody.getOutputs.addAll(outputsToWithdraw.asJava)
            )
            .additionalSignersCount(numberOfSignatories(headNativeScript))
            .build

        Right(MultisigTx(TxL1(settlementTx.serialize())))
}
