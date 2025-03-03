package hydrozoa.l1.multisig.tx.settlement

import com.bloxbean.cardano.client.api.model.Amount.lovelace
import com.bloxbean.cardano.client.api.model.{Amount, ProtocolParams, Utxo}
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import com.bloxbean.cardano.client.transaction.util.TransactionUtil.getTxHash
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.infra.{addressToBloxbean, force, toEither, txOutputToUtxo}
import hydrozoa.l1.multisig.state.{given_ToData_MultisigTreasuryDatum, mkInitMultisigTreasuryDatum}
import hydrozoa.l1.multisig.tx.MultisigTxs.{PostDatedRefundTx, SettlementTx}
import hydrozoa.node.server.HeadStateReader
import hydrozoa.{AppCtx, L1Tx}
import scalus.bloxbean.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.prelude.Maybe.{Just, Nothing}

import java.math.BigInteger
import scala.jdk.CollectionConverters.*
import scala.language.postfixOps

class BloxBeanSettlementTxBuilder(
    ctx: AppCtx,
    headStateReader: HeadStateReader
) extends SettlementTxBuilder {

    private val backendService = ctx.backendService

    lazy val protocolParams: ProtocolParams = {
        val result = backendService.getEpochService.getProtocolParameters
        if !result.isSuccessful then sys.error(result.getResponse)
        result.getValue
    }
    private lazy val quickTxBuilder = QuickTxBuilder(backendService)

    private lazy val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)

    private lazy val evaluator = ScalusTransactionEvaluator(
      slotConfig = SlotConfig.Preprod,
      protocolParams = protocolParams,
      utxoSupplier = utxoSupplier,
      scriptSupplier = NoScriptSupplier(),
      mode = EvaluatorMode.EVALUATE_AND_COMPUTE_COST
    )

    override def mkSettlement(
        r: SettlementRecipe
    ): Either[String, SettlementTx] =

        val refs = r.deposits.map(d => (d.txId, d.txIx)) + headStateReader.currentTreasuryRef

        val utxos: Set[Utxo] =
            refs.map(r =>
                backendService.getUtxoService.getTxOutput(r._1.hash, r._2.ix.intValue).force
            )

        val treasuryValue: List[Amount] = utxos.toList.flatMap(u => u.getAmount.asScala)

        val Some(headAddressBech32) = headStateReader.headBechAddress

        val treasuryDatum = Interop.toPlutusData(
          mkInitMultisigTreasuryDatum(r.majorVersion, ByteString.empty).toData
        )

        val tx = Tx()
            .collectFrom(utxos.asJava)
            .payToContract(
              headAddressBech32.bech32,
              treasuryValue.asJava,
              treasuryDatum
            )
            .from(headAddressBech32.bech32)

        val ret = quickTxBuilder
            .compose(tx)
            .withTxEvaluator(evaluator)
            // TODO: 3 witnesses + 3 (roughly for the native script)
            // TODO: magic numbers
            .additionalSignersCount(3 + 3)
            .build

        // TODO: Add native script before balancing
        // I didn't find the way to add native script
        val Some(headNativeScript) = headStateReader.headNativeScript
        val script = NativeScript.deserializeScriptRef(headNativeScript.bytes)
        ret.getWitnessSet.setNativeScripts(List(script).asJava)

        Right(SettlementTx.apply(L1Tx(ret.serialize())))
}
