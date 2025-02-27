package hydrozoa.l1.multisig.tx.refund

import com.bloxbean.cardano.client.api.model.Amount.lovelace
import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import com.bloxbean.cardano.client.transaction.util.TransactionUtil.getTxHash
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.infra.txOutputToUtxo
import hydrozoa.l1.multisig.state.{DepositDatum, given_FromData_DepositDatum}
import hydrozoa.l1.multisig.tx.MultisigTxs.PostDatedRefundTx
import hydrozoa.node.server.HeadStateReader
import hydrozoa.{AppCtx, L1Tx}
import scalus.bloxbean.*
import scalus.builtin.Data.fromData

import java.math.BigInteger
import scala.jdk.CollectionConverters.*

class BloxBeanRefundTxBuilder(
    ctx: AppCtx,
    headStateReader: HeadStateReader
) extends RefundTxBuilder {

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

    override def mkPostDatedRefund(
        r: PostDatedRefundRecipe
    ): Either[String, PostDatedRefundTx] =

        val txBytes = r.depositTx.toTx.bytes
        val tb = Transaction.deserialize(txBytes)
        val txHash = getTxHash(txBytes)
        val txIxInt = r.txIx.ix.intValue()
        val depositOutput = tb.getBody.getOutputs.get(txIxInt) // TODO: may throw
        val depositUtxo = txOutputToUtxo(txHash, txIxInt, depositOutput)

        val datum: DepositDatum = fromData[DepositDatum](
          Interop.toScalusData(
            PlutusData.deserialize(HexUtil.decodeHexString(depositUtxo.getInlineDatum))
          )
        )

        val Some(headAddressBech32) = headStateReader.headBechAddress()
        val refundAddress = ctx.account.enterpriseAddress
        // TODO: Checks/missing things
        // 1. Deposit is locked at the head's script may be not necessary
        // 2. Use refund datum
        // 3. Use refund address, not ctx.account.enterpriseAddress()

        val tx = Tx()
            .collectFrom(List(depositUtxo).asJava)
            .payToAddress(
              refundAddress,
              // TODO: split up 5 ada for fees
              lovelace(depositOutput.getValue.getCoin.subtract(BigInteger("5000000")))
            )
            // TODO: This won't work, but it's ok for now.
            // .withChangeAddress(refundAddress)
            .from(headAddressBech32.bech32)

        val ret = quickTxBuilder
            .compose(tx)
            .withTxEvaluator(evaluator)
            // TODO: 3 witnesses + 3 (roughly for the native script)
            .additionalSignersCount(3 + 3)
            .build

        // TODO: Add native script before balancing
        // I didn't find the way to add native script
        val Some(headNativeScript) = headStateReader.headNativeScript()
        val script = NativeScript.deserializeScriptRef(headNativeScript.bytes)
        ret.getWitnessSet.setNativeScripts(List(script).asJava)

        Right(PostDatedRefundTx.apply(L1Tx(ret.serialize())))
}
