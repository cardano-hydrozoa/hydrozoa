package hydrozoa.l1.multisig.tx.deposit

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.api.model.Amount.ada
import com.bloxbean.cardano.client.api.model.{Amount, ProtocolParams}
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import com.bloxbean.cardano.client.transaction.spec.Transaction
import hydrozoa.infra.toEither
import hydrozoa.l1.multisig.state.given_ToData_DepositDatum
import hydrozoa.node.server.HeadStateReader
import hydrozoa.{AppCtx, L1Tx, TxIx}
import scalus.bloxbean.*
import scalus.builtin.Data.toData

import scala.jdk.CollectionConverters.*

// TODO factor out common parts into a separate component
class BloxBeanDepositTxBuilder(
    ctx: AppCtx,
    headStateReader: HeadStateReader
) extends DepositTxBuilder {

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

    override def mkDepositTx(r: DepositTxRecipe): Either[String, (L1Tx, TxIx)] =
        for
            fundUtxo <- backendService.getUtxoService
                .getTxOutput(r.utxo._1.hash, r.utxo._2.ix.intValue)
                .toEither

            Some(headAddressBech32) = headStateReader.headBechAddress()
            // TODO: valueToAmountList(fundUtxo.toValue) OR we should ask for a value (might be easier)
            amountList: List[Amount] = List(ada(100))
            datum: PlutusData = Interop.toPlutusData(r.datum.toData)

            tx = Tx()
                .collectFrom(List(fundUtxo).asJava)
                .payToContract(headAddressBech32.bech32, amountList.asJava, datum)
                .from(fundUtxo.getAddress)

            ret: Transaction = quickTxBuilder
                .compose(tx)
                .withTxEvaluator(evaluator)
                .withRequiredSigners(Address(fundUtxo.getAddress))
                .build()

            // FIXME: returns -1 for some reason
            // index = ret.getBody.getOutputs.asScala
            //   .indexWhere(_.getAddress.equals(headAddressBech32))
            index = 0
        yield (L1Tx(ret.serialize()), TxIx(index))
}
