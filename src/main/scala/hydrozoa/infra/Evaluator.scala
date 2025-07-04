package hydrozoa.infra

import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.bloxbean.cardano.client.backend.api.{BackendService, DefaultUtxoSupplier}
import com.bloxbean.cardano.client.quicktx.{AbstractTx, QuickTxBuilder}
import hydrozoa.l1.rulebased.onchain.{DisputeResolutionScript, TreasuryValidatorScript}
import scalus.bloxbean.*

import scala.jdk.CollectionConverters
import scala.jdk.CollectionConverters.MapHasAsJava

/** @tparam T
  *   likely you should use Tx
  * @return
  *   function that takes a tx skeleton and return the builder
  */
def mkBuilder[T](backendService: BackendService): AbstractTx[T] => QuickTxBuilder#TxContext =
    (tx: AbstractTx[T]) =>

        val protocolParams: ProtocolParams = {
            val result = backendService.getEpochService.getProtocolParameters
            if !result.isSuccessful then sys.error(result.getResponse)
            val pp = result.getValue

            // val ow = new ObjectMapper().writer.withDefaultPrettyPrinter
            // val json = ow.writeValueAsString(pp)
            // println(json)

            pp
        }

        val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)
        val scriptSupplier = new ScriptServiceSupplier(backendService.getScriptService)

        val evaluator = ScalusTransactionEvaluator(
          slotConfig = SlotConfig.Preprod, // FIXME: use config parameter
          protocolParams = protocolParams,
          utxoSupplier = utxoSupplier,
          scriptSupplier = scriptSupplier,
          mode = EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

        QuickTxBuilder(backendService)
            .compose(tx)
            .withTxEvaluator(evaluator)
