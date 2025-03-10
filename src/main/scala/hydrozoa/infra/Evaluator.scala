package hydrozoa.infra

import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.quicktx.{AbstractTx, QuickTxBuilder}
import hydrozoa.AppCtx
import scalus.bloxbean.{EvaluatorMode, NoScriptSupplier, ScalusTransactionEvaluator, SlotConfig}

/** @param ctx
  * @tparam T
  *   likely you should use Tx
  * @return
  *   function that takes a tx skeleton and return the builder
  */
def mkBuilder[T](ctx: AppCtx): AbstractTx[T] => QuickTxBuilder#TxContext =
    (tx: AbstractTx[T]) =>
        val backendService = ctx.backendService

        val protocolParams: ProtocolParams = {
            val result = backendService.getEpochService.getProtocolParameters
            if !result.isSuccessful then sys.error(result.getResponse)
            result.getValue
        }

        val quickTxBuilder = QuickTxBuilder(backendService)

        val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)

        val evaluator = ScalusTransactionEvaluator(
          slotConfig = SlotConfig.Preprod, // FIMXE: paramater
          protocolParams = protocolParams,
          utxoSupplier = utxoSupplier,
          scriptSupplier = NoScriptSupplier(),
          mode = EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

        QuickTxBuilder(backendService)
            .compose(tx)
            .withTxEvaluator(evaluator)
