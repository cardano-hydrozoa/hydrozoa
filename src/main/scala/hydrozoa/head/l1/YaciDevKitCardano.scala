package hydrozoa.head.l1

import hydrozoa.head.{Network, L1Tx, TxId}

class YaciDevKitCardano(ctx: AppCtx) extends Cardano {

    private val backendService = ctx.backendService

    override def submit(tx: L1Tx): Either[SubmissionError, TxId] = {
        val result = backendService.getTransactionService.submitTransaction(tx.bytes)
        Either.cond(result.isSuccessful, TxId(result.getValue), result.getResponse)
    }

    override def network(): Network =
        val nw = ctx.network
        Network(nw.getNetworkId, nw.getProtocolMagic)
}
