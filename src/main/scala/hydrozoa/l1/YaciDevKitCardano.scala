package hydrozoa.l1

import hydrozoa.{AppCtx, TxAny, Network, TxId}
import scalus.ledger.api.v1.PosixTime

class YaciDevKitCardano(ctx: AppCtx) extends Cardano {

    private val backendService = ctx.backendService

    override def submit(tx: TxAny): Either[SubmissionError, TxId] = {
        val result = backendService.getTransactionService.submitTransaction(tx.bytes)
        Either.cond(result.isSuccessful, TxId(result.getValue), result.getResponse)
    }

    override def network: Network =
        val nw = ctx.network
        Network(nw.getNetworkId, nw.getProtocolMagic)

    override def lastBlockTime: PosixTime =
        ctx.backendService.getBlockService.getLatestBlock.getValue.getTime
}
