package hydrozoa.l1

import hydrozoa.infra.toEither
import hydrozoa.{AppCtx, Network, TxId, TxL1}
import scalus.ledger.api.v1.PosixTime

import scala.util.boundary

class CardanoL1YaciDevKit(ctx: AppCtx) extends CardanoL1:

    private val backendService = ctx.backendService

    override def submit(tx: TxL1): Either[SubmissionError, TxId] = {
        val result = backendService.getTransactionService.submitTransaction(tx.bytes)
        Either.cond(result.isSuccessful, TxId(result.getValue), result.getResponse)
    }

    override def awaitTx(txId: TxId): Unit =
        boundary {
            1 to 42 foreach { _ =>
                backendService.getTransactionService.getTransaction(txId.hash).toEither match
                    case Left(_)  => Thread.sleep(100)
                    case Right(_) => boundary.break()
            }
        }

    override def network: Network =
        val nw = ctx.network
        Network(nw.getNetworkId, nw.getProtocolMagic)

    override def lastBlockTime: PosixTime =
        ctx.backendService.getBlockService.getLatestBlock.getValue.getTime
