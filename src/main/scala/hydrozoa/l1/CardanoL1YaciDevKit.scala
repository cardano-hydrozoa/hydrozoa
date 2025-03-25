package hydrozoa.l1

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.*
import hydrozoa.infra.toEither
import scalus.ledger.api.v1.PosixTime

import scala.util.boundary

class CardanoL1YaciDevKit(backendService: BackendService) extends CardanoL1:

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

    override def network: Network = networkL1static

    override def lastBlockTime: PosixTime =
        backendService.getBlockService.getLatestBlock.getValue.getTime
