package hydrozoa.l1

import com.bloxbean.cardano.client.backend.api.BackendService
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{toEither, txHash}
import ox.resilience.{RetryConfig, retry}
import ox.scheduling.Jitter
import scalus.ledger.api.v1.PosixTime

import scala.concurrent.duration.DurationInt
import scala.util.{Try, boundary}

class CardanoL1YaciDevKit(backendService: BackendService) extends CardanoL1:

    private val log = Logger(getClass)

    /** Submit for Yaci (and real networks for that matter) should take into account
     * that Ogmios seems to fail if a tx has been already submitted before.
     * @param tx
     * @return
     */
    override def submit(tx: TxL1): Either[SubmissionError, TxId] = {
        val hash = txHash(tx)

        def smartSubmit =
            backendService.getTransactionService.getTransaction(hash.hash).toEither match
                case Right(_) =>
                    log.info(s"Tx already on the chain: $hash")
                    hash
                case Left(_) =>
                    val result = backendService.getTransactionService.submitTransaction(tx.bytes)
                    if result.isSuccessful
                    then TxId(result.getValue)
                    else throw RuntimeException(result.getResponse)

        Try(
          retry(RetryConfig.backoff(10, 100.millis, 1.seconds, Jitter.Equal))(smartSubmit)
        ).toEither.swap.map(_.toString).swap

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
