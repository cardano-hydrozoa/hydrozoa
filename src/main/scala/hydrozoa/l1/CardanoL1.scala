package hydrozoa.l1

import hydrozoa.*
import hydrozoa.node.monitoring.Metrics
import ox.channels.ActorRef
import ox.resilience.RetryConfig
import ox.scheduling.Jitter
import scalus.cardano.address.Network
import scalus.cardano.ledger.{Coin, Slot, TransactionHash}
import scalus.ledger.api.v1.PosixTime

import scala.concurrent.duration.DurationInt

trait CardanoL1 {
    def setMetrics(metrics: ActorRef[Metrics]): Unit

    def submit(tx: TxL1): Either[SubmissionError, TransactionHash]

    def awaitTx(
        txId: TransactionHash,
        retryConfig: RetryConfig[Throwable, Option[Unit]] =
            RetryConfig.backoff(10, 100.millis, 1.seconds, Jitter.Equal)
    ): Option[Unit]

    def network: Network

    // TODO: do we still need it after refactoring deposit timeouts?
    def lastBlockTime: PosixTime

    def lastBlockSlot: Slot

    def utxosAtAddress(headAddress: AddressL1): List[(Utxo[L1])]

    def utxoIdsAdaAtAddress(headAddress: AddressL1): Map[UtxoIdL1, Coin]

    def slotToTime(slot: Slot): PosixTime

    /** Try to find a tx and take its upper bound validity interval a.k.a. TTL.
      * @param txId
      * @return
      *   None if a tx not found or it TTL was not set.
      */
    def txTtl(txId: TransactionHash): Option[Slot]
}

type SubmissionError = String
