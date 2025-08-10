package hydrozoa.l1

import hydrozoa.node.monitoring.Metrics
import hydrozoa.{AddressL1, L1, Output, TxL1, Utxo, UtxoIdL1}
import ox.channels.ActorRef
import ox.resilience.RetryConfig
import ox.scheduling.Jitter
import scalus.cardano.address.Network
import scalus.cardano.ledger.{Coin, TransactionHash}
import scalus.ledger.api.v1.PosixTime

import scala.concurrent.duration.DurationInt

trait CardanoL1 {
    def setMetrics(metrics: ActorRef[Metrics]): Unit
    //
    def submit(tx: TxL1): Either[SubmissionError, TransactionHash]
    def awaitTx(
        txId: TransactionHash,
        retryConfig: RetryConfig[Throwable, Option[TxL1]] =
            RetryConfig.backoff(10, 100.millis, 1.seconds, Jitter.Equal)
    ): Option[TxL1]
    def network: Network
    def lastBlockTime: PosixTime

    def utxosAtAddress(headAddress: AddressL1): List[(Utxo[L1])]

    def utxoIdsAdaAtAddress(headAddress: AddressL1): Map[UtxoIdL1, Coin]
}

type SubmissionError = String
