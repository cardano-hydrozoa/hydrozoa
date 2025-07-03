package hydrozoa.l1

import com.bloxbean.cardano.client.api.model.Utxo
import hydrozoa.node.monitoring.Metrics
import hydrozoa.{AddressBechL1, Network, TxId, TxL1, UtxoIdL1}
import ox.channels.ActorRef
import ox.resilience.RetryConfig
import ox.scheduling.Jitter
import scalus.ledger.api.v1.PosixTime

import scala.concurrent.duration.DurationInt

trait CardanoL1 {
    def setMetrics(metrics: ActorRef[Metrics]): Unit
    //
    def submit(tx: TxL1): Either[SubmissionError, TxId]
    def awaitTx(
        txId: TxId,
        retryConfig: RetryConfig[Throwable, Option[TxL1]] =
            RetryConfig.backoff(10, 100.millis, 1.seconds, Jitter.Equal)
    ): Option[TxL1]
    def network: Network
    def lastBlockTime: PosixTime

    // FIXME: BB type
    def utxosAtAddress(headAddress: AddressBechL1): List[Utxo]

    def utxoIdsAdaAtAddress(headAddress: AddressBechL1): Map[UtxoIdL1, BigInt]
}

type SubmissionError = String
