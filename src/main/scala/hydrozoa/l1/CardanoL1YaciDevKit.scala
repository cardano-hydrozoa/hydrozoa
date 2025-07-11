package hydrozoa.l1

import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.backend.api.BackendService
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{toEither, txFees, txHash}
import hydrozoa.node.monitoring.Metrics
import ox.channels.ActorRef
import ox.resilience.{RetryConfig, retry}
import ox.scheduling.Jitter
import scalus.ledger.api.v1.PosixTime

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.util.Try

class CardanoL1YaciDevKit(backendService: BackendService) extends CardanoL1:

    private val log = Logger(getClass)

    private var metrics: ActorRef[Metrics] = _

    override def setMetrics(metrics: ActorRef[Metrics]): Unit =
        this.metrics = metrics

    // TODO: temporarily: Yaci cannot return serialized tx so far
    private val knownTxs: mutable.Map[TxId, TxL1] = mutable.Map()

    /** Submit for Yaci (and real networks for that matter) should take into account that Ogmios
      * seems to fail if a tx has been already submitted before.
      * @param tx
      * @return
      */
    override def submit(tx: TxL1): Either[SubmissionError, TxId] = {
        val hash = txHash(tx)

        log.info(s"Submitting tx $hash")

        def handleSubmit(): Unit = {
            knownTxs.put(hash, tx)
            metrics.tell(_.addFeesL1Volume(txFees(tx)))
        }

        def smartSubmit =
            backendService.getTransactionService.getTransaction(hash.hash).toEither match
                case Left(_) =>
                    val result = backendService.getTransactionService.submitTransaction(tx.bytes)
                    if result.isSuccessful
                    then
                        log.info(s"Tx $hash has been submitted to L1.")
                        handleSubmit()
                        TxId(result.getValue)
                    else throw RuntimeException(result.getResponse)
                case Right(_) =>
                    log.info(s"Tx already on the chain: $hash")
                    handleSubmit()
                    hash

        Try(
          retry(RetryConfig.backoff(10, 100.millis, 1.seconds, Jitter.Equal))(smartSubmit)
        ).toEither.swap.map(_.toString).swap

    }

    override def awaitTx(
        txId: TxId,
        retryConfig: RetryConfig[Throwable, Option[TxL1]]
    ): Option[TxL1] =
        def tryAwait =
            backendService.getTransactionService.getTransaction(txId.hash).toEither match
                case Left(_)  => throw RuntimeException(s"Tx: $txId hasn't appeared.")
                case Right(_) => knownTxs.get(txId)

        Try(retry(retryConfig)(tryAwait)).get

    override def network: Network = networkL1static

    override def lastBlockTime: PosixTime =
        backendService.getBlockService.getLatestBlock.getValue.getTime

    override def utxosAtAddress(headAddress: AddressBechL1): List[Utxo] =
        // FIXME: can't be more than 100
        backendService.getUtxoService.getUtxos(headAddress.bech32, 100, 1).toEither match
            case Left(err) =>
                throw RuntimeException(err)
            case Right(utxos) => utxos.asScala.toList

    override def utxoIdsAdaAtAddress(headAddress: AddressBechL1): Map[UtxoIdL1, BigInt] =
        // NB: can't be more than 100
        backendService.getUtxoService.getUtxos(headAddress.bech32, 100, 1).toEither match
            case Left(err) =>
                throw RuntimeException(err)
            case Right(utxos) =>
                utxos.asScala
                    .map(u =>
                        (
                          UtxoIdL1(TxId(u.getTxHash), TxIx(u.getOutputIndex)),
                          BigInt.apply(
                            u.getAmount.asScala
                                .find(a => a.getUnit.equals("lovelace"))
                                .get
                                .getQuantity
                          )
                        )
                    )
                    .toMap
