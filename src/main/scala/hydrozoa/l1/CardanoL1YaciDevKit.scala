package hydrozoa.l1

import com.bloxbean.cardano.client.api.model.{Utxo as BBUtxo}
import com.bloxbean.cardano.client.backend.api.BackendService
import com.typesafe.scalalogging.Logger
import hydrozoa.{Utxo as HUtxo, *}
import hydrozoa.infra.{toEither, txHash}
import hydrozoa.node.monitoring.Metrics
import ox.channels.ActorRef
import ox.resilience.{RetryConfig, retry}
import ox.scheduling.Jitter
import scalus.builtin.ByteString
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
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
    private val knownTxs: mutable.Map[TransactionHash, TxL1] = mutable.Map()

    /** Submit for Yaci (and real networks for that matter) should take into account that Ogmios
      * seems to fail if a tx has been already submitted before.
      * @param tx
      * @return
      */
    override def submit(tx: TxL1): Either[SubmissionError, TransactionHash] = {
        val hash = txHash(tx)

        log.info(s"Submitting tx $hash")

        def handleSubmit(): Unit = {
            knownTxs.put(hash, tx)
            metrics.tell(_.addFeesL1Volume(tx.body.value.fee.value))
        }

        def smartSubmit =
            backendService.getTransactionService.getTransaction(hash.toHex).toEither match
                case Left(_) =>
                    val result =
                        backendService.getTransactionService.submitTransaction(tx.toCbor)
                    if result.isSuccessful
                    then
                        log.info(s"Tx $hash has been submitted to L1.")
                        handleSubmit()
                        Hash[Blake2b_256, HashPurpose.TransactionHash](
                          ByteString.fromHex((result.getValue))
                        )
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
        txId: TransactionHash,
        retryConfig: RetryConfig[Throwable, Option[TxL1]]
    ): Option[TxL1] =
        def tryAwait =
            backendService.getTransactionService.getTransaction(txId.toHex).toEither match
                case Left(_)  => throw RuntimeException(s"Tx: $txId hasn't appeared.")
                case Right(_) => knownTxs.get(txId)

        Try(retry(retryConfig)(tryAwait)).get

    override def network: Network = networkL1static

    override def lastBlockTime: PosixTime =
        backendService.getBlockService.getLatestBlock.getValue.getTime

    override def utxosAtAddress(headAddress: AddressL1): List[HUtxo[L1]] =
        // FIXME: can't be more than 100
        backendService.getUtxoService
            .getUtxos(headAddress.toBech32.get, 100, 1)
            .toEither match
            case Left(err) =>
                throw RuntimeException(err)
            case Right(utxos) => utxos.asScala.toList.map(bbutxo => HUtxo.fromBB(bbutxo))

    override def utxoIdsAdaAtAddress(headAddress: AddressL1): Map[UtxoIdL1, Coin] =
        // NB: can't be more than 100
        backendService.getUtxoService
            .getUtxos(headAddress.toBech32.get, 100, 1)
            .toEither match
            case Left(err) =>
                throw RuntimeException(err)
            case Right(utxos) =>
                utxos.asScala
                    .map(u =>
                        (
                          UtxoIdL1(
                            TransactionInput(
                              Hash[Blake2b_256, HashPurpose.TransactionHash](
                                ByteString.fromHex(u.getTxHash)
                              ),
                              u.getOutputIndex
                            )
                          ),
                          Coin(BigInt.apply(
                            u.getAmount.asScala
                                .find(a => a.getUnit.equals("lovelace"))
                                .get
                                .getQuantity
                          ).toLong)
                        )
                    )
                    .toMap
