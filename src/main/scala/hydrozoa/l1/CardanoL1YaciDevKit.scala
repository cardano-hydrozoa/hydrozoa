package hydrozoa.l1

import com.bloxbean.cardano.client.backend.api.BackendService
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.typesafe.scalalogging.Logger
import hydrozoa.infra.transitionary.toScalus
import hydrozoa.infra.{Piper, toEither}
import hydrozoa.l1.YaciCluster.{YaciClusterInfo, adminApiBaseUri, blockfrostApiBaseUri}
import hydrozoa.node.monitoring.Metrics
import hydrozoa.{Utxo as HUtxo, *}
import ox.channels.ActorRef
import ox.resilience.{RetryConfig, retry}
import ox.scheduling.Jitter
import scalus.builtin.ByteString
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.ledger.api.v1.PosixTime
import sttp.client4.quick.*
import sttp.client4.{Response, ResponseAs}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.util.Try

class CardanoL1YaciDevKit(
    backendService: BackendService,
    yaciClusterInfo: YaciClusterInfo
) extends CardanoL1:

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
        val hash = tx.id

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
        retryConfig: RetryConfig[Throwable, Option[Unit]]
    ): Option[Unit] =
        def tryAwait =
            backendService.getTransactionService.getTransaction(txId.toHex).toEither match
                case Left(_) => throw RuntimeException(s"Tx: $txId hasn't appeared.")
                // TODO: this won't work now for deposit txs :-(
                case Right(_) => Some(())

        Try(retry(retryConfig)(tryAwait)).get

    override def network: Network = networkL1static

    override def lastBlockTime: PosixTime =
        backendService.getBlockService.getLatestBlock.getValue.getTime

    override def lastBlockSlot: Slot =
        backendService.getBlockService.getLatestBlock.getValue.getSlot |> Slot.apply

    override def utxosAtAddress(headAddress: AddressL1): List[HUtxo[L1]] =
        // FIXME: can't be more than 100
        backendService.getUtxoService
            .getUtxos(headAddress.toBech32.get, 100, 1)
            .toEither match
            case Left(err) =>
                throw RuntimeException(err)
            case Right(utxos) =>
                utxos.asScala.toList.map(bbutxo =>
                    val s = bbutxo.toScalus
                    HUtxo[L1](UtxoId[L1](s._1), Output[L1](s._2))
                )

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
                          Coin(
                            BigInt
                                .apply(
                                  u.getAmount.asScala
                                      .find(a => a.getUnit.equals("lovelace"))
                                      .get
                                      .getQuantity
                                )
                                .toLong
                          )
                        )
                    )
                    .toMap

    override def slotToTime(slot: Slot): PosixTime =
        val secondsDecimal = slot.slot * yaciClusterInfo.slotLength + yaciClusterInfo.startTime
        secondsDecimal.rounded.toBigInt

    case class TransactionContent(
        ttl: Long
    )

    implicit val codec: JsonValueCodec[TransactionContent] = JsonCodecMaker.make

    override def txTtl(txId: TransactionHash): Option[Slot] =
        // TODO: Obtain TTL, report the issue to Satya
        Try(quickRequest
            .get(uri"$blockfrostApiBaseUri/txs/${txId.toHex}")
            .response(asJsoniterBytes[TransactionContent])
            .send()).toEither match
            case Left(err) =>
                log.error(s"Error while getting tx content: $err")
                None
            case Right(txInfo) =>
                Option(txInfo.body.ttl)
                    .map(s => Slot.apply(BigInt.apply(s).longValue))

object YaciCluster:

    val adminApiBaseUri = "http://localhost:10000/local-cluster/api/admin"
    val blockfrostApiBaseUri = uri"http://localhost:8080/api/v1/"

    // TODO: use external network topology config?
    // val blockfrostApiBaseUri = "http://yaci-cli:8080/api/v1/"

    private val log = Logger(getClass)

    case class YaciClusterInfo(
        slotLength: BigDecimal,
        startTime: Long,
        protocolMagic: Int
    )

    implicit val codec: JsonValueCodec[YaciClusterInfo] = JsonCodecMaker.make

    def reset(): YaciClusterInfo =
        log.info("Resetting Yaci cluster...")

        val _ = quickRequest
            .post(uri"$adminApiBaseUri/devnet/reset")
            .send()

        obtainClusterInfo()

    def obtainClusterInfo(): YaciClusterInfo =
        // Obtain the cluster information
        val clusterInfo = quickRequest
            .get(uri"$adminApiBaseUri/devnet")
            .response(asJsoniterBytes[YaciClusterInfo])
            .send()
            .body

        log.info(s"Cluster info: $clusterInfo")

        clusterInfo

// ResponseAs that decodes from Array[Byte] using jsoniter
def asJsoniterBytes[T: JsonValueCodec]: ResponseAs[T] =
    asByteArray.map(bytes => readFromArray[T](bytes.toOption.get))
