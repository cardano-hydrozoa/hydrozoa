package hydrozoa.multisig.backend.cardano

import cats.data.EitherT
import cats.effect.*
import cats.syntax.traverse.*
import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.Result
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.backend.model.{AssetTransactionContent, ScriptDatumCbor, TxContentRedeemers}
import com.bloxbean.cardano.client.plutus.spec.RedeemerTag
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend.Error.*
import hydrozoa.multisig.backend.cardano.CardanoBackend.{ContinuingTx, Error}
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import io.bullet.borer.Cbor
import io.circe.parser.parse
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.jdk.CollectionConverters.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockfrostProvider, UtxoFilter, UtxoQuery, UtxoQueryError, UtxoSource}
import scalus.uplc.builtin.{ByteString, Data}

/** Cardano backend to use with Blockfrost-compatible API. UTxO reads and protocol parameters go
  * through Scalus' [[BlockfrostProvider]]; tx lookups, asset history, redeemers and submission go
  * through BloxBean's [[BackendService]].
  *
  * @param backendService
  *   BloxBean backend service
  * @param pageSize
  *   Used when paginating over methods of [[backendService]]
  * @param blockfrostProviderFuture
  *   Used to fulfill get protocol parameters method. We keep it as Future till the time we use it
  *   to maintain semantic we had before Scalus refactored [[BlockfrostProvider]], we want to deffer
  *   all errors till the time we actualy use the provider.
  */

class CardanoBackendBlockfrost private (
    private val backendService: BackendService,
    private val pageSize: Int,
    /** Builds a FRESH provider. Called again after a failure -- see [[provider]]. */
    private val mkProvider: IO[BlockfrostProvider],
    /** Memoises the provider ON SUCCESS ONLY. */
    private val providerRef: Ref[IO, Option[BlockfrostProvider]],
    protected val tracer: ContraTracer[IO, CardanoBackendEvent],
    // Base URL + project id for the raw tx-utxos read in [[continuingTx]] — BloxBean's
    // `TxContentUtxoInputs` model drops the per-input tx_hash/output_index/collateral/reference we
    // need there, so that one call goes straight to the JSON.
    private val baseUrl: String,
    private val apiKey: String
) extends CardanoBackend[IO] {

    /** The Scalus provider, built on first use and memoised **only if it succeeds**.
      *
      * Building it fetches from Blockfrost, so it can fail. A memoised failure — what an eager
      * Scala `Future` would keep — would fail every later UTxO poll and `fetchLatestParams` for the
      * life of the process, with no way back short of a restart. So: retry on failure, memoise on
      * success.
      *
      * ⚠️ Two concurrent first-callers may each build one; the loser's is discarded. Harmless (the
      * discarded provider has only its own empty script cache) and cheaper than serialising every
      * access.
      */
    private def provider: IO[BlockfrostProvider] =
        providerRef.get.flatMap {
            case Some(p) => IO.pure(p)
            case None    => mkProvider.flatTap(p => providerRef.set(Some(p)))
        }

    /** Runs a UTxO query on the provider. A failure to build the provider, or an exception from the
      * query, comes back as a `Left` like any other query error.
      */
    private def findUtxos(query: UtxoQuery): IO[Either[UtxoQueryError, Utxos]] =
        provider
            .flatMap(p => IO.fromFuture(IO.delay(p.findUtxos(query))))
            .handleError(e => Left(UtxoQueryError.NetworkError(e.getMessage, Some(e))))

    private val httpClient: HttpClient = HttpClient.newHttpClient()

    // BloxBean's get*Service builds a fresh Retrofit + OkHttpClient (its own
    // connection pool and dispatcher threads) on every call — and pagination
    // calls it once per page. Cache one instance per service.
    private val scriptService = backendService.getScriptService
    private val transactionService = backendService.getTransactionService
    private val assetService = backendService.getAssetService

    /** Reads the input's transaction rather than querying the input itself: Scalus reports any
      * failure of an input query as `NotFound`, so a Blockfrost outage would look like a spent
      * input. A transaction query keeps "no such tx" (HTTP 404) apart from a failed request.
      */
    override def resolve(input: Input): IO[Either[Error, Option[ledger.Utxo]]] =
        findUtxos(UtxoQuery(UtxoSource.FromTransaction(input.transactionId))).map {
            case Right(utxos) => Right(utxos.get(input).map(ledger.Utxo(input, _)))
            case Left(UtxoQueryError.NotFound(_)) => Right(None)
            case Left(error)                      => Left(ErrorResolving(input, error.toString))
        }

    override def utxosAt(address: ShelleyAddress): IO[Either[CardanoBackend.Error, Utxos]] =
        findUtxos(UtxoQuery(UtxoSource.FromAddress(address)))
            .map(_.left.map(error => Unexpected(error.toString)))

    override def utxosAt(
        address: ShelleyAddress,
        asset: (PolicyId, AssetName)
    ): IO[Either[CardanoBackend.Error, Utxos]] =
        findUtxos(
          UtxoQuery(UtxoSource.FromAddress(address)) && UtxoFilter.HasAsset(asset._1, asset._2)
        )
            .map(_.left.map(error => Unexpected(error.toString)))

    /** `IO.blocking`, here and at every other `backendService` call site: those calls are
      * synchronous okhttp, so the thread that enters one stays there for the whole HTTPS round
      * trip. On a compute worker the runtime cannot see that and does not start a replacement, so
      * the pool runs a thread short until the call returns.
      *
      * This one is the worst of them — one round trip per page, on the CardanoLiaison poll path
      * that runs for the life of the head.
      */
    private def paginate[A](
        apiCall: Int => Result[java.util.List[A]],
        mbStopPred: Option[A => Boolean]
    ): IO[Either[CardanoBackend.Error, List[A]]] =
        IO.blocking {
            val elems: mutable.Buffer[A] = mutable.Buffer.empty
            var page: Int = 1

            while {
                val result = apiCall(page)
                if result.isSuccessful then {
                    result.getValue.asScala.toList match {
                        case Nil => false
                        case someElems =>
                            val toAdd = mbStopPred.fold(someElems)(stopPred =>
                                someElems.takeWhile(e => !stopPred(e))
                            )
                            elems.addAll(toAdd)
                            page = page + 1
                            toAdd.sizeIs == someElems.size
                    }
                } else {
                    // Blockfrost replies with HTTP 404 when there is no elements on the list
                    if result.code() == 404
                    then false
                    else
                        throw RuntimeException(
                          s"Non-404 error while trying to fetch page $page: ${result.getResponse}"
                        )
                }
            } do ()
            Right(elems.toList)
        }.handleError(e =>
            Left(Unexpected(s"${e.getMessage}, caused by: ${
                    if e.getCause != null then e.getCause.getMessage else "N/A"
                }"))
        )

    /** `IO.blocking`: a synchronous Blockfrost round trip — see [[paginate]] for why that matters.
      */
    override def isTxKnown(
        txHash: TransactionHash
    ): IO[Either[CardanoBackend.Error, Boolean]] =
        IO.blocking {
            val result = transactionService.getTransaction(txHash.toHex)
            if result.isSuccessful then {
                Right(true)
            } else {
                // Blockfrost replies with HTTP 404 when there is no such transaction
                if result.code() == 404
                then Right(false)
                else
                    throw RuntimeException(
                      s"Non-404 error while trying to call Blockfrost ${result.getResponse}"
                    )
            }
        }.handleError(e =>
            Left(Unexpected(s"${e.getMessage}, caused by: ${
                    if e.getCause != null then e.getCause.getMessage else "N/A"
                }"))
        )

    override def lastContinuingTxs(
        asset: (PolicyId, AssetName),
        after: TransactionHash
    ): IO[Either[CardanoBackend.Error, List[ContinuingTx]]] =
        val unit = s"${asset._1.toHex}${asset._2.bytes.toHex}"
        val hex = after.toHex
        (for {
            txIds <- EitherT(
              paginate(
                apiCall = page =>
                    assetService.getTransactions(
                      unit,
                      pageSize,
                      page,
                      OrderEnum.desc
                    ),
                mbStopPred = Some((c: AssetTransactionContent) => {
                    c.getTxHash == hex
                })
              ).map(ret =>
                  ret.map(content => content.map(e => TransactionHash.fromHex(e.getTxHash)))
              )
            )

            txRets <- txIds.traverse(txHash => EitherT(continuingTx(txHash, asset)))

        } yield txRets.flatten).value

    /** Tries to treat a transaction as one having a continuing output with the asset. Returns the
      * continuing output (as a resolvable utxo) together with the redeemer of the continuing input
      * if the tx conforms. Returns None if the tx doesn't conform the pattern, i.e., an input is
      * missing, an output is missing or the redeemer is missing. NB: Decoding redeemer error is
      * thrown though.
      *
      * @param txHash
      * @param unit
      *   the asset unit string (policyId + assetName hex)
      * @return
      */
    private def continuingTx(
        txHash: TransactionHash,
        asset: (PolicyId, AssetName)
    ): IO[Either[CardanoBackend.Error, Option[ContinuingTx]]] = {
        val (policyId, assetName) = asset
        val unit = s"${policyId.toHex}${assetName.bytes.toHex}"
        (for {
            outputs <- EitherT(
              findUtxos(UtxoQuery(UtxoSource.FromTransaction(txHash)))
                  .map(_.left.map(error => Unexpected(error.toString)))
            )
            // A redeemer's `tx_index` points into the tx's CANONICALLY-SORTED regular spend inputs
            // (excluding reference + collateral), not Blockfrost's returned input order — so index
            // the continuing (asset-bearing) input the same way, or `txRedeemer` looks it up at the
            // wrong slot and returns `SpendingRedeemerNotFound`, silently dropping the tx. BloxBean's
            // input model exposes neither the outref nor the reference/collateral flags, so we read
            // them from the raw tx-utxos JSON. (Alternative: decode the tx CBOR — its inputs are
            // already canonical and its redeemers carry input pointers — and pick the spend redeemer
            // whose data parses as a treasury redeemer, dropping index-matching entirely.)
            rawInputs <- EitherT(rawTxUtxoInputs(txHash))
            inputIx <- EitherT.fromOption[IO](
              opt = rawInputs
                  .filterNot(i => i.collateral || i.reference)
                  .sortBy(i => (i.txHash, i.outputIndex))
                  .zipWithIndex
                  .find { (input, _) => input.units.contains(unit) }
                  .map(_._2),
              ifNone = NoTxInputWithAsset(txHash, unit)
            )
            continuingOutput <- EitherT.fromOption[IO](
              opt = outputs
                  .find((_, output) =>
                      output.value.assets.assets.get(policyId).exists(_.contains(assetName))
                  )
                  .map(ledger.Utxo(_)),
              ifNone = NoTxOutputWithAsset(txHash, unit)
            )
            _ <- EitherT.cond[IO](
              continuingOutput.output.datumOption.exists(_.isInstanceOf[DatumOption.Inline]),
              (),
              NoInlineDatum(continuingOutput.input)
            )

            redeemerInfo <- EitherT(txRedeemer(txHash, inputIx))

            // The redeemer's Data is keyed by `redeemer_data_hash`, NOT `datum_hash` (the latter is
            // Blockfrost's deprecated/renamed field; on the Yaci store it resolves to the spent
            // input's datum instead, so `fromData[TreasuryRedeemer]` fails with "not a constructor").
            redeemerData <- EitherT(redeemerByHash(redeemerInfo.getRedeemerDataHash))

            redeemer <- EitherT.fromOption[IO](
              opt = scala.util.Try {
                  val datumBytes =
                      ByteString.fromHex(redeemerData.getCbor)
                  Cbor.decode(datumBytes.bytes).to[Data].value
              }.toOption,
              ifNone = ErrorDecodingRedeemerCbor(redeemerData.getCbor)
            )

        } yield Some(ContinuingTx(continuingOutput, redeemer))).value.map {
            // Some errors are ignored - there may be txs that doesn't conform
            // the pattern.
            case Left(NoTxInputWithAsset(_, _))       => Right(None)
            case Left(NoTxOutputWithAsset(_, _))      => Right(None)
            case Left(SpendingRedeemerNotFound(_, _)) => Right(None)
            case other                                => other
        }
    }

    /** One input of a Blockfrost `/txs/{hash}/utxos` response, carrying the fields BloxBean's
      * `TxContentUtxoInputs` model omits: the outref (`txHash`#`outputIndex`, for canonical
      * sorting) and the reference/collateral flags (to keep only regular spend inputs). See
      * [[continuingTx]].
      */
    private final case class RawTxInput(
        txHash: String,
        outputIndex: Int,
        collateral: Boolean,
        reference: Boolean,
        units: Set[String]
    )

    /** Read a tx's inputs straight from the Blockfrost `/txs/{hash}/utxos` JSON — see
      * [[RawTxInput]] for why [[continuingTx]] bypasses BloxBean for this one call.
      */
    private def rawTxUtxoInputs(
        txHash: TransactionHash
    ): IO[Either[CardanoBackend.Error, List[RawTxInput]]] =
        IO.blocking {
            val request = HttpRequest
                .newBuilder(URI.create(s"$baseUrl/txs/${txHash.toHex}/utxos"))
                .header("project_id", apiKey)
                .GET()
                .build()
            val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
            if response.statusCode() == 200 then
                parse(response.body()) match {
                    case Left(e) =>
                        Left(Unexpected(s"Malformed tx-utxos JSON: ${e.getMessage}"))
                    case Right(json) =>
                        val inputs = json.hcursor.downField("inputs").values.getOrElse(Nil)
                        Right(inputs.toList.map { i =>
                            val c = i.hcursor
                            RawTxInput(
                              txHash = c.get[String]("tx_hash").getOrElse(""),
                              outputIndex = c.get[Int]("output_index").getOrElse(0),
                              collateral = c.get[Boolean]("collateral").getOrElse(false),
                              reference = c.get[Boolean]("reference").getOrElse(false),
                              units = c
                                  .downField("amount")
                                  .values
                                  .getOrElse(Nil)
                                  .flatMap(_.hcursor.get[String]("unit").toOption)
                                  .toSet
                            )
                        })
                }
            else Left(Unexpected(s"tx-utxos HTTP ${response.statusCode()}: ${response.body()}"))
        }.handleError(e =>
            Left(Unexpected(s"${e.getMessage}, caused by: ${
                    if e.getCause != null then e.getCause.getMessage else "N/A"
                }"))
        )

    private def txRedeemer(
        txHash: TransactionHash,
        inputIx: Int
    ): IO[Either[CardanoBackend.Error, TxContentRedeemers]] =
        IO.delay(transactionService.getTransactionRedeemers(txHash.toHex))
            .map(res =>
                if res.isSuccessful
                then
                    res.getValue.asScala.toList
                        .find(r => r.getTxIndex == inputIx && r.getPurpose == RedeemerTag.Spend)
                        .toRight(SpendingRedeemerNotFound(txHash, inputIx))
                else
                    Left(
                      Unexpected(
                        s"Unexpected exception while retrieving tx redeemers: ${res.getResponse}"
                      )
                    )
            )
            .handleError(e =>
                Left(
                  Unexpected(
                    s"Unexpected exception while retrieving tx redeemers: ${e.getMessage}, caused by: ${
                            if e.getCause != null then e.getCause.getMessage else "N/A"
                        }"
                  )
                )
            )

    private def redeemerByHash(
        redeemerHash: String
    ): IO[Either[CardanoBackend.Error, ScriptDatumCbor]] =
        IO.delay(
          scriptService
              .getScriptDatumCbor(redeemerHash)
        ).map(res =>
            if res.isSuccessful then Right(res.getValue)
            else
                Left(
                  Unexpected(
                    s"Unexpected exception while retrieving redeemer by its hash: ${res.getResponse}"
                  )
                )
        ).handleError(e =>
            Left(
              Unexpected(
                s"Unexpected exception while retrieving redeemer by its hash: ${e.getMessage}, caused by: ${
                        if e.getCause != null then e.getCause.getMessage else "N/A"
                    }"
              )
            )
        )

    /** `IO.blocking`: a synchronous Blockfrost round trip — see [[paginate]] for why that matters.
      * This one is on the L1 effect path, so a stalled worker here delays settlement itself.
      */
    override def submitTx(etx: EnrichedTx[?]): IO[Either[CardanoBackend.Error, Unit]] =
        IO.blocking {
            val result = transactionService.submitTransaction(etx.tx.toCbor)
            if result.isSuccessful
            then Right(())
            else Left(Unexpected(result.getResponse))
        }.handleError(e =>
            Left(Unexpected(s"${e.getMessage}, caused by: ${
                    if e.getCause != null then e.getCause.getMessage else "N/A"
                }"))
        )

    override def fetchLatestParams: IO[Either[Error, ProtocolParams]] =
        (for
            p <- provider
            result <- IO.fromFuture(IO.pure(p.fetchLatestParams))
        yield Right(result))
            .handleError(e =>
                Left(Unexpected(s"${e.getMessage}, caused by: ${
                        if e.getCause != null then e.getCause.getMessage else "N/A"
                    }"))
            )

    def getStartupParams: IO[Either[Error, ProtocolParams]] =
        (for p <- provider
        yield Right(p.cardanoInfo.protocolParams))
            .handleError(e =>
                Left(Unexpected(s"${e.getMessage}, caused by: ${
                        if e.getCause != null then e.getCause.getMessage else "N/A"
                    }"))
            )

}

object CardanoBackendBlockfrost:

    // TODO: use uri from sttp?
    type URL = String
    type ApiKey = String

    def apply_(
        network: Either[StandardCardanoNetwork, (CardanoNetwork.Custom, URL)],
        apiKey: ApiKey = "",
        pageSize: Int = 100,
        tracer: ContraTracer[IO, CardanoBackendEvent]
    ): CardanoBackendBlockfrost = {
        // 1. BloxBean service
        // Strip any trailing slash so we never emit `…/api/v1//path`; Bloxbean wants exactly one.
        val baseUrl = network.fold(_.baseUrl, _._2).stripSuffix("/")
        val backendService = BFBackendService(s"$baseUrl/", apiKey)

        // 2. Scalus blockfrost provider, as a RETRYABLE IO rather than an eager Future: each
        //    evaluation starts a fresh fetch, so a failure is not memoised. See `provider`.
        val mkProvider: IO[BlockfrostProvider] = IO.defer(IO.fromFuture(IO.delay(network match {
            case Left(std) =>
                std match {
                    case CardanoNetwork.Mainnet =>
                        BlockfrostProvider.mainnet(apiKey)
                    case CardanoNetwork.Preprod =>
                        BlockfrostProvider.preprod(apiKey)
                    case CardanoNetwork.Preview =>
                        BlockfrostProvider.preview(apiKey)

                }
            case Right(custom, _) =>
                BlockfrostProvider.create(
                  apiKey = apiKey,
                  baseUrl = baseUrl,
                  network = custom.network,
                  slotConfig = custom.cardanoInfo.slotConfig
                )
        })))

        new CardanoBackendBlockfrost(
          backendService,
          pageSize,
          mkProvider,
          Ref.unsafe[IO, Option[BlockfrostProvider]](None),
          tracer,
          baseUrl,
          apiKey
        )
    }

    def apply(
        network: Either[StandardCardanoNetwork, (CardanoNetwork.Custom, URL)],
        apiKey: ApiKey = "",
        pageSize: Int = 100,
        tracer: ContraTracer[IO, CardanoBackendEvent]
    ): IO[CardanoBackendBlockfrost] =
        IO.delay(apply_(network, apiKey, pageSize, tracer))

    /** Build a backend for a [[CardanoNetwork]] directly: a standard network derives its own
      * Blockfrost URL; a `Custom` one uses `blockfrostApiUrl` (failing if it is absent). Folds the
      * network→selector resolution so callers need not thread the internal `Either` selector.
      */
    def apply(
        network: CardanoNetwork,
        blockfrostApiUrl: Option[URL],
        apiKey: ApiKey,
        tracer: ContraTracer[IO, CardanoBackendEvent]
    ): IO[CardanoBackendBlockfrost] =
        networkSelector(network, blockfrostApiUrl).flatMap(selector =>
            apply(selector, apiKey, tracer = tracer)
        )

    /** Resolve a [[CardanoNetwork]] + optional `blockfrostApiUrl` into the selector [[apply]]
      * expects. A standard network with no URL uses its own public Blockfrost endpoint; a standard
      * network *with* a URL is served from that private endpoint as a `Custom` over its baked-in
      * `CardanoInfo`, so its slot config and magic stay the standard chain's own. A `Custom`
      * network requires a URL and fails without one.
      */
    private[cardano] def networkSelector(
        network: CardanoNetwork,
        blockfrostApiUrl: Option[URL]
    ): IO[Either[StandardCardanoNetwork, (CardanoNetwork.Custom, URL)]] =
        network match {
            case standard: StandardCardanoNetwork =>
                blockfrostApiUrl match {
                    case None      => IO.pure(Left(standard))
                    case Some(url) =>
                        // A standard chain served by a private Blockfrost endpoint: keep its
                        // baked-in slot config and magic, but send queries and submissions to `url`.
                        val custom: CardanoNetwork.Custom =
                            CardanoNetwork.Custom(standard.cardanoInfo, standard.protocolMagic)
                        IO.pure(Right((custom, url)))
                }
            case custom: CardanoNetwork.Custom =>
                blockfrostApiUrl match {
                    case Some(url) => IO.pure(Right((custom, url)))
                    case None =>
                        IO.raiseError(
                          IllegalStateException(
                            "a Custom cardanoNetwork requires blockfrostApiUrl in the peer's " +
                                "private config"
                          )
                        )
                }
        }

    extension (self: StandardCardanoNetwork)
        def baseUrl: URL = self match {
            case _: CardanoNetwork.Mainnet.type => BlockfrostProvider.mainnetUrl
            case _: CardanoNetwork.Preprod.type => BlockfrostProvider.preprodUrl
            case _: CardanoNetwork.Preview.type => BlockfrostProvider.previewUrl
        }
