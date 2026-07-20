package hydrozoa.multisig.server

import cats.effect.IO
import cats.syntax.traverse.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.NodeStatus
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{BlockWeaver, RequestSequencer}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.l2.EutxoL2LedgerReader
import hydrozoa.multisig.persistence.ConsensusStoreReader
import hydrozoa.multisig.server.ApiDto.*
import hydrozoa.multisig.server.HydrozoaHttpEvent.*
import hydrozoa.multisig.server.JsonCodecs.UserRequestDecoder
import hydrozoa.multisig.server.TapirJson.*
import io.circe.Json
import io.circe.syntax.*
import java.time.Instant
import org.http4s.HttpRoutes
import scala.util.Try
import scalus.cardano.address.Address
import sttp.apispec.openapi.circe.yaml.*
import sttp.model.StatusCode
import sttp.model.headers.WWWAuthenticateChallenge
import sttp.tapir.*
import sttp.tapir.docs.openapi.{OpenAPIDocsInterpreter, OpenAPIDocsOptions}
import sttp.tapir.generic.auto.*
import sttp.tapir.model.UsernamePassword
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter

/** The Hydrozoa node's user-facing HTTP API, described as tapir endpoint values.
  *
  * Endpoints-as-data is what makes the OpenAPI schema (and Swagger UI) genuinely *generated* from
  * the same definitions that serve traffic — there is one source of truth, so the docs cannot drift
  * from the routes. The tapir http4s interpreter stays inside cats-effect: handlers are `IO`, the
  * result is a plain `HttpRoutes[IO]`, and it mounts on the same Ember server.
  */
class HydrozoaRoutes(
    requestSequencer: RequestSequencer.Handle,
    blockWeaver: BlockWeaver.Handle,
    nodeStatus: IO[NodeStatus],
    consensusReader: ConsensusStoreReader[IO],
    l2QueryReader: Option[EutxoL2LedgerReader[IO]],
    headConfig: HeadConfig,
    serverConfig: HydrozoaServer.Config,
    tracer: ContraTracer[IO, HydrozoaHttpEvent]
) {
    import HydrozoaRoutes.{apiTitle, apiVersion, l2ApiTitle}

    /** The block-0 header encoder and the brief codecs are `CardanoNetwork.Section`-dependent;
      * `headConfig` satisfies the section.
      */
    private given CardanoNetwork.Section = headConfig

    /** How many recent L2 transactions to return when `?count=` is omitted. */
    private val defaultRecentTxCount = 50

    /** The decoder for user requests (deposit registrations and L2 transactions). */
    private val userRequestDecoder: UserRequestDecoder = UserRequestDecoder()

    /** The shared error output: an HTTP status plus an `{ error }` body. */
    private val errorOut: EndpointOutput[(StatusCode, ErrorResponse)] =
        statusCode.and(jsonBody[ErrorResponse])

    private def fail(code: StatusCode, message: String): (StatusCode, ErrorResponse) =
        (code, ErrorResponse(message))

    /** The admin realm advertised on a `401` from the finalize endpoint. */
    private val adminChallenge: WWWAuthenticateChallenge =
        WWWAuthenticateChallenge.basic("Hydrozoa Admin")

    /** Finalize's error output — like [[errorOut]], but carrying an optional `WWW-Authenticate`
      * header so a `401` re-advertises the Basic challenge (as the old route did).
      */
    private val finalizeErrorOut: EndpointOutput[(StatusCode, Option[String], ErrorResponse)] =
        statusCode.and(header[Option[String]]("WWW-Authenticate")).and(jsonBody[ErrorResponse])

    // ---- Endpoint definitions (the single source of truth for routes + schema) ----

    private val submitEndpoint: ServerEndpoint[Any, IO] =
        endpoint.post
            .in("head" / "tx")
            .name("postHeadTx")
            .in(stringJsonBody)
            .out(jsonBody[RequestAcceptedResponse])
            .errorOut(errorOut)
            .description(
              "Submit an L2 transaction (a JSON UserRequest whose L2 payload is a native, " +
                  "self-authenticating Cardano transaction)."
            )
            .serverLogic(body => acceptUserRequest("POST /head/tx", body))

    private val registerDepositEndpoint: ServerEndpoint[Any, IO] =
        endpoint.post
            .in("head" / "deposit")
            .name("postHeadDeposit")
            .in(stringJsonBody)
            .out(jsonBody[RequestAcceptedResponse])
            .errorOut(errorOut)
            .description(
              "Register an L1 deposit (a JSON UserRequest carrying the unsigned deposit tx " +
                  "CBOR and the serialized L2 outputs the deposit spawns on absorption)."
            )
            .serverLogic(body => acceptUserRequest("POST /head/deposit", body))

    private val headInfoEndpoint: ServerEndpoint[Any, IO] =
        endpoint.get
            .in("head" / "info")
            .name("getHeadInfo")
            .out(jsonBody[HeadInfoResponse])
            .errorOut(errorOut)
            .description("Head parameters plus the current node time.")
            .serverLogic(_ =>
                IO.realTimeInstant
                    .map(_.getEpochSecond)
                    .map(now => Right(ApiDto.mkHeadInfoResponse(headConfig, now)))
                    .handleError(err => Left(fail(StatusCode.InternalServerError, err.getMessage)))
            )

    /** `{block-number}` path segments decode to a validated [[BlockNumber]] — a malformed or
      * negative number is a 400 at decode, never a throw in a handler.
      */
    private given Codec[String, BlockNumber, CodecFormat.TextPlain] =
        Codec.int.mapDecode(i =>
            if i >= 0 then DecodeResult.Value(BlockNumber(i))
            else
                DecodeResult.Error(
                  i.toString,
                  new IllegalArgumentException("negative block number")
                )
        )(_.convert)

    /** `{request-id}` path segments decode to a validated [[RequestId]] from its opaque i64 form
      * (`asI64`, bits 40-47 = author peer, bits 0-39 = request number). Out of `[0, 2^48)` — where
      * `fromI64` would throw an out-of-range peer/number — is a 400 at decode, not a 500.
      */
    private given Codec[String, RequestId, CodecFormat.TextPlain] =
        Codec.long.mapDecode(n =>
            if n >= 0 && n < (1L << 48) then DecodeResult.Value(RequestId.fromI64(n))
            else
                DecodeResult.Error(
                  n.toString,
                  new IllegalArgumentException("request id out of range [0, 2^48)")
                )
        )(_.asI64)

    private val blocksEndpoint: ServerEndpoint[Any, IO] =
        endpoint.get
            .in("head" / "blocks")
            .name("getHeadBlocks")
            .out(jsonBody[List[BlockSummaryView]])
            .errorOut(errorOut)
            .description(
              "Every block of the head in block order — number, fast-cycle leader, and type. " +
                  "Block 0 is the head's initial block (config, no leader)."
            )
            .serverLogic(_ =>
                consensusReader.blockBriefs
                    .map(briefs =>
                        Right(
                          ApiDto.mkInitialBlockSummaryView ::
                              briefs.map(ApiDto.mkBlockSummaryView(_, headConfig.nHeadPeers))
                        )
                    )
                    .handleError(err => Left(fail(StatusCode.InternalServerError, err.getMessage)))
            )

    private val blockDetailsEndpoint: ServerEndpoint[Any, IO] =
        endpoint.get
            .in("head" / "blocks" / path[BlockNumber]("block-number"))
            .name("getHeadBlock")
            .out(jsonBody[BlockDetailsView])
            .errorOut(errorOut)
            .description(
              "One block's type, leader, and confirmation status: PROPOSED, SOFT (with this " +
                  "node's soft-confirmation time), or HARD (plus its hard-confirmation time)."
            )
            .serverLogic(num =>
                blockDetails(num)
                    .handleError(err => Left(fail(StatusCode.InternalServerError, err.getMessage)))
            )

    private val blockHeaderEndpoint: ServerEndpoint[Any, IO] =
        endpoint.get
            .in("head" / "blocks" / path[BlockNumber]("block-number") / "header")
            .name("getHeadBlockHeader")
            .out(jsonBody[Json])
            .errorOut(errorOut)
            .description(
              "The block's header content. The shape varies by block type (initial, minor, " +
                  "major, final)."
            )
            .serverLogic(num =>
                blockHeader(num)
                    .handleError(err => Left(fail(StatusCode.InternalServerError, err.getMessage)))
            )

    private val requestsEndpoint: ServerEndpoint[Any, IO] =
        endpoint.get
            .in("head" / "requests")
            .in(query[Option[String]]("type"))
            .in(query[Option[Int]]("peer_number"))
            .name("getHeadRequests")
            .out(jsonBody[List[RequestSummaryView]])
            .errorOut(errorOut)
            .description(
              "Requests the head has assigned an id — opaque request id, author peer number, and " +
                  "type (transaction or deposit). Filter with ?type=transaction|deposit and " +
                  "?peer_number=<n>."
            )
            .serverLogic((typeFilter, peerFilter) =>
                listRequests(typeFilter, peerFilter)
                    .handleError(err => Left(fail(StatusCode.InternalServerError, err.getMessage)))
            )

    private val requestDetailsEndpoint: ServerEndpoint[Any, IO] =
        endpoint.get
            .in("head" / "requests" / path[RequestId]("request-id"))
            .name("getHeadRequest")
            .out(jsonBody[RequestDetailsView])
            .errorOut(errorOut)
            .description(
              "One request's peer, type, receive time, and lifecycle status: UNPROCESSED, " +
                  "LOCALLY_PROCESSED (block + validity), SOFT_CONFIRMED (+ this node's " +
                  "soft-confirmation time), or HARD_CONFIRMED (+ its hard-confirmation time)."
            )
            .serverLogic(id =>
                requestDetails(id.peerNum, id.requestNum)
                    .handleError(err => Left(fail(StatusCode.InternalServerError, err.getMessage)))
            )

    // The two EUTXO L2-query endpoints are defined apart from their handlers so their schema
    // (openapi-eutxo-l2.yaml) can be generated even on a node with no reader (a remote-ledger node),
    // and the routes are mounted only when the EUTXO reader is present. See [[l2QueryReader]].

    private val l2UtxosDef
        : PublicEndpoint[String, (StatusCode, ErrorResponse), List[L2UtxoView], Any] =
        endpoint.get
            .in("l2" / "cardano-eutxo" / "utxos" / path[String]("address"))
            .name("getL2CardanoEutxoUtxos")
            .out(jsonBody[List[L2UtxoView]])
            .errorOut(errorOut)
            .description("Current L2 utxos controlled by a bech32 address (EUTXO ledger only).")

    private val l2TransactionsDef
        : PublicEndpoint[Option[Int], (StatusCode, ErrorResponse), List[L2TxSummaryView], Any] =
        endpoint.get
            .in("l2" / "cardano-eutxo" / "transactions")
            .name("getL2CardanoEutxoTransactions")
            .in(query[Option[Int]]("count"))
            .out(jsonBody[List[L2TxSummaryView]])
            .errorOut(errorOut)
            .description("Recent applied L2 transactions, newest first (EUTXO ledger only).")

    /** The EUTXO L2-query endpoints wired to `reader` — mounted only when the node runs the EUTXO
      * ledger.
      */
    private def l2ServerEndpoints(
        reader: EutxoL2LedgerReader[IO]
    ): List[ServerEndpoint[Any, IO]] =
        List(
          l2UtxosDef.serverLogic(address =>
              parseAddress(address) match {
                  case Left(message) => IO.pure(Left(fail(StatusCode.BadRequest, message)))
                  case Right(addr) =>
                      reader
                          .utxosByAddress(addr)
                          .map(utxos =>
                              Right(utxos.toList.map((in, out) => ApiDto.mkL2UtxoView(in, out)))
                          )
                          .handleError(err =>
                              Left(fail(StatusCode.InternalServerError, err.getMessage))
                          )
              }
          ),
          l2TransactionsDef.serverLogic(count =>
              reader
                  .recentTransactions(count.getOrElse(defaultRecentTxCount))
                  .map(summaries => Right(summaries.map(ApiDto.mkL2TxSummaryView).toList))
                  .handleError(err => Left(fail(StatusCode.InternalServerError, err.getMessage)))
          )
        )

    private val healthEndpoint: ServerEndpoint[Any, IO] =
        endpoint.get
            .in("health")
            .name("getHealth")
            .out(jsonBody[HealthResponse])
            .description("Liveness — always 200 while the process is serving HTTP.")
            .serverLogicSuccess(_ => IO.pure(HealthResponse("ok")))

    /** Readiness — `200` only while the head is [[NodeStatus.Active]] (open on L1); otherwise
      * `503`, always with the lifecycle status in the body, so a proxy routes user traffic here
      * only when the node can serve it. The verdict is the status code; the body is diagnostic.
      */
    private val readyEndpoint: ServerEndpoint[Any, IO] =
        endpoint.get
            .in("ready")
            .name("getReady")
            .out(statusCode.and(jsonBody[ReadinessResponse]))
            .description(
              "Readiness — 200 only while the head is Active (open on L1); 503 with the lifecycle " +
                  "status otherwise."
            )
            .serverLogicSuccess(_ =>
                nodeStatus.map { status =>
                    val code =
                        if status == NodeStatus.Active then StatusCode.Ok
                        else StatusCode.ServiceUnavailable
                    (code, ApiDto.mkReadinessResponse(status))
                }
            )

    private val finalizeEndpoint: ServerEndpoint[Any, IO] =
        endpoint.post
            // Optional credentials so the security logic runs (and logs) even when the header is
            // absent, rather than tapir short-circuiting the missing-header case.
            .securityIn(auth.basic[Option[UsernamePassword]](adminChallenge))
            .in("api" / "admin" / "finalize")
            .name("postAdminFinalize")
            .out(jsonBody[FinalizeResponse])
            .errorOut(finalizeErrorOut)
            .description("Trigger head finalization (admin only).")
            .serverSecurityLogic {
                case Some(credentials)
                    if credentials.username == serverConfig.adminUsername
                        && credentials.password.contains(serverConfig.adminPassword) =>
                    IO.pure(Right(()))
                case _ =>
                    tracer
                        .traceWith(UnauthorizedAdmin("POST /api/admin/finalize"))
                        .as(
                          Left(
                            (
                              StatusCode.Unauthorized,
                              Some(adminChallenge.toString),
                              ErrorResponse("Unauthorized")
                            )
                          )
                        )
            }
            .serverLogic(_ =>
                _ =>
                    (for {
                        _ <- tracer.traceWith(FinalizeTriggered)
                        _ <- blockWeaver ! BlockWeaver.LocalFinalizationTrigger.Triggered
                        _ <- tracer.traceWith(FinalizeSignalSent)
                    } yield Right(
                      FinalizeResponse("success", "Head finalization triggered")
                    )).handleErrorWith(err =>
                        tracer
                            .traceWith(RequestFailed("POST /api/admin/finalize", err))
                            .as(
                              Left(
                                (
                                  StatusCode.InternalServerError,
                                  None,
                                  ErrorResponse(err.getMessage)
                                )
                              )
                            )
                    )
            )

    /** The core API endpoints, in the order they appear in the docs — always served. */
    private val coreEndpoints: List[ServerEndpoint[Any, IO]] =
        List(
          submitEndpoint,
          registerDepositEndpoint,
          headInfoEndpoint,
          requestsEndpoint,
          requestDetailsEndpoint,
          blocksEndpoint,
          blockDetailsEndpoint,
          blockHeaderEndpoint,
          healthEndpoint,
          readyEndpoint,
          finalizeEndpoint
        )

    /** Don't auto-document a body/param decode-failure response: our JSON bodies are raw strings
      * whose codec never fails, so tapir's default would advertise an unreachable `text/plain` 400
      * that contradicts the real JSON `ErrorResponse`. The genuine errors are on each endpoint's
      * declared error output.
      */
    private val docsOptions: OpenAPIDocsOptions =
        OpenAPIDocsOptions.default.copy(defaultDecodeFailureOutput = _ => None)

    /** Swagger UI (served at `/docs`) + the raw OpenAPI document for the core API. */
    private val coreDocsEndpoints: List[ServerEndpoint[Any, IO]] =
        SwaggerInterpreter(openAPIInterpreterOptions = docsOptions)
            .fromServerEndpoints[IO](coreEndpoints, apiTitle, apiVersion)

    /** The EUTXO L2-query routes, mounted only when the node runs the EUTXO ledger. */
    private val l2Routes: List[ServerEndpoint[Any, IO]] =
        l2QueryReader.fold(List.empty)(l2ServerEndpoints)

    /** The full route set: the core API + its docs, plus the EUTXO L2-query routes when present. */
    val routes: HttpRoutes[IO] =
        Http4sServerInterpreter[IO]().toRoutes(coreEndpoints ++ coreDocsEndpoints ++ l2Routes)

    /** The core OpenAPI document as YAML — pinned by the golden test (docs/openapi.yaml). */
    def openApiYaml: String =
        OpenAPIDocsInterpreter(docsOptions)
            .toOpenAPI(coreEndpoints.map(_.endpoint), apiTitle, apiVersion)
            .toYaml

    /** The EUTXO L2-ledger OpenAPI document as YAML — pinned by the golden test
      * (docs/openapi-eutxo-l2.yaml). Generated from the endpoint definitions, so it exists whether
      * or not the L2 routes are mounted on this node.
      */
    def l2OpenApiYaml: String =
        OpenAPIDocsInterpreter(docsOptions)
            .toOpenAPI(List(l2UtxosDef, l2TransactionsDef), l2ApiTitle, apiVersion)
            .toYaml

    // ---- Handlers ----

    /** Parse a user request, forward it to the sequencer, and return the assigned id — preserving
      * the body / decode-error / failure tracing. Any failure is a 400.
      */
    private def acceptUserRequest(
        path: String,
        bodyText: String
    ): IO[Either[(StatusCode, ErrorResponse), RequestAcceptedResponse]] =
        val handled =
            for {
                _ <- tracer.traceWith(RequestBody(path, bodyText))
                json <- io.circe.parser.parse(bodyText) match {
                    case Left(parseError) =>
                        tracer.traceWith(JsonParseError(path, parseError)) *>
                            IO.raiseError(parseError)
                    case Right(json) => IO.pure(json)
                }
                userRequest <- userRequestDecoder.decodeJson(json) match {
                    case Left(decodeError) =>
                        tracer.traceWith(JsonDecodeError(path, decodeError)) *>
                            tracer.traceWith(
                              JsonDecodeErrorHistory(path, decodeError.history.toString)
                            ) *> IO.raiseError(decodeError)
                    case Right(request) => IO.pure(request)
                }
                _ <- tracer.traceWith(RequestDecoded(path, userRequest.toString))
                requestId <- (requestSequencer ?: userRequest).flatMap {
                    case Right(id) => IO.pure(id)
                    // A screen rejection surfaces as a 400 via handleErrorWith below.
                    case Left(rejected) => IO.raiseError(new RuntimeException(rejected.reason))
                }
            } yield ApiDto.mkRequestAcceptedResponse(requestId)

        handled
            .map(Right(_))
            .handleErrorWith(err =>
                tracer
                    .traceWith(RequestFailed(path, err))
                    .as(Left(fail(StatusCode.BadRequest, err.getMessage)))
            )

    /** The block-details body for `num`: block 0 is synthesized from the head config; any other
      * block resolves through its persisted brief, or a 404.
      */
    private def blockDetails(
        num: BlockNumber
    ): IO[Either[(StatusCode, ErrorResponse), BlockDetailsView]] =
        if num == BlockNumber.zero then
            blockConfirmation(num).map(confirmation =>
                val summary = ApiDto.mkInitialBlockSummaryView
                Right(
                  BlockDetailsView(summary.number, summary.leader, summary.blockType, confirmation)
                )
            )
        else
            consensusReader.blockBrief(num).flatMap {
                case None => IO.pure(Left(blockNotFound(num)))
                case Some(brief) =>
                    blockConfirmation(num).map(confirmation =>
                        val summary = ApiDto.mkBlockSummaryView(brief, headConfig.nHeadPeers)
                        Right(
                          BlockDetailsView(
                            summary.number,
                            summary.leader,
                            summary.blockType,
                            confirmation
                          )
                        )
                    )
            }

    /** The block's header as JSON: block 0's comes from the head config, the rest from their
      * persisted briefs.
      */
    private def blockHeader(num: BlockNumber): IO[Either[(StatusCode, ErrorResponse), Json]] =
        if num == BlockNumber.zero then
            IO.pure(Right(headConfig.initialBlock.blockBrief.header.asJson))
        else
            consensusReader.blockBrief(num).map {
                case None => Left(blockNotFound(num))
                case Some(brief) =>
                    Right(brief match {
                        case b: BlockBrief.Minor => b.header.asJson
                        case b: BlockBrief.Major => b.header.asJson
                        case b: BlockBrief.Final => b.header.asJson
                    })
            }

    /** Resolve a block's confirmation status from this node's records: the soft-confirmation
      * moment, and — through the block → stack index — the hard-confirmation moment.
      */
    private def blockConfirmation(num: BlockNumber): IO[BlockConfirmationView] =
        confirmationTimes(num).map((soft, hard) => ApiDto.mkBlockConfirmationView(soft, hard))

    /** This node's `(soft, hard)` confirmation moments for a block, as wall-clock instants derived
      * from the records' arrival stamps: the soft-confirmation record, and — through the block →
      * stack index — the hard-confirmation record.
      */
    private def confirmationTimes(num: BlockNumber): IO[(Option[Instant], Option[Instant])] =
        for {
            soft <- consensusReader.softConfirmation(num)
            stack <- consensusReader.stackOf(num)
            hard <- stack match {
                case None    => IO.pure(None)
                case Some(s) => consensusReader.hardConfirmation(s)
            }
            softAt <- soft.flatTraverse(t => consensusReader.wallClockOf(t.stamp))
            hardAt <- hard.flatTraverse(t => consensusReader.wallClockOf(t.stamp))
        } yield (softAt, hardAt)

    /** The request-details body for `(peer, num)`, or a 404 when the head has assigned no such id.
      * The lifecycle status resolves through the request → block index and the block's confirmation
      * records.
      */
    /** The request listing, optionally narrowed to one author peer (`peer_number`) and one type
      * (`transaction`/`deposit`). An unknown `type` value matches nothing (empty list); an
      * out-of-range `peer_number` likewise yields an empty list, since no such author exists.
      */
    private def listRequests(
        typeFilter: Option[String],
        peerFilter: Option[Int]
    ): IO[Either[(StatusCode, ErrorResponse), List[RequestSummaryView]]] =
        val peers = peerFilter match
            case None    => headConfig.headPeerNums.toList
            case Some(p) => headConfig.headPeerNums.toList.filter(_.convert == p)
        peers
            .traverse(consensusReader.requestsOf)
            .map { perPeer =>
                val rows = perPeer.flatten.map(t => ApiDto.mkRequestSummaryView(t.payload))
                Right(typeFilter.fold(rows)(t => rows.filter(_.requestType == t)))
            }

    private def requestDetails(
        peer: HeadPeerNumber,
        num: RequestNumber
    ): IO[Either[(StatusCode, ErrorResponse), RequestDetailsView]] =
        consensusReader.request(peer, num).flatMap {
            case None => IO.pure(Left(requestNotFound(peer, num)))
            case Some(stamped) =>
                for {
                    receivedAt <- consensusReader.wallClockOf(stamped.stamp)
                    processed <- consensusReader.requestBlock(peer, num)
                    confirmation <- processed match {
                        case None        => IO.pure((Option.empty[Instant], Option.empty[Instant]))
                        case Some(entry) => confirmationTimes(entry.blockNum)
                    }
                    (softAt, hardAt) = confirmation
                    status = ApiDto.mkRequestStatusView(
                      block = processed.map(e => (e.blockNum.convert, e.validity)),
                      softConfirmedAt = softAt,
                      hardConfirmedAt = hardAt
                    )
                } yield Right(ApiDto.mkRequestDetailsView(stamped.payload, receivedAt, status))
        }

    private def requestNotFound(
        peer: HeadPeerNumber,
        num: RequestNumber
    ): (StatusCode, ErrorResponse) =
        fail(
          StatusCode.NotFound,
          s"Request ${RequestId(peer, num).asI64} not found"
        )

    private def blockNotFound(num: BlockNumber): (StatusCode, ErrorResponse) =
        fail(StatusCode.NotFound, s"Block ${num.convert} not found")

    /** Parse a bech32 address, or a message describing why it is malformed. */
    private def parseAddress(bech32: String): Either[String, Address] =
        Try(Address.fromBech32(bech32)).toEither.left
            .map(err => s"Invalid bech32 address '$bech32': ${err.getMessage}")
}

object HydrozoaRoutes {
    val apiTitle: String = "Hydrozoa node API"
    val l2ApiTitle: String = "Hydrozoa EUTXO L2 ledger API"
    val apiVersion: String = "0.1.0"

    def apply(
        requestSequencer: RequestSequencer.Handle,
        blockWeaver: BlockWeaver.Handle,
        nodeStatus: IO[NodeStatus],
        consensusReader: ConsensusStoreReader[IO],
        l2QueryReader: Option[EutxoL2LedgerReader[IO]],
        headConfig: HeadConfig,
        serverConfig: HydrozoaServer.Config,
        tracer: ContraTracer[IO, HydrozoaHttpEvent]
    ): IO[HydrozoaRoutes] =
        IO.pure(
          new HydrozoaRoutes(
            requestSequencer,
            blockWeaver,
            nodeStatus,
            consensusReader,
            l2QueryReader,
            headConfig,
            serverConfig,
            tracer
          )
        )
}
