package hydrozoa.multisig.server

import cats.effect.IO
import hydrozoa.config.head.HeadConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.{BlockWeaver, RequestSequencer}
import hydrozoa.multisig.ledger.l2.EutxoL2LedgerReader
import hydrozoa.multisig.server.ApiDto.*
import hydrozoa.multisig.server.HydrozoaHttpEvent.*
import hydrozoa.multisig.server.JsonCodecs.UserRequestDecoder
import hydrozoa.multisig.server.TapirJson.*
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
    l2QueryReader: Option[EutxoL2LedgerReader[IO]],
    headConfig: HeadConfig,
    serverConfig: HydrozoaServer.Config,
    tracer: ContraTracer[IO, HydrozoaHttpEvent]
) {
    import HydrozoaRoutes.{apiTitle, apiVersion, l2ApiTitle}

    private given HeadConfig = headConfig

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
            .in("api" / "l2" / "submit")
            .in(stringJsonBody)
            .out(jsonBody[RequestAcceptedResponse])
            .errorOut(errorOut)
            .description(
              "Submit an L2 transaction (a JSON UserRequest whose L2 payload is a native, " +
                  "self-authenticating Cardano transaction)."
            )
            .serverLogic(body => acceptUserRequest("POST /api/l2/submit", body))

    private val registerDepositEndpoint: ServerEndpoint[Any, IO] =
        endpoint.post
            .in("api" / "deposit" / "register")
            .in(stringJsonBody)
            .out(jsonBody[RequestAcceptedResponse])
            .errorOut(errorOut)
            .description(
              "Register an L1 deposit (a JSON UserRequest whose L2 payload is a native, " +
                  "self-authenticating Cardano transaction)."
            )
            .serverLogic(body => acceptUserRequest("POST /api/deposit/register", body))

    private val headInfoEndpoint: ServerEndpoint[Any, IO] =
        endpoint.get
            .in("api" / "head-info")
            .out(jsonBody[HeadInfoResponse])
            .errorOut(errorOut)
            .description("Head parameters plus the current node time.")
            .serverLogic(_ =>
                IO.realTimeInstant
                    .map(_.getEpochSecond)
                    .map(now => Right(ApiDto.mkHeadInfoResponse(headConfig, now)))
                    .handleError(err => Left(fail(StatusCode.InternalServerError, err.getMessage)))
            )

    // The two EUTXO L2-query endpoints are defined apart from their handlers so their schema
    // (openapi-eutxo-l2.yaml) can be generated even on a node with no reader (a remote-ledger node),
    // and the routes are mounted only when the EUTXO reader is present. See [[l2QueryReader]].

    private val l2UtxosDef
        : PublicEndpoint[String, (StatusCode, ErrorResponse), List[L2UtxoView], Any] =
        endpoint.get
            .in("api" / "l2" / "utxos" / path[String]("address"))
            .out(jsonBody[List[L2UtxoView]])
            .errorOut(errorOut)
            .description("Current L2 utxos controlled by a bech32 address (EUTXO ledger only).")

    private val l2TransactionsDef
        : PublicEndpoint[Option[Int], (StatusCode, ErrorResponse), List[L2TxSummaryView], Any] =
        endpoint.get
            .in("api" / "l2" / "transactions")
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
            .out(jsonBody[HealthResponse])
            .description("Liveness — always 200 while the process is serving HTTP.")
            .serverLogicSuccess(_ => IO.pure(HealthResponse("ok")))

    private val finalizeEndpoint: ServerEndpoint[Any, IO] =
        endpoint.post
            // Optional credentials so the security logic runs (and logs) even when the header is
            // absent, rather than tapir short-circuiting the missing-header case.
            .securityIn(auth.basic[Option[UsernamePassword]](adminChallenge))
            .in("api" / "admin" / "finalize")
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
          healthEndpoint,
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
        l2QueryReader: Option[EutxoL2LedgerReader[IO]],
        headConfig: HeadConfig,
        serverConfig: HydrozoaServer.Config,
        tracer: ContraTracer[IO, HydrozoaHttpEvent]
    ): IO[HydrozoaRoutes] =
        IO.pure(
          new HydrozoaRoutes(
            requestSequencer,
            blockWeaver,
            l2QueryReader,
            headConfig,
            serverConfig,
            tracer
          )
        )
}
