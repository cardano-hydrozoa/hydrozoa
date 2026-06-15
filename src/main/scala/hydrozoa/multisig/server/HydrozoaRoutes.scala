package hydrozoa.multisig.server

import cats.effect.IO
import fs2.Stream
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.{BlockWeaver, RequestSequencer, UserRequest}
import hydrozoa.multisig.server.ApiResponse.{CardanoNativeToken, Error, HeadInfo, RequestAccepted}
import hydrozoa.multisig.server.HydrozoaHttpEvent.*
import hydrozoa.multisig.server.JsonCodecs.{UserRequestDecoder, given}
import io.circe.syntax.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.headers.Authorization
import org.http4s.{BasicCredentials, EntityDecoder, HttpRoutes}

/** HTTP routes for the Hydrozoa server. These routes are what get called by the frontend (or a
  * proxy -- load-balancer, unified api).
  */
class HydrozoaRoutes(
    requestSequencer: RequestSequencer.Handle,
    blockWeaver: BlockWeaver.Handle,
    headConfig: HeadConfig,
    serverConfig: HydrozoaServer.Config,
    tracer: ContraTracer[IO, HydrozoaHttpEvent]
) {
    private given HeadConfig = headConfig

    /** Check if the provided credentials match the configured admin credentials */
    private def checkAuth(req: org.http4s.Request[IO]): Boolean =
        req.headers.get[Authorization] match {
            case Some(Authorization(BasicCredentials(username, password))) =>
                username == serverConfig.adminUsername && password == serverConfig.adminPassword
            case _ => false
        }

    // Implicit decoders for request bodies
    given depositRequestEntityDecoder(using
        CardanoNetwork.Section
    ): EntityDecoder[IO, UserRequest] =
        jsonOf[IO, UserRequest]

    private val userRequestDecoder: JsonCodecs.UserRequestDecoder = UserRequestDecoder()

    val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

        // POST /api/l2/submit - Submit an L2 transaction
        case req @ POST -> Root / "api" / "l2" / "submit" =>
            val path = "POST /api/l2/submit"
            val result: IO[org.http4s.Response[IO]] = for {
                bodyText <- req.bodyText.compile.string
                _ <- tracer.traceWith(RequestHeaders(path, req.headers.toString))
                _ <- tracer.traceWith(RequestBody(path, bodyText))
                // Try to parse as JSON to get better error messages
                _ <- io.circe.parser.parse(bodyText) match {
                    case Left(parseError) =>
                        tracer.traceWith(JsonParseError(path, parseError))
                    case Right(json) =>
                        userRequestDecoder.decodeJson(json) match {
                            case Left(decodeError) =>
                                tracer.traceWith(JsonDecodeError(path, decodeError)) *>
                                    tracer.traceWith(
                                      JsonDecodeErrorHistory(path, decodeError.history.toString)
                                    )
                            case Right(_) =>
                                IO.unit
                        }
                }
                // Re-create request with the body we just read
                newReq = req.withBodyStream(Stream.emits(bodyText.getBytes))
                transactionRequest <- newReq.as[UserRequest]
                _ <- tracer.traceWith(RequestDecoded(path, transactionRequest.toString))
                // Send synchronous request to RequestSequencer and get back RequestId
                requestId <- requestSequencer ?: transactionRequest
                response = RequestAccepted(requestId = requestId)
                resp <- Ok(response.asJson)
            } yield resp

            result.handleErrorWith { err =>
                tracer.traceWith(RequestFailed(path, err)) *>
                    BadRequest(Error(error = err.getMessage).asJson)
            }

        // POST /api/deposit/register - Register a deposit
        case req @ POST -> Root / "api" / "deposit" / "register" =>
            val path = "POST /api/deposit/register"
            val result: IO[org.http4s.Response[IO]] = for {
                bodyText <- req.bodyText.compile.string
                _ <- tracer.traceWith(RequestHeaders(path, req.headers.toString))
                _ <- tracer.traceWith(RequestBody(path, bodyText))
                _ <- io.circe.parser.parse(bodyText) match {
                    case Left(parseError) =>
                        tracer.traceWith(JsonParseError(path, parseError))
                    case Right(json) =>
                        io.circe.Decoder[UserRequest].decodeJson(json) match {
                            case Left(decodeError) =>
                                tracer.traceWith(JsonDecodeError(path, decodeError)) *>
                                    tracer.traceWith(
                                      JsonDecodeErrorHistory(path, decodeError.history.toString)
                                    )
                            case Right(_) =>
                                IO.unit
                        }
                }
                newReq = req.withBodyStream(Stream.emits(bodyText.getBytes))
                depositRequest <- newReq.as[UserRequest]
                _ <- tracer.traceWith(RequestDecoded(path, depositRequest.toString))
                requestId <- requestSequencer ?: depositRequest
                response = RequestAccepted(requestId)
                resp <- Ok(response.asJson)
            } yield resp

            result.handleErrorWith { err =>
                tracer.traceWith(RequestFailed(path, err)) *>
                    BadRequest(Error(error = err.getMessage).asJson)
            }

        // GET /api/head-info
        case GET -> Root / "api" / "head-info" =>
            val result: IO[org.http4s.Response[IO]] = for {
                currentTimePosixSeconds <- IO.realTimeInstant.map(_.getEpochSecond)
                resp <- Ok(
                  HeadInfo(
                    headId = headConfig.headId,
                    headAddress = headConfig.headMultisigAddress,
                    multisigRegimeUtxo = headConfig.multisigRegimeUtxo.input,
                    treasuryBeaconToken = CardanoNativeToken(
                      headConfig.headMultisigScript.policyId,
                      headConfig.headTokenNames.treasuryTokenName
                    ),
                    submissionDurationSeconds =
                        headConfig.txTiming.depositSubmissionDuration.finiteDuration.toSeconds,
                    absorptionStartOffsetSeconds =
                        headConfig.txTiming.absorptionStartOffsetDuration.finiteDuration.toSeconds,
                    refundStartOffsetSeconds =
                        headConfig.txTiming.refundStartOffsetDuration.finiteDuration.toSeconds,
                    currentTimePosixSeconds = currentTimePosixSeconds,
                    maxNonPlutusTxFee = headConfig.maxNonPlutusTxFee
                  ).asJson
                )
            } yield resp

            result.handleErrorWith { err =>
                InternalServerError(Error(error = err.getMessage).asJson)
            }

        // GET /health - Health check endpoint
        case GET -> Root / "health" =>
            Ok(io.circe.Json.obj("status" -> "ok".asJson))

        // POST /api/admin/finalize - Trigger head finalization (admin only)
        case req @ POST -> Root / "api" / "admin" / "finalize" =>
            val path = "POST /api/admin/finalize"
            if !checkAuth(req) then
                tracer.traceWith(UnauthorizedAdmin(path)) *>
                    Unauthorized(
                      org.http4s.headers.`WWW-Authenticate`(
                        org.http4s.Challenge("Basic", "Hydrozoa Admin")
                      )
                    )
            else
                val result: IO[org.http4s.Response[IO]] = for {
                    _ <- tracer.traceWith(FinalizeTriggered)
                    _ <- blockWeaver ! BlockWeaver.LocalFinalizationTrigger.Triggered
                    _ <- tracer.traceWith(FinalizeSignalSent)
                    resp <- Ok(
                      io.circe.Json.obj(
                        "status" -> "success".asJson,
                        "message" -> "Head finalization triggered".asJson
                      )
                    )
                } yield resp

                result.handleErrorWith { err =>
                    tracer.traceWith(RequestFailed(path, err)) *>
                        InternalServerError(Error(error = err.getMessage).asJson)
                }
    }
}

object HydrozoaRoutes {
    def apply(
        requestSequencer: RequestSequencer.Handle,
        blockWeaver: BlockWeaver.Handle,
        headConfig: HeadConfig,
        serverConfig: HydrozoaServer.Config,
        tracer: ContraTracer[IO, HydrozoaHttpEvent]
    ): IO[HydrozoaRoutes] =
        IO.pure(new HydrozoaRoutes(requestSequencer, blockWeaver, headConfig, serverConfig, tracer))
}
