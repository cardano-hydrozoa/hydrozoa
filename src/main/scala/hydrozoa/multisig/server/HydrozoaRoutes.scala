package hydrozoa.multisig.server

import cats.effect.IO
import fs2.Stream
import hydrozoa.config.head.HeadConfig
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.{EventSequencer, UserRequest}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.server.ApiResponse.{CardanoNativeToken, Error, HeadInfo, RequestAccepted}
import hydrozoa.multisig.server.JsonCodecs.given
import io.circe.syntax.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.{EntityDecoder, HttpRoutes}
import org.typelevel.log4cats.Logger

/** HTTP routes for the Hydrozoa server. These routes are what get called by the frontend (or a
  * proxy -- load-balancer, unified api).
  */
class HydrozoaRoutes(
    eventSequencer: EventSequencer.Handle,
    headConfig: HeadConfig
) {

    private given logger: Logger[IO] = Logging.loggerIO("HydrozoaRoutes")

    // Implicit decoders for request bodies
    implicit val depositRequestEntityDecoder: EntityDecoder[IO, UserRequest] =
        jsonOf[IO, UserRequest]

    val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

        // POST /api/l2/submit - Submit an L2 transaction
        case req @ POST -> Root / "api" / "l2" / "submit" =>
            val result: IO[org.http4s.Response[IO]] = for {
                bodyText <- req.bodyText.compile.string
                _ <- logger.debug(s"POST /api/l2/submit - Headers: ${req.headers}")
                _ <- logger.debug(s"POST /api/l2/submit - Body: $bodyText")
                // Try to parse as JSON to get better error messages
                _ <- io.circe.parser.parse(bodyText) match {
                    case Left(parseError) =>
                        logger.error(
                          s"POST /api/l2/submit - JSON parse error: ${parseError.getMessage}"
                        )
                    case Right(json) =>
                        // Try to decode to UserRequest
                        io.circe.Decoder[UserRequest].decodeJson(json) match {
                            case Left(decodeError) =>
                                logger.error(
                                  s"POST /api/l2/submit - JSON decode error: ${decodeError.getMessage}"
                                ) *>
                                    logger.error(
                                      s"POST /api/l2/submit - Decode error history: ${decodeError.history}"
                                    )
                            case Right(_) =>
                                IO.unit
                        }
                }
                // Re-create request with the body we just read
                newReq = req.withBodyStream(Stream.emits(bodyText.getBytes))
                transactionRequest <- newReq.as[UserRequest]
                _ <- logger.debug(s"POST /api/l2/submit - Decoded: $transactionRequest")
                // Send synchronous request to EventSequencer and get back RequestId
                requestId <- eventSequencer ?: transactionRequest
                response = RequestAccepted(requestId = requestId)
                resp <- Ok(response.asJson)
            } yield resp

            result.handleErrorWith { error =>
                logger.error(error)(s"POST /api/l2/submit - Error: ${error.getMessage}") *>
                    BadRequest(
                      Error(
                        error = error.getMessage
                      ).asJson
                    )
            }

        // POST /api/deposit/register - Register a deposit
        case req @ POST -> Root / "api" / "deposit" / "register" =>
            val result: IO[org.http4s.Response[IO]] = for {
                bodyText <- req.bodyText.compile.string
                _ <- logger.debug(s"POST /api/deposit/register - Headers: ${req.headers}")
                _ <- logger.debug(s"POST /api/deposit/register - Body: $bodyText")
                // Try to parse as JSON to get better error messages
                _ <- io.circe.parser.parse(bodyText) match {
                    case Left(parseError) =>
                        logger.error(
                          s"POST /api/deposit/register - JSON parse error: ${parseError.getMessage}"
                        )
                    case Right(json) =>
                        // Try to decode to UserRequest
                        io.circe.Decoder[UserRequest].decodeJson(json) match {
                            case Left(decodeError) =>
                                logger.error(
                                  s"POST /api/deposit/register - JSON decode error: ${decodeError.getMessage}"
                                ) *>
                                    logger.error(
                                      s"POST /api/deposit/register - Decode error history: ${decodeError.history}"
                                    )
                            case Right(_) =>
                                IO.unit
                        }
                }
                // Re-create request with the body we just read
                newReq = req.withBodyStream(Stream.emits(bodyText.getBytes))
                depositRequest <- newReq.as[UserRequest]
                _ <- logger.debug(s"POST /api/deposit/register - Decoded: $depositRequest")
                // Send synchronous request to EventSequencer and get back RequestId
                requestId <- eventSequencer ?: depositRequest
                response = RequestAccepted(requestId)
                resp <- Ok(response.asJson)
            } yield resp

            result.handleErrorWith { error =>
                logger.error(error)(s"POST /api/deposit/register - Error: ${error.getMessage}") *>
                    BadRequest(
                      Error(
                        error = error.getMessage
                      ).asJson
                    )
            }

        // GET /api/head-info
        case GET -> Root / "api" / "head-info" =>
            val result: IO[org.http4s.Response[IO]] = for {
                currentTimePosixSeconds <- IO.realTimeInstant.map(_.getEpochSecond)
                resp <- Ok(
                  HeadInfo(
                    headId = headConfig.headId,
                    headAddress = headConfig.headMultisigAddress,
                    multisigRegimeUtxo = headConfig.multisigRegimeUtxo.utxoId,
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

            result.handleErrorWith { error =>
                InternalServerError(
                  Error(
                    error = error.getMessage
                  ).asJson
                )
            }

        // GET /health - Health check endpoint
        case GET -> Root / "health" =>
            Ok(io.circe.Json.obj("status" -> "ok".asJson))
    }
}

object HydrozoaRoutes {
    def apply(
        eventSequencer: EventSequencer.Handle,
        headConfig: HeadConfig,
    ): IO[HydrozoaRoutes] = IO.pure(new HydrozoaRoutes(eventSequencer, headConfig))
}
