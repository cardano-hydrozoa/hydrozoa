package hydrozoa.multisig.server

import cats.effect.{IO, Ref}
import hydrozoa.config.head.HeadConfig
import hydrozoa.multisig.consensus.EventSequencer
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.server.ApiResponse.{Error, HeadInfo, RequestAccepted}
import hydrozoa.multisig.server.JsonCodecs.given
import io.circe.syntax.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.{EntityDecoder, HttpRoutes}

/** HTTP routes for the Hydrozoa server. These routes are what get called by the frontend (or a
  * proxy -- load-balancer, unified api).
  *
  * Note: EventSequencer integration is currently commented out. Requests are accepted but not yet
  * processed.
  */
class HydrozoaRoutes(
    @annotation.unused eventSequencer: EventSequencer.Handle,
    headConfig: HeadConfig,
    eventCounter: Ref[IO, Int]
) {

    // Implicit decoders for request bodies
    implicit val depositRequestEntityDecoder: EntityDecoder[IO, UserRequest] =
        jsonOf[IO, UserRequest]

    /** Generate next RequestId for stub responses using incrementing counter */
    private def nextRequestId: IO[RequestId] =
        eventCounter.getAndUpdate(_ + 1).map { eventNum =>
            RequestId(
              peerNum = HeadPeerNumber.zero,
              requestNum = RequestNumber(eventNum)
            )
        }

    val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

        // POST /api/l2/submit - Submit an L2 transaction
        case req @ POST -> Root / "api" / "l2" / "submit" =>
            val result: IO[org.http4s.Response[IO]] = for {
                transactionRequest <- req.as[UserRequest]

                // TODO: Process TransactionRequest
                // Send synchronous request to EventSequencer and get back RequestId
                // eventId <- eventSequencer ?: l2TxRequest

                // For now, generate next event ID and return RequestAccepted
                eventId <- nextRequestId
                response = RequestAccepted(requestId = eventId)
                resp <- Ok(response.asJson)
            } yield resp

            result.handleErrorWith { error =>
                BadRequest(
                  Error(
                    error = error.getMessage
                  ).asJson
                )
            }

        // POST /api/deposit/register - Register a deposit
        case req @ POST -> Root / "api" / "deposit" / "register" =>
            val result: IO[org.http4s.Response[IO]] = for {
                depositRequest <- req.as[UserRequest]

                // TODO: Process DepositRequest
                // Send synchronous request to EventSequencer and get back RequestId
                // eventId <- eventSequencer ?: depositRequest

                // For now, generate next event ID and return RequestAccepted
                requestId <- nextRequestId
                response = RequestAccepted(requestId)
                resp <- Ok(response.asJson)
            } yield resp

            result.handleErrorWith { error =>
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
                    submissionDurationSeconds =
                        headConfig.txTiming.depositSubmissionDuration.finiteDuration.toSeconds,
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
    ): IO[HydrozoaRoutes] =
        Ref.of[IO, Int](0).map { counter =>
            new HydrozoaRoutes(eventSequencer, headConfig, counter)
        }
}
