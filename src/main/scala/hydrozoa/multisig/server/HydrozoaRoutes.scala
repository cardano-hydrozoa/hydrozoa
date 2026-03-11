package hydrozoa.multisig.server

import cats.effect.{IO, Ref}
import hydrozoa.multisig.consensus.EventSequencer
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.server.ApiResponse.{Error, RequestAccepted}
import hydrozoa.multisig.server.JsonCodecs.given
import hydrozoa.multisig.server.JsonCodecs.{DepositRequest, TransactionRequest}
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
    eventCounter: Ref[IO, Int]
) {

    // Implicit decoders for request bodies
    implicit val transactionRequestEntityDecoder: EntityDecoder[IO, TransactionRequest] =
        jsonOf[IO, TransactionRequest]
    implicit val depositRequestEntityDecoder: EntityDecoder[IO, DepositRequest] =
        jsonOf[IO, DepositRequest]

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
                transactionRequest <- req.as[TransactionRequest]

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
                depositRequest <- req.as[DepositRequest]

                // TODO: Process DepositRequest
                // Send synchronous request to EventSequencer and get back RequestId
                // eventId <- eventSequencer ?: depositRequest

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

        // GET /health - Health check endpoint
        case GET -> Root / "health" =>
            Ok(io.circe.Json.obj("status" -> "ok".asJson))
    }
}

object HydrozoaRoutes {
    def apply(eventSequencer: EventSequencer.Handle): IO[HydrozoaRoutes] =
        Ref.of[IO, Int](0).map { counter =>
            new HydrozoaRoutes(eventSequencer, counter)
        }
}
