package hydrozoa.multisig.server

import cats.effect.IO
import hydrozoa.multisig.consensus.EventSequencer
import hydrozoa.multisig.consensus.EventSequencer.{DepositRequest, L2TxRequest}
import hydrozoa.multisig.server.ApiResponse.{Error, EventSubmitted}
import hydrozoa.multisig.server.JsonCodecs.given
import io.circe.syntax.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.{EntityDecoder, HttpRoutes}

/** HTTP routes for the Hydrozoa server. These routes are what get called by the frontend (or a
  * proxy -- load-balancer, unified api).
  */
class HydrozoaRoutes(eventSequencer: EventSequencer.Handle) {
    import hydrozoa.multisig

    // Implicit decoders for request bodies
    implicit val l2TxRequestEntityDecoder: EntityDecoder[IO, L2TxRequest] = jsonOf[IO, L2TxRequest]
    implicit val depositRequestEntityDecoder: EntityDecoder[IO, DepositRequest] =
        jsonOf[IO, DepositRequest]

    val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

        // POST /api/l2/submit - Submit an L2 action
        case req @ POST -> Root / "api" / "l2" / "submit" =>
            val result: IO[org.http4s.Response[IO]] = for {
                l2TxRequest <- req.as[L2TxRequest]

                // Send synchronous request to EventSequencer and get back LedgerEventId
                eventId <- eventSequencer ?: l2TxRequest

                response = EventSubmitted(eventId)
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

                // Send synchronous request to EventSequencer and get back LedgerEventId
                eventId <- eventSequencer ?: depositRequest

                response = EventSubmitted(eventId)
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
    def apply(eventSequencer: EventSequencer.Handle): HydrozoaRoutes =
        new HydrozoaRoutes(eventSequencer)
}
