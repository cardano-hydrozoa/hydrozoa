package hydrozoa.multisig.ledger.remote

import cats.Monad
import cats.data.EitherT
import cats.effect.{Async, Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import fs2.Stream
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l2.{L2Ledger, L2LedgerCommand, L2LedgerError}
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.{Request, Response}
import io.circe.Decoder
import io.circe.parser.*
import io.circe.syntax.*
import java.util.UUID
import org.http4s.client.websocket.{WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration.*

/** A remote L2Ledger implementation that communicates with a black-box ledger over WebSocket.
  *
  * This implementation uses a single WebSocket connection to send requests and receive responses
  * from the remote ledger. Requests and responses are matched using correlation IDs.
  *
  * @param wsUri
  *   The WebSocket URI to connect to (e.g., ws://localhost:9000/l2-ledger)
  */
class RemoteL2Ledger private (
    wsUri: String,
    sendQueue: Ref[IO, Map[String, WSFrame]],
    pendingRequests: Ref[IO, Map[String, Deferred[IO, Either[L2LedgerError, Response]]]]
) extends L2Ledger[IO] {

    private given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

    override implicit def monadF: Monad[IO] = Async[IO]

    /** Send a request to the remote ledger and wait for the response */
    private def sendRequest[A](
        request: Request
    )(using decoder: Decoder[A]): EitherT[IO, L2LedgerError, A] = {
        import RemoteL2LedgerCodecs.given
        EitherT {
            for {
                // Create correlation ID and deferred for response
                correlationId <- IO(UUID.randomUUID().toString)
                deferred <- Deferred[IO, Either[L2LedgerError, Response]]

                // Register pending request
                _ <- pendingRequests.update(_ + (correlationId -> deferred))

                // Create WebSocket message
                message = Request.Envelope(correlationId, request).asJson.noSpaces
                frame = WSFrame.Text(message)

                // Queue the frame for sending
                _ <- sendQueue.update(_ + (correlationId -> frame))

                // Wait for response
                result <- deferred.get

                // Clean up
                _ <- pendingRequests.update(_ - correlationId)
                _ <- sendQueue.update(_ - correlationId)

                // Decode response
                decoded <- result match {
                    case Left(error) => IO.pure(Left(error))
                    case Right(response) =>
                        response match {
                            case Response.Success(data) =>
                                decode[A](data.noSpaces) match {
                                    case Left(err) =>
                                        IO.pure(
                                          Left(
                                            L2LedgerError(
                                              s"Failed to decode response: ${err.getMessage}".getBytes
                                            )
                                          )
                                        )
                                    case Right(value) =>
                                        IO.pure(Right(value))
                                }
                            case Response.Error(message) =>
                                IO.pure(Left(L2LedgerError(message.getBytes)))
                        }
                }
            } yield decoded
        }
    }

    override def sendRegisterDepositRequest(
        req: L2LedgerCommand.RegisterDepositRequest
    ): EitherT[IO, L2LedgerError, Unit] = {
        import RemoteL2LedgerCodecs.given
        sendRequest[Unit](Request.DepositRegistration(req))
    }

    override def sendApplyDepositDecisions(
        req: L2LedgerCommand.ApplyDepositDecisions
    ): EitherT[IO, L2LedgerError, Vector[EvacuationDiff]] = {
        import RemoteL2LedgerCodecs.given
        sendRequest[Vector[EvacuationDiff]](Request.DepositDecisions(req))
    }

    override def sendApplyTransactionRequest(
        req: L2LedgerCommand.ApplyTransactionRequest
    ): EitherT[IO, L2LedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])] = {
        import RemoteL2LedgerCodecs.given
        sendRequest[(Vector[EvacuationDiff], Vector[Payout.Obligation])](Request.L2Event(req))
    }
}

object RemoteL2Ledger {

    private given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

    /** Request types sent to the remote L2 ledger */
    sealed trait Request

    object Request {
        final case class Envelope(correlationId: String, request: Request)

        final case class DepositRegistration(event: L2LedgerCommand.RegisterDepositRequest)
            extends Request
        final case class DepositDecisions(event: L2LedgerCommand.ApplyDepositDecisions)
            extends Request
        final case class L2Event(event: L2LedgerCommand.ApplyTransactionRequest) extends Request
    }

    /** Response types received from the remote L2 ledger */
    sealed trait Response

    object Response {
        final case class Envelope(correlationId: String, response: Response)

        final case class Success(data: io.circe.Json) extends Response
        final case class Error(message: String) extends Response
    }

    /** Create a RemoteL2Ledger with a managed WebSocket connection
      *
      * @param wsUri
      *   The WebSocket URI (e.g., "ws://localhost:9000/l2-ledger")
      * @return
      *   A Resource managing the WebSocket connection and RemoteL2Ledger instance
      */
    def create(wsUri: String): Resource[IO, RemoteL2Ledger] = {
        import org.http4s.Uri

        for {
            sendQueue <- Resource.eval(Ref.of[IO, Map[String, WSFrame]](Map.empty))
            pendingRequests <- Resource.eval(
              Ref.of[IO, Map[String, Deferred[IO, Either[L2LedgerError, Response]]]](Map.empty)
            )
            wsClient <- Resource.eval(JdkWSClient.simple[IO])
            uri <- Resource.eval(IO.fromEither(Uri.fromString(wsUri)))
            conn <- wsClient.connectHighLevel(WSRequest(uri))
            _ <- Resource.eval(logger.info(s"Connected to WebSocket at $wsUri"))
            _ <- runWebSocketConnection(conn, sendQueue, pendingRequests)
        } yield new RemoteL2Ledger(wsUri, sendQueue, pendingRequests)
    }

    /** Run the WebSocket connection, handling send/receive loops */
    private def runWebSocketConnection(
        conn: WSConnectionHighLevel[IO],
        sendQueue: Ref[IO, Map[String, WSFrame]],
        pendingRequests: Ref[IO, Map[String, Deferred[IO, Either[L2LedgerError, Response]]]]
    ): Resource[IO, Unit] = {
        import RemoteL2LedgerCodecs.given

        val sendLoop: Stream[IO, Unit] = Stream
            .repeatEval {
                for {
                    queue <- sendQueue.get
                    // Clear the send queue after reading
                    _ <- sendQueue.set(Map.empty)
                } yield queue.values.toList
            }
            .flatMap(Stream.emits)
            .evalMap {
                case frame @ WSFrame.Text(_, _) =>
                    conn.send(frame) *> logger.debug("Sent WebSocket frame")
                case _ => IO.unit
            }
            .metered(10.millis) // Poll send queue every 10ms

        val receiveLoop: Stream[IO, Unit] = conn.receiveStream
            .collect { case WSFrame.Text(text, _) =>
                text
            }
            .evalMap { text =>
                for {
                    _ <- logger.debug(s"Received WebSocket message: $text")
                    _ <- decode[Response.Envelope](text) match {
                        case Left(err) =>
                            logger.error(s"Failed to decode WebSocket response: ${err.getMessage}")
                        case Right(envelope) =>
                            for {
                                pending <- pendingRequests.get
                                _ <- pending.get(envelope.correlationId) match {
                                    case Some(deferred) =>
                                        deferred.complete(Right(envelope.response)) *>
                                            logger.debug(
                                              s"Completed request with correlation ID: ${envelope.correlationId}"
                                            )
                                    case None =>
                                        logger.warn(
                                          s"Received response for unknown correlation ID: ${envelope.correlationId}"
                                        )
                                }
                            } yield ()
                    }
                } yield ()
            }

        // Run send and receive loops concurrently as a background resource
        Resource.eval(
          sendLoop.concurrently(receiveLoop).compile.drain.start.void
        )
    }
}
