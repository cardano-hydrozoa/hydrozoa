package hydrozoa.multisig.ledger.remote

import cats.Monad
import cats.data.EitherT
import cats.effect.{Async, IO, Temporal}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2Ledger, L2LedgerCommand, L2LedgerError}
import hydrozoa.multisig.ledger.remote
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.{Request, Response}
import hydrozoa.multisig.ledger.remote.RemoteL2LedgerEvent.*
import io.circe.Codec
import io.circe.parser.*
import io.circe.syntax.*
import org.http4s.Uri
import org.http4s.client.websocket.{WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient
import scala.concurrent.duration.*
import scalus.uplc.builtin.ByteString

/** A remote L2Ledger implementation that communicates with a black-box ledger over WebSocket.
  *
  * This implementation uses synchronous request-response communication: each request blocks until
  * the corresponding response is received from the remote ledger.
  *
  * Includes automatic reconnection logic with exponential backoff when the connection is lost.
  *
  * @param wsUri
  *   The WebSocket URI for reconnection
  * @param connRef
  *   A reference to the current WebSocket connection (None if not yet connected)
  * @param maxRetries
  *   Maximum number of reconnection attempts
  * @param initialBackoff
  *   Initial backoff duration between retries (default: 1 second)
  */
class RemoteL2Ledger private (
    wsUri: Uri,
    initialBackoff: FiniteDuration,
    maxBackoff: FiniteDuration,
    config: RemoteL2Ledger.Config,
    tracer: ContraTracer[IO, RemoteL2LedgerEvent]
) extends L2Ledger[IO] {
    import RemoteL2LedgerCodecs.given

    override implicit def monadF: Monad[IO] = Async[IO]

    given CardanoNetwork.Section = config.cardanoNetwork

    /** Execute an operation with automatic reconnection on failure.
      *
      * Opens a fresh connection per attempt via [[Resource.use]], ensuring the connection is always
      * closed regardless of outcome. Retries infinitely with exponential backoff capped at
      * maxBackoff.
      */
    private def withReconnect[A](
        operation: WSConnectionHighLevel[IO] => IO[A],
        attempt: Int = 0
    ): IO[A] = {
        val run = for {
            _ <- tracer.traceWith(Connecting(wsUri))
            wsClient <- JdkWSClient.simple[IO]
            result <- wsClient.connectHighLevel(WSRequest(wsUri)).use { conn =>
                tracer.traceWith(Connected(wsUri)) >> operation(conn)
            }
        } yield result

        run.handleErrorWith { error =>
            val calculatedBackoffSeconds = initialBackoff.toSeconds * Math.pow(2, attempt).toLong
            val backoffDuration =
                if calculatedBackoffSeconds.seconds > maxBackoff then maxBackoff
                else calculatedBackoffSeconds.seconds
            for {
                _ <- tracer.traceWith(ConnectionError(attempt, backoffDuration, error))
                _ <- Temporal[IO].sleep(backoffDuration)
                result <- withReconnect(operation, attempt + 1)
            } yield result
        }
    }

    /** Send a request to the remote ledger and wait for the synchronous response
      *
      * Connection errors are handled by withReconnect (infinite retry). Only L2 ledger errors
      * (decode failures, Response.Failure) are returned as Left(L2LedgerError).
      */
    private def sendRequest(
        request: Request
    ): EitherT[IO, L2LedgerError, Response.Success] = {
        EitherT {
            withReconnect { conn =>
                for {
                    // Send request
                    message <- IO.pure(request.asJson.noSpaces)
                    _ <- tracer.traceWith(Sending(message))
                    _ <- conn.send(WSFrame.Text(message))

                    // Wait for response (synchronous)
                    responseText <- conn.receiveStream
                        .collect { case WSFrame.Text(text, _) => text }
                        .head
                        .compile
                        .lastOrError

                    _ <- tracer.traceWith(Received(responseText))

                    // Decode response
                    ret <- decode[Response](responseText) match {
                        case Left(err) =>
                            IO.pure(
                              Left(
                                L2LedgerError(
                                  s"Failed to decode response: ${err.getMessage}"
                                )
                              )
                            )
                        case Right(response) =>
                            response match {
                                case s: Response.Success => IO.pure(Right(s))
                                case f: Response.Failure =>
                                    IO.pure(
                                      Left(L2LedgerError(s"Internal L2 failure: ${f.message}"))
                                    )
                            }
                    }
                } yield ret
            }
        }
    }

    override def sendRegisterDeposit(
        req: L2LedgerCommand.RegisterDeposit
    ): EitherT[IO, L2LedgerError, Unit] = {
        sendRequest(Request.RegisterDeposit(req)).map(_ => ())
    }

    override def sendApplyDepositDecisions(
        req: L2LedgerCommand.ApplyDepositDecisions
    ): EitherT[IO, L2LedgerError, Vector[EvacuationDiff]] = {
        sendRequest(Request.ApplyDepositDecisions(req)).map(s => s.evacuationDiffs)
    }

    override def sendApplyTransaction(
        req: L2LedgerCommand.ApplyTransaction
    ): EitherT[IO, L2LedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])] = {
        sendRequest(Request.ApplyTransaction(req)).map(s => (s.evacuationDiffs, s.payouts))
    }

    /** Passthrough for now: the remote ledger accepts every request and does its own screening at
      * submission. A dedicated remote screening endpoint (§9 item 3) replaces this so a remote node
      * also rejects pre-RequestId.
      */
    override def sendScreenTx(l2Payload: ByteString): EitherT[IO, L2LedgerError, Unit] =
        EitherT.rightT[IO, L2LedgerError](())

    /** Passthrough for now — see [[sendScreenTx]]. */
    override def sendScreenDeposit(
        req: L2LedgerCommand.ScreenDeposit
    ): EitherT[IO, L2LedgerError, Unit] =
        EitherT.rightT[IO, L2LedgerError](())

    /** The remote ledger owns its own persistence + recovery behind the WebSocket, so the host does
      * not track its commandNumber (R2b is the EUTXO reference ledger only); always report
      * [[L2CommandNumber.zero]].
      */
    override def currentCommandNumber: IO[L2CommandNumber] =
        IO.pure(L2CommandNumber.zero)

    /** Unsupported — see [[currentCommandNumber]]. */
    override def restoreTo(commandNumber: L2CommandNumber): EitherT[IO, L2LedgerError, Unit] =
        EitherT.leftT(L2LedgerError("restoreTo is not supported by RemoteL2Ledger"))

}

object RemoteL2Ledger {
    type Config = CardanoNetwork.Section

    /** Request types sent to the remote L2 ledger */
    sealed trait Request

    object Request {
        final case class RegisterDeposit(command: L2LedgerCommand.RegisterDeposit) extends Request
        final case class ApplyDepositDecisions(command: L2LedgerCommand.ApplyDepositDecisions)
            extends Request
        final case class ApplyTransaction(command: L2LedgerCommand.ApplyTransaction) extends Request

        // Request codecs
        given requestCodec: Codec[Request] = {
            import L2LedgerCommand.given

            Codec.from(
              encodeA = {
                  case Request.RegisterDeposit(event) =>
                      io.circe.Json.obj("RegisterDeposit" -> event.asJson)
                  case Request.ApplyDepositDecisions(event) =>
                      io.circe.Json.obj("ApplyDepositDecisions" -> event.asJson)
                  case Request.ApplyTransaction(event) =>
                      io.circe.Json.obj("ApplyTransaction" -> event.asJson)
              },
              decodeA = c =>
                  c.keys
                      .flatMap(_.headOption)
                      .toRight(
                        io.circe.DecodingFailure("Request must have exactly one field", c.history)
                      )
                      .flatMap {
                          case "RegisterDepositRequest" =>
                              c.downField("RegisterDepositRequest")
                                  .as[L2LedgerCommand.RegisterDeposit]
                                  .map(Request.RegisterDeposit.apply)
                          case "ApplyDepositDecisions" =>
                              c.downField("ApplyDepositDecisions")
                                  .as[L2LedgerCommand.ApplyDepositDecisions]
                                  .map(Request.ApplyDepositDecisions.apply)
                          case "ApplyTransaction" =>
                              c.downField("ApplyTransaction")
                                  .as[L2LedgerCommand.ApplyTransaction]
                                  .map(Request.ApplyTransaction.apply)
                          case other =>
                              Left(
                                io.circe.DecodingFailure(s"Unknown request type: $other", c.history)
                              )
                      }
            )
        }

    }

    /** Response types received from the remote L2 ledger */
    sealed trait Response

    object Response {

        final case class Success(
            evacuationDiffs: Vector[EvacuationDiff],
            payouts: Vector[Payout.Obligation]
        ) extends Response

        final case class Failure(message: String) extends Response
    }

    /** Create a RemoteL2Ledger with automatic connection and reconnection management
      *
      * The connection is established lazily on first use and automatically reconnected if lost.
      * Connection attempts retry infinitely with exponential backoff capped at maxBackoff.
      * {policyIdHex}.{assetNameHex}
      * @param wsUri
      *   The WebSocket URI (e.g., "ws://localhost:9000/l2-ledger")
      * @param initialBackoff
      *   Initial backoff duration between retries (default: 1 second)
      * @param maxBackoff
      *   Maximum backoff duration cap for exponential backoff (default: 30 seconds)
      * @return
      *   A RemoteL2Ledger instance that manages its own connections
      */
    def create(
        wsUri: String,
        config: Config,
        tracer: ContraTracer[IO, RemoteL2LedgerEvent],
        initialBackoff: FiniteDuration = 1.second,
        maxBackoff: FiniteDuration = 30.seconds,
    ): IO[RemoteL2Ledger] =
        IO.fromEither(Uri.fromString(wsUri))
            .map(uri => new RemoteL2Ledger(uri, initialBackoff, maxBackoff, config, tracer))
}
