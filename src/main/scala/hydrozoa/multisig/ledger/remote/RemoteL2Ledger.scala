package hydrozoa.multisig.ledger.remote

import cats.Monad
import cats.data.EitherT
import cats.effect.{Async, IO, Ref, Temporal}
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l2.{L2Ledger, L2LedgerCommand, L2LedgerError}
import hydrozoa.multisig.ledger.remote
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.{Request, Response}
import hydrozoa.multisig.ledger.remote.RemoteL2LedgerCodecs.{*, given}
import io.circe.parser.*
import io.circe.syntax.*
import org.http4s.Uri
import org.http4s.client.websocket.{WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient
import org.typelevel.log4cats.Logger
import scala.concurrent.duration.*

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
    connRef: Ref[IO, Option[WSConnectionHighLevel[IO]]],
    initialBackoff: FiniteDuration,
    maxBackoff: FiniteDuration,
    config: RemoteL2Ledger.Config
) extends L2Ledger[IO] {

    private given logger: Logger[IO] = Logging.loggerIO("RemoteL2Ledger")

    override implicit def monadF: Monad[IO] = Async[IO]

    given CardanoNetwork.Section = config.cardanoNetwork

    /** Send a request to the remote ledger and wait for the synchronous response */
    /** Establish a new WebSocket connection
      *
      * Creates a new WebSocket connection and updates the connection reference. Note: This doesn't
      * close the old connection - in a production environment you may want to track and close old
      * connections.
      */
    private def connect(): IO[WSConnectionHighLevel[IO]] = {
        for {
            _ <- logger.info(s"Connecting to WebSocket at $wsUri")
            wsClient <- JdkWSClient.simple[IO]
            newConn <- wsClient.connectHighLevel(WSRequest(wsUri)).allocated.map(_._1)
            _ <- connRef.set(Some(newConn))
            _ <- logger.info(s"Successfully connected to $wsUri")
        } yield newConn
    }

    /** Get or establish a connection */
    private def getOrConnect(): IO[WSConnectionHighLevel[IO]] = {
        connRef.get.flatMap {
            case Some(conn) => IO.pure(conn)
            case None       => connect()
        }
    }

    /** Execute an operation with automatic reconnection on failure
      *
      * Retries infinitely with exponential backoff capped at maxBackoff.
      *
      * @param operation
      *   The operation to execute
      * @param attempt
      *   Current attempt number (for exponential backoff)
      * @return
      *   Result of the operation
      */
    private def withReconnect[A](
        operation: WSConnectionHighLevel[IO] => IO[A],
        attempt: Int = 0
    ): IO[A] = {
        getOrConnect().flatMap(operation).handleErrorWith { error =>
            // Calculate backoff with maximum cap from config
            val calculatedBackoffSeconds = initialBackoff.toSeconds * Math.pow(2, attempt).toLong
            val backoffDuration =
                if calculatedBackoffSeconds.seconds > maxBackoff then maxBackoff
                else calculatedBackoffSeconds.seconds

            for {
                _ <- logger.warn(error)(
                  s"Connection error (attempt ${attempt + 1}), reconnecting after $backoffDuration"
                )
                _ <- Temporal[IO].sleep(backoffDuration)
                // Clear the old connection and try again
                _ <- connRef.set(None)
                _ <- connect().handleErrorWith { reconnectError =>
                    logger.debug(reconnectError)("Reconnection attempt failed, will retry") *>
                        IO.unit // Swallow the error, continue retrying
                }
                // Recursively retry - NO LIMIT
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
                    _ <- logger.debug(s"Sending request: $message")
                    _ <- conn.send(WSFrame.Text(message))

                    // Wait for response (synchronous)
                    responseText <- conn.receiveStream
                        .collect { case WSFrame.Text(text, _) => text }
                        .head
                        .compile
                        .lastOrError

                    _ <- logger.debug(s"Received response: $responseText")

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

    override def sendProxyBlockConfirmation(
        req: L2LedgerCommand.ProxyBlockConfirmation
    ): EitherT[IO, L2LedgerError, Unit] = {
        sendRequest(Request.ProxyBlockConfirmation(req)).map(_ => ())
    }

    override def sendProxyHydrozoaRequestError(
        req: L2LedgerCommand.ProxyRequestError
    ): EitherT[IO, L2LedgerError, Unit] = {
        sendRequest(Request.ProxyRequestError(req)).map(_ => ())
    }
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
        final case class ProxyBlockConfirmation(command: L2LedgerCommand.ProxyBlockConfirmation)
            extends Request
        final case class ProxyRequestError(command: L2LedgerCommand.ProxyRequestError)
            extends Request
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
        initialBackoff: FiniteDuration = 1.second,
        maxBackoff: FiniteDuration = 30.seconds,
        config: Config
    ): IO[RemoteL2Ledger] = {
        for {
            uri <- IO.fromEither(Uri.fromString(wsUri))
            connRef <- Ref.of[IO, Option[WSConnectionHighLevel[IO]]](None)
        } yield new RemoteL2Ledger(uri, connRef, initialBackoff, maxBackoff, config)
    }
}
