package hydrozoa.multisig.ledger.remote

import cats.Monad
import cats.data.EitherT
import cats.effect.{Async, IO, Resource}
import cats.syntax.all.*
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l2.{L2Ledger, L2LedgerCommand, L2LedgerError}
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.{Request, Response}
import io.circe.Decoder
import io.circe.parser.*
import io.circe.syntax.*
import org.http4s.client.websocket.{WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** A remote L2Ledger implementation that communicates with a black-box ledger over WebSocket.
  *
  * This implementation uses synchronous request-response communication: each request blocks until
  * the corresponding response is received from the remote ledger.
  *
  * @param conn
  *   The WebSocket connection
  */
class RemoteL2Ledger private (
    conn: WSConnectionHighLevel[IO]
) extends L2Ledger[IO] {

    import RemoteL2LedgerCodecs.given

    private given logger: Logger[IO] = Logging.loggerIO("RemoteL2Ledger")

    override implicit def monadF: Monad[IO] = Async[IO]

    /** Send a request to the remote ledger and wait for the synchronous response */
    private def sendRequest[A](
        request: Request
    )(using decoder: Decoder[A]): EitherT[IO, L2LedgerError, A] = {
        import RemoteL2LedgerCodecs.given
        EitherT {
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
                decoded <- decode[Response](responseText) match {
                    case Left(err) =>
                        IO.pure(
                          Left(
                            L2LedgerError(
                              s"Failed to decode response: ${err.getMessage}".getBytes
                            )
                          )
                        )
                    case Right(response) =>
                        response match {
                            case Response.Success(data) =>
                                decode[A](data.noSpaces) match {
                                    case Left(err) =>
                                        IO.pure(
                                          Left(
                                            L2LedgerError(
                                              s"Failed to decode response data: ${err.getMessage}".getBytes
                                            )
                                          )
                                        )
                                    case Right(value) =>
                                        IO.pure(Right(value))
                                }
                            case Response.Error(message) => for {
                                _ <- logger.warn(s"Error response: $message")
                            } yield Left(L2LedgerError(message.getBytes))
                        }
                }
            } yield decoded
        }
    }

    override def sendInitialize(
        req: L2LedgerCommand.Initialize
    ): EitherT[IO, L2LedgerError, Vector[EvacuationDiff]] = {
        sendRequest[Vector[EvacuationDiff]](Request.Initialize(req))
    }

    override def sendRegisterDeposit(
        req: L2LedgerCommand.RegisterDeposit
    ): EitherT[IO, L2LedgerError, Unit] = {
        sendRequest[Unit](Request.RegisterDeposit(req))
    }

    override def sendApplyDepositDecisions(
        req: L2LedgerCommand.ApplyDepositDecisions
    ): EitherT[IO, L2LedgerError, Vector[EvacuationDiff]] = {
        sendRequest[Vector[EvacuationDiff]](Request.ApplyDepositDecisions(req))
    }

    override def sendApplyTransaction(
        req: L2LedgerCommand.ApplyTransaction
    ): EitherT[IO, L2LedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])] = {
        sendRequest[(Vector[EvacuationDiff], Vector[Payout.Obligation])](Request.ApplyTransaction(req))
    }

    override def sendProxyBlockConfirmation(
        req: L2LedgerCommand.ProxyBlockConfirmation
    ): EitherT[IO, L2LedgerError, Unit] = {
        sendRequest[Unit](Request.ProxyBlockConfirmation(req))
    }

    override def sendProxyHydrozoaRequestError(
        req: L2LedgerCommand.ProxyRequestError
    ): EitherT[IO, L2LedgerError, Unit] = {
        sendRequest[Unit](Request.ProxyRequestError(req))
    }

}

object RemoteL2Ledger {

    private given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

    /** Request types sent to the remote L2 ledger */
    sealed trait Request

    object Request {
        final case class Initialize(command: L2LedgerCommand.Initialize) extends Request
        final case class RegisterDeposit(command: L2LedgerCommand.RegisterDeposit) extends Request
        final case class ApplyDepositDecisions(command: L2LedgerCommand.ApplyDepositDecisions)
            extends Request
        final case class ApplyTransaction(event: L2LedgerCommand.ApplyTransaction) extends Request
        final case class ProxyBlockConfirmation(event: L2LedgerCommand.ProxyBlockConfirmation)
            extends Request
        final case class ProxyRequestError(event: L2LedgerCommand.ProxyRequestError)
            extends Request
    }

    /** Response types received from the remote L2 ledger */
    sealed trait Response

    object Response {
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
            wsClient <- Resource.eval(JdkWSClient.simple[IO])
            uri <- Resource.eval(IO.fromEither(Uri.fromString(wsUri)))
            conn <- wsClient.connectHighLevel(WSRequest(uri))
            _ <- Resource.eval(logger.info(s"Connected to WebSocket at $wsUri"))
        } yield new RemoteL2Ledger(conn)
    }
}
