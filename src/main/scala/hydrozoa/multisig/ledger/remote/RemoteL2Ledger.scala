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

    private given logger: Logger[IO] = Logging.loggerIO("RemoteL2Ledger")

    override implicit def monadF: Monad[IO] = Async[IO]

    /** Send a request to the remote ledger and wait for the synchronous response */
    private def sendRequest(
        request: Request
    ): EitherT[IO, L2LedgerError, Response.Success] = {
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
                                IO.pure(Left(L2LedgerError(s"Internal L2 failure: ${f.message}")))
                        }
                }
            } yield ret
        }
    }

    override def sendRegisterDeposit(
        req: L2LedgerCommand.RegisterDepositRequest
    ): EitherT[IO, L2LedgerError, Unit] = {
        sendRequest(Request.RegisterDepositRequest(req)).map(_ => ())
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

    private given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

    /** Request types sent to the remote L2 ledger */
    sealed trait Request

    object Request {
        final case class RegisterDepositRequest(command: L2LedgerCommand.RegisterDepositRequest)
            extends Request
        final case class ApplyDepositDecisions(command: L2LedgerCommand.ApplyDepositDecisions)
            extends Request
        final case class ApplyTransaction(event: L2LedgerCommand.ApplyTransaction) extends Request
        final case class ProxyBlockConfirmation(event: L2LedgerCommand.ProxyBlockConfirmation)
            extends Request
        final case class ProxyRequestError(event: L2LedgerCommand.ProxyRequestError) extends Request
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
