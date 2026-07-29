package hydrozoa.multisig.ledger.remote

import cats.Monad
import cats.data.EitherT
import cats.effect.std.Mutex
import cats.effect.{Async, IO, Ref, Resource}
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2Ledger, L2LedgerCommand, L2LedgerError}
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.{Conn, Request, Response}
import hydrozoa.multisig.ledger.remote.RemoteL2LedgerEvent.*
import io.circe.Codec
import io.circe.parser.*
import io.circe.syntax.*
import org.http4s.Uri
import org.http4s.client.websocket.{WSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient
import scala.concurrent.duration.*
import scalus.uplc.builtin.ByteString

/** A remote L2Ledger implementation that talks to a black-box ledger over a single, long-lived
  * WebSocket connection, one synchronous request/response at a time.
  *
  * Every request shares one connection (one JDK `HttpClient`), serialized by [[mutex]] so
  * concurrent callers never interleave frames on the socket. Opening a fresh client + connection
  * per request — as an earlier version did — leaks a selector thread and file descriptors each time
  * (the JDK `HttpClient` is reclaimed only on GC), and exhausts the process's file-descriptor limit
  * under load. The connection is opened lazily on first use and transparently reopened once if it
  * has dropped; when the remote ledger cannot be reached, a request fails with an [[L2LedgerError]]
  * ("try again later") that flows through the normal error channel, rather than blocking forever or
  * crashing the node.
  *
  * @param wsUri
  *   The WebSocket URI of the remote ledger
  * @param wsClient
  *   The shared WebSocket client (one JDK `HttpClient`), reused across all connections
  * @param mutex
  *   Serializes request/response exchanges over the one shared connection
  * @param connRef
  *   The current connection and its finalizer (None when not connected)
  * @param requestTimeout
  *   Upper bound on one request/response exchange before the connection is dropped
  */
class RemoteL2Ledger private (
    wsUri: Uri,
    wsClient: WSClient[IO],
    mutex: Mutex[IO],
    connRef: Ref[IO, Option[Conn]],
    requestTimeout: FiniteDuration,
    config: RemoteL2Ledger.Config,
    tracer: ContraTracer[IO, RemoteL2LedgerEvent]
) extends L2Ledger[IO] {
    import RemoteL2LedgerCodecs.given

    override implicit def monadF: Monad[IO] = Async[IO]

    given CardanoNetwork.Section = config.cardanoNetwork

    /** Returned when the remote ledger is unreachable — a calm, retryable error, not a crash. */
    private val unavailable: L2LedgerError =
        L2LedgerError("remote L2 ledger unavailable, try again later")

    /** Run [[use]] against the shared connection, opening it if needed and reopening it once if the
      * first attempt fails on a broken connection. Serialized by [[mutex]] so concurrent callers
      * never interleave frames. A connection failure becomes `Left(unavailable)`; a successful
      * exchange is passed through untouched, so a valid response carrying an application-level
      * error is left for the caller to interpret (it is not mistaken for connection loss).
      */
    private def withConnection[A](
        use: WSConnectionHighLevel[IO] => IO[A]
    ): IO[Either[L2LedgerError, A]] =
        mutex.lock.surround {
            def go(reopened: Boolean): IO[Either[L2LedgerError, A]] =
                acquire.flatMap {
                    case Left(err) => IO.pure(Left(err))
                    case Right(conn) =>
                        use(conn.connection).attempt.flatMap {
                            case Right(a) => IO.pure(Right(a))
                            case Left(err) =>
                                drop(conn) >> {
                                    if reopened then
                                        tracer
                                            .traceWith(Unavailable(wsUri, err))
                                            .as(Left(unavailable))
                                    else go(reopened = true)
                                }
                        }
                }
            go(reopened = false)
        }

    /** The current connection, opening (and caching) a new one if none is live. */
    private def acquire: IO[Either[L2LedgerError, Conn]] =
        connRef.get.flatMap {
            case Some(conn) => IO.pure(Right(conn))
            case None       => open
        }

    /** Open a connection, cache it, and trace the outcome; a failure to connect is reported as
      * unavailable rather than raised.
      */
    private def open: IO[Either[L2LedgerError, Conn]] =
        tracer.traceWith(Connecting(wsUri)) >>
            wsClient.connectHighLevel(WSRequest(wsUri)).allocated.attempt.flatMap {
                case Right((connection, release)) =>
                    val conn = Conn(connection, release)
                    connRef.set(Some(conn)) >> tracer.traceWith(Connected(wsUri)).as(Right(conn))
                case Left(err) =>
                    tracer.traceWith(Unavailable(wsUri, err)).as(Left(unavailable))
            }

    /** Discard a connection believed broken: clear it from the cache (if still current) and release
      * its resources, ignoring any close error.
      */
    private def drop(conn: Conn): IO[Unit] =
        connRef.update {
            case Some(current) if current eq conn => None
            case other                            => other
        } >> conn.release.attempt.void

    /** Send a request to the remote ledger and wait for the synchronous response.
      *
      * Connection loss surfaces as `Left(unavailable)` via [[withConnection]]. Response-level
      * errors (decode failures, `Response.Failure`) come back as `Left(L2LedgerError)` too, but
      * leave the connection intact.
      */
    private def sendRequest(
        request: Request
    ): EitherT[IO, L2LedgerError, Response.Success] =
        EitherT {
            withConnection { conn =>
                val message = request.asJson.noSpaces
                val exchange = for {
                    _ <- tracer.traceWith(Sending(message))
                    _ <- conn.send(WSFrame.Text(message))
                    responseText <- conn.receiveStream
                        .collect { case WSFrame.Text(text, _) => text }
                        .head
                        .compile
                        .lastOrError
                    _ <- tracer.traceWith(Received(responseText))
                } yield responseText
                // Bound the exchange so a silent remote can't wedge the shared connection (and, with
                // it, every other request): on timeout the exchange fails, the connection is dropped,
                // and the request comes back as unavailable.
                exchange.timeout(requestTimeout)
            }.map {
                case Left(err) => Left(err)
                case Right(responseText) =>
                    decode[Response](responseText) match {
                        case Left(err) =>
                            Left(L2LedgerError(s"Failed to decode response: ${err.getMessage}"))
                        case Right(s: Response.Success) => Right(s)
                        case Right(f: Response.Failure) =>
                            Left(L2LedgerError(s"Internal L2 failure: ${f.message}"))
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
      * submission. A dedicated remote screening endpoint (docs/l2-isomorphism.md, Limitations)
      * replaces this so a remote node also rejects pre-RequestId.
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

    /** A live WebSocket connection paired with the finalizer that closes it. */
    private final case class Conn(connection: WSConnectionHighLevel[IO], release: IO[Unit])

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

    /** Create a RemoteL2Ledger as a [[Resource]] that owns one shared WebSocket client for its
      * whole lifetime; the release step closes any open connection.
      *
      * The connection is opened lazily on first use, reused across all requests, and transparently
      * reopened if it drops. When the remote ledger is unreachable, requests fail with a retryable
      * [[L2LedgerError]] instead of blocking or crashing.
      *
      * @param wsUri
      *   The WebSocket URI (e.g., "ws://localhost:9000/l2-ledger")
      * @param requestTimeout
      *   How long one request/response exchange may take before the connection is dropped and the
      *   request reported unavailable (default: 30 seconds)
      * @return
      *   A RemoteL2Ledger scoped to the returned Resource
      */
    def create(
        wsUri: String,
        config: Config,
        tracer: ContraTracer[IO, RemoteL2LedgerEvent],
        requestTimeout: FiniteDuration = 30.seconds,
    ): Resource[IO, RemoteL2Ledger] =
        for {
            uri <- Resource.eval(IO.fromEither(Uri.fromString(wsUri)))
            // One HttpClient for the whole lifetime — reused across every connection.
            wsClient <- Resource.eval(JdkWSClient.simple[IO])
            connRef <- Resource.make(Ref[IO].of(Option.empty[Conn]))(ref =>
                ref.get.flatMap(_.traverse_(_.release.attempt.void))
            )
            mutex <- Resource.eval(Mutex[IO])
        } yield new RemoteL2Ledger(uri, wsClient, mutex, connRef, requestTimeout, config, tracer)
}
