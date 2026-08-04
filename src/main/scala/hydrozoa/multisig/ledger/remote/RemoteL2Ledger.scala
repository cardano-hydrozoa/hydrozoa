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
import io.circe.parser.*
import io.circe.syntax.*
import io.circe.{Codec, DecodingFailure, Json}
import org.http4s.Uri
import org.http4s.client.websocket.{WSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient
import scala.concurrent.duration.*

/** A remote [[L2Ledger]] that drives a black-box ledger over one long-lived WebSocket connection,
  * one synchronous request/response at a time.
  *
  * The mutation path must not turn a transport failure into a per-request verdict: each peer drives
  * its own remote replica, so a peer-local connection blip that dropped a command would diverge
  * that peer's block from the others'. So a transport failure (connection loss, silent remote) is
  * **retried through, forever** (bounded exponential backoff) rather than surfaced — a request only
  * returns once the remote gives a real answer. Blind resend is safe because JointLedger stamps
  * each command with a monotonic command number and the remote caches its last response by it: a
  * re-sent command the remote already evaluated replays that cached response verbatim (the same
  * [[Response.Applied]] carrying the original effects), so it takes effect exactly once. A
  * permanently-unreachable ledger stalls this peer until the Cardano liaison's L1 fallback resolves
  * the head; there is no "unavailable" verdict.
  *
  * One shared JDK `HttpClient` ([[wsClient]]) is held for the ledger's lifetime — opening a fresh
  * client per request leaks a selector thread + file descriptors each time (reclaimed only on GC)
  * and exhausts the fd limit under load. The single connection is opened lazily, cached in
  * [[connRef]], reused across requests, and reopened after a drop. [[mutex]] serialises exchanges
  * so frames never interleave on the socket.
  *
  * @param wsUri
  *   The WebSocket URI of the remote ledger
  * @param wsClient
  *   The shared WebSocket client (one JDK `HttpClient`), reused across all connections
  * @param mutex
  *   Serialises request/response exchanges over the one shared connection
  * @param connRef
  *   The current connection and its finalizer (None when not connected)
  * @param requestTimeout
  *   Upper bound on one request/response exchange before the connection is dropped and retried
  * @param initialBackoff
  *   Backoff before the first reconnect attempt; doubles each attempt up to [[maxBackoff]]
  * @param maxBackoff
  *   Cap on the reconnect backoff
  */
class RemoteL2Ledger private (
    wsUri: Uri,
    wsClient: WSClient[IO],
    mutex: Mutex[IO],
    connRef: Ref[IO, Option[Conn]],
    requestTimeout: FiniteDuration,
    initialBackoff: FiniteDuration,
    maxBackoff: FiniteDuration,
    config: RemoteL2Ledger.Config,
    tracer: ContraTracer[IO, RemoteL2LedgerEvent]
) extends L2Ledger[IO] {
    import RemoteL2LedgerCodecs.given

    override implicit def monadF: Monad[IO] = Async[IO]

    given CardanoNetwork.Section = config.cardanoNetwork

    override def sendRegisterDeposit(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.RegisterDeposit
    ): EitherT[IO, L2LedgerError, Unit] =
        sendRequest(Request.RegisterDeposit(commandNumber, req)).map(_ => ())

    override def sendApplyDepositDecisions(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyDepositDecisions
    ): EitherT[IO, L2LedgerError, Vector[EvacuationDiff]] =
        // A remote `Rejected` for a decision → `Left` → JointLedger panics on it (a coordination
        // bug, not a soft reject); a `LedgerFreeze` → fail-stop in `sendRequest`.
        sendRequest(Request.ApplyDepositDecisions(commandNumber, req)).map(_._1)

    override def sendApplyTransaction(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyTransaction
    ): EitherT[IO, L2LedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])] =
        sendRequest(Request.ApplyTransaction(commandNumber, req))

    /** A no-op: the remote black box owns its own recovery, so there is nothing to co-anchor here.
      * It must not fail — `JointLedger.State.recover` treats a `Left` as fatal, so returning an
      * error would crash a remote-backed node at boot after its first block. A real desync between
      * the restored JointLedger command number and the remote's own position surfaces on the next
      * command as [[Response.OutOfOrder]], not here.
      */
    override def restoreTo(commandNumber: L2CommandNumber): EitherT[IO, L2LedgerError, Unit] =
        EitherT.rightT(())

    /** Send a request and interpret the remote's answer. Transport failure is retried through by
      * [[exchange]] and never seen here, so a returned response is always a real verdict:
      *   - [[Response.Applied]] → the command's effects (a lost-ack resend replays the remote's
      *     cached response, so it is applied exactly once);
      *   - [[Response.Rejected]] → `Left` (a deterministic verdict — JointLedger invalidates a
      *     `RegisterDeposit`/`ApplyTransaction`, and panics on a rejected `ApplyDepositDecisions`);
      *   - [[Response.OutOfOrder]], [[Response.LedgerFreeze]], an undecodable response, or a
      *     response for the wrong command number → a hard fail-stop, never a `Left`: turning a
      *     desync, a frozen ledger, or a protocol violation into a per-request verdict would
      *     diverge this peer.
      *
      * Every response echoes the command number it answers; a mismatch against the request is a
      * protocol violation (a stray/duplicated frame), so it fail-stops rather than being trusted.
      */
    private def sendRequest(
        request: Request
    ): EitherT[IO, L2LedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])] =
        EitherT {
            exchange(request).flatMap { text =>
                decode[Response](text) match {
                    case Left(err) =>
                        IO.raiseError(
                          L2LedgerError(
                            s"remote L2 ledger sent an undecodable response: ${err.getMessage}"
                          )
                        )
                    case Right(response) if response.commandNumber != request.commandNumber =>
                        IO.raiseError(
                          L2LedgerError(
                            s"remote L2 ledger answered command ${response.commandNumber} " +
                                s"but we sent ${request.commandNumber}"
                          )
                        )
                    case Right(Response.Applied(_, diffs, payouts)) =>
                        IO.pure(Right((diffs, payouts)))
                    case Right(Response.Rejected(_, message)) =>
                        IO.pure(Left(L2LedgerError(message)))
                    case Right(Response.OutOfOrder(_, expected)) =>
                        IO.raiseError(
                          L2LedgerError(
                            s"remote L2 ledger out of order: it expected command number $expected"
                          )
                        )
                    case Right(Response.LedgerFreeze(_, wrongDecision)) =>
                        IO.raiseError(
                          L2LedgerError(
                            s"remote L2 ledger is frozen (decision $wrongDecision could not be applied)"
                          )
                        )
                }
            }
        }

    /** Run one request/response exchange over the shared connection, retrying through transport
      * failure forever with bounded backoff. Serialised by [[mutex]]; on any failure the connection
      * is dropped and reopened on the next attempt. Returns the raw response text — decoding and
      * verdict interpretation happen in [[sendRequest]], so a malformed response is a protocol
      * failure there, not something retried here.
      *
      * The mutex is held across the *whole* retry loop, so a permanently-unreachable remote holds
      * it until the exchange is cancelled. That is intentional: JointLedger is the sole,
      * single-message-at-a-time driver of the mutation path (screening rides its own trait), so
      * there is no competing caller to starve — a stall correctly blocks only the one driver, which
      * has nothing else to do until its command lands.
      */
    private def exchange(request: Request): IO[String] = {
        val message = request.asJson.noSpaces
        mutex.lock.surround {
            def attempt(n: Int): IO[String] =
                acquire
                    .flatMap { conn =>
                        val once = for {
                            _ <- tracer.traceWith(Sending(message))
                            _ <- conn.connection.send(WSFrame.Text(message))
                            text <- conn.connection.receiveStream
                                .collect { case WSFrame.Text(t, _) => t }
                                .head
                                .compile
                                .lastOrError
                            _ <- tracer.traceWith(Received(text))
                        } yield text
                        once.timeout(requestTimeout).onError { case _ => drop(conn) }
                    }
                    .handleErrorWith { err =>
                        val wait = backoff(n)
                        tracer.traceWith(ConnectionError(n, wait, err)) >>
                            IO.sleep(wait) >>
                            attempt(n + 1)
                    }
            attempt(0)
        }
    }

    /** Reconnect backoff for `attempt` (zero-indexed): `initialBackoff` doubled per attempt, capped
      * at `maxBackoff`. The exponent is clamped so the shift can never overflow, and the base is
      * floored at 1ms so a sub-millisecond (or zero) `initialBackoff` can never degenerate into a
      * zero-delay reconnect storm.
      */
    private def backoff(attempt: Int): FiniteDuration = {
        val exp = math.min(attempt, 20)
        val base = initialBackoff.max(1.milli)
        (base.toNanos * (1L << exp)).nanos.min(maxBackoff)
    }

    /** The current connection, opening (and caching) a new one if none is live. */
    private def acquire: IO[Conn] =
        connRef.get.flatMap {
            case Some(conn) => IO.pure(conn)
            case None       => open
        }

    /** Open a connection, cache it, and trace the outcome. A failure to connect propagates so
      * [[exchange]] backs off and retries.
      */
    private def open: IO[Conn] =
        tracer.traceWith(Connecting(wsUri)) >>
            // Allocate-then-cache must be atomic w.r.t. cancellation: `poll` keeps the connect itself
            // interruptible, but once `allocated` yields the (connection, release) pair, caching the
            // release handle in `connRef` runs uninterruptibly. Otherwise a cancellation landing
            // between the fd opening and the cache would strand the release handle and leak the
            // connection — the exact fd leak this class exists to prevent.
            IO.uncancelable { poll =>
                poll(wsClient.connectHighLevel(WSRequest(wsUri)).allocated).flatMap {
                    case (connection, release) =>
                        val conn = Conn(connection, release)
                        connRef.set(Some(conn)) >> tracer.traceWith(Connected(wsUri)).as(conn)
                }
            }

    /** Discard a connection believed broken: clear it from the cache (if still current) and release
      * its resources, ignoring any close error.
      */
    private def drop(conn: Conn): IO[Unit] =
        connRef.update {
            case Some(current) if current eq conn => None
            case other                            => other
        } >> conn.release.attempt.void

}

object RemoteL2Ledger {
    type Config = CardanoNetwork.Section

    /** A live WebSocket connection paired with the finalizer that closes it. */
    private final case class Conn(connection: WSConnectionHighLevel[IO], release: IO[Unit])

    /** Request types sent to the remote L2 ledger. Every request carries the Hydrozoa-assigned
      * command number.
      */
    sealed trait Request {
        def commandNumber: L2CommandNumber
    }

    object Request {
        final case class RegisterDeposit(
            commandNumber: L2CommandNumber,
            command: L2LedgerCommand.RegisterDeposit
        ) extends Request
        final case class ApplyDepositDecisions(
            commandNumber: L2CommandNumber,
            command: L2LedgerCommand.ApplyDepositDecisions
        ) extends Request
        final case class ApplyTransaction(
            commandNumber: L2CommandNumber,
            command: L2LedgerCommand.ApplyTransaction
        ) extends Request

        // Request codecs. Each request is a single-key object tagging the command variant, whose
        // value carries the Hydrozoa-assigned `commandNumber` and the `command` payload.
        given requestCodec: Codec[Request] = {
            import L2LedgerCommand.given

            def tagged(tag: String, commandNumber: L2CommandNumber, command: io.circe.Json): Json =
                Json.obj(
                  tag -> Json.obj(
                    "commandNumber" -> commandNumber.asJson,
                    "command" -> command
                  )
                )

            Codec.from(
              encodeA = {
                  case Request.RegisterDeposit(cn, command) =>
                      tagged("RegisterDeposit", cn, command.asJson)
                  case Request.ApplyDepositDecisions(cn, command) =>
                      tagged("ApplyDepositDecisions", cn, command.asJson)
                  case Request.ApplyTransaction(cn, command) =>
                      tagged("ApplyTransaction", cn, command.asJson)
              },
              decodeA = c =>
                  c.keys
                      .flatMap(_.headOption)
                      .toRight(
                        DecodingFailure("Request must have exactly one field", c.history)
                      )
                      .flatMap { tag =>
                          val body = c.downField(tag)
                          val cn = body.downField("commandNumber").as[L2CommandNumber]
                          val command = body.downField("command")
                          tag match {
                              case "RegisterDeposit" =>
                                  for {
                                      n <- cn
                                      cmd <- command.as[L2LedgerCommand.RegisterDeposit]
                                  } yield Request.RegisterDeposit(n, cmd)
                              case "ApplyDepositDecisions" =>
                                  for {
                                      n <- cn
                                      cmd <- command.as[L2LedgerCommand.ApplyDepositDecisions]
                                  } yield Request.ApplyDepositDecisions(n, cmd)
                              case "ApplyTransaction" =>
                                  for {
                                      n <- cn
                                      cmd <- command.as[L2LedgerCommand.ApplyTransaction]
                                  } yield Request.ApplyTransaction(n, cmd)
                              case other =>
                                  Left(DecodingFailure(s"Unknown request type: $other", c.history))
                          }
                      }
            )
        }

    }

    /** Response types received from the remote L2 ledger. The remote validates each command number
      * against its tip `T`: `== T + 1` is fresh ([[Response.Applied]] / [[Response.Rejected]]);
      * `== T` is a lost-ack resend, answered by replaying the cached last response verbatim;
      * anything else (`> T + 1` or `< T`) is [[Response.OutOfOrder]]. A decision the ledger could
      * not apply freezes it, and every command after that is answered [[Response.LedgerFreeze]].
      *
      * **Every response echoes the command number it answers**, so the client correlates it to the
      * request it sent and fail-stops on a mismatch (a stray/duplicated frame). Applied effects are
      * only what the answered command produces: none for `RegisterDeposit`, evacuation diffs for
      * `ApplyDepositDecisions`, diffs + payouts for `ApplyTransaction`.
      */
    sealed trait Response {
        def commandNumber: L2CommandNumber
    }

    object Response {

        /** The command was applied at `commandNumber`; the effects it produced follow (empty
          * vectors for a command that produces none). A lost-ack resend replays this from the
          * remote's cache, so the command is still applied exactly once.
          */
        final case class Applied(
            commandNumber: L2CommandNumber,
            evacuationDiffs: Vector[EvacuationDiff],
            payouts: Vector[Payout.Obligation]
        ) extends Response

        /** The command was deterministically rejected — a real verdict, not a transport failure.
          * The client invalidates a `RegisterDeposit`/`ApplyTransaction` and panics on a rejected
          * `ApplyDepositDecisions`.
          */
        final case class Rejected(commandNumber: L2CommandNumber, message: String) extends Response

        /** The command number is neither fresh nor the cached last — `> T + 1` (ahead of the
          * remote) or `< T` (behind it): a desync. `commandNumber` is what we sent; `expected` is
          * the number the remote wanted next (`T + 1`). The client fail-stops.
          */
        final case class OutOfOrder(commandNumber: L2CommandNumber, expected: L2CommandNumber)
            extends Response

        /** The ledger is **frozen** — it could not apply the `ApplyDepositDecisions` at
          * `wrongDecisionCommandNumber`, and answers this to every command until a `restoreTo`
          * rewinds past the freeze. The client fail-stops.
          */
        final case class LedgerFreeze(
            commandNumber: L2CommandNumber,
            wrongDecisionCommandNumber: L2CommandNumber
        ) extends Response
    }

    /** Create a RemoteL2Ledger as a [[Resource]] owning one shared WebSocket client for its whole
      * lifetime; the release step closes any open connection.
      *
      * The connection is opened lazily on first use, reused across requests, and reopened if it
      * drops. A transport failure is retried through forever (bounded backoff) — made safe by
      * command-number deduplication — rather than surfaced as an error.
      *
      * @param wsUri
      *   The WebSocket URI (e.g., "ws://localhost:9000/l2-ledger")
      * @param requestTimeout
      *   How long one exchange may take before the connection is dropped and retried (default 30s)
      * @param initialBackoff
      *   Backoff before the first reconnect attempt (default 1s), doubled per attempt
      * @param maxBackoff
      *   Cap on the reconnect backoff (default 30s)
      */
    def create(
        wsUri: String,
        config: Config,
        tracer: ContraTracer[IO, RemoteL2LedgerEvent],
        requestTimeout: FiniteDuration = 30.seconds,
        initialBackoff: FiniteDuration = 1.second,
        maxBackoff: FiniteDuration = 30.seconds,
    ): Resource[IO, RemoteL2Ledger] =
        for {
            uri <- Resource.eval(IO.fromEither(Uri.fromString(wsUri)))
            // One HttpClient for the whole lifetime — reused across every connection.
            wsClient <- Resource.eval(JdkWSClient.simple[IO])
            connRef <- Resource.make(Ref[IO].of(Option.empty[Conn]))(ref =>
                ref.get.flatMap(_.traverse_(_.release.attempt.void))
            )
            mutex <- Resource.eval(Mutex[IO])
        } yield new RemoteL2Ledger(
          uri,
          wsClient,
          mutex,
          connRef,
          requestTimeout,
          initialBackoff,
          maxBackoff,
          config,
          tracer
        )
}
