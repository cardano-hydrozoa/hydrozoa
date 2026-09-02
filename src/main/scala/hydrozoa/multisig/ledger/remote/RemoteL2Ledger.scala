package hydrozoa.multisig.ledger.remote

import cats.Monad
import cats.data.EitherT
import cats.effect.std.{Mutex, Queue}
import cats.effect.{Async, Deferred, FiberIO, IO, Ref, Resource}
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.QuietRelease
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.ledger.joint.EvacuationMapHash
import hydrozoa.multisig.ledger.l2.{ApplyDepositDecisionsResponse, ApplyTransactionResponse, L2CommandNumber, L2Ledger, L2LedgerCommand, L2LedgerResponse, RegisterDepositResponse, RestoreError}
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.{Conn, Request, RestoreResponse}
import hydrozoa.multisig.ledger.remote.RemoteL2LedgerEvent.*
import io.circe.parser.*
import io.circe.syntax.*
import java.util.concurrent.TimeoutException
import org.http4s.Uri
import org.http4s.client.websocket.{WSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient
import scala.concurrent.duration.*

/** A broken-transport failure from a [[RemoteL2Ledger]] — an undecodable frame, a command-number
  * echo mismatch, or a response whose `Applied`/`Rejected` variant does not match the command sent.
  * A protocol violation, not one of the four verdicts, so it fail-stops (a raise) rather than being
  * returned as a response.
  */
final case class RemoteL2LedgerError(message: String) extends RuntimeException(message)

/** Raised to break an in-flight exchange out of its retry loop when the node is shutting down.
  *
  * ⛔ This exists because a cats-actors message handler runs inside `ActorCell`'s `.uncancelable`
  * region, so an actor parked in an unbounded retry CANNOT be cancelled and the actor system can
  * never terminate — measured: SIGTERM did not shut the node down at all, and the process survived
  * only to be force-killed by cats-effect's `shutdownHookTimeout` (whose default is `Duration.Inf`,
  * i.e. never). The handler must therefore RETURN of its own accord, and this is how it does so.
  */
final case class RemoteL2LedgerShuttingDown(message: String) extends RuntimeException(message)

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
  * [[L2LedgerResponse.Applied]] carrying the original effects), so it takes effect exactly once. A
  * permanently-unreachable ledger stalls this peer until the Cardano liaison's L1 fallback resolves
  * the head; there is no "unavailable" verdict.
  *
  * One shared JDK `HttpClient` ([[wsClient]]) is held for the ledger's lifetime — opening a fresh
  * client per request leaks a selector thread + file descriptors each time (reclaimed only on GC)
  * and exhausts the fd limit under load. The single connection is opened lazily, cached in
  * [[connRef]], reused across requests, and reopened after a drop. [[mutex]] serialises exchanges
  * so frames never interleave on the socket.
  *
  * Each open connection runs a background fiber that continuously drains its `receiveStream` into a
  * per-connection queue; an exchange sends, then takes its response off that queue. Draining
  * without pause keeps the JDK WebSocket's read-demand (`request(n)`) outstanding at all times —
  * pulling `receiveStream.head` afresh per exchange instead can leave a delivered response unread
  * in the socket, stalling the (serialised) mutation path until something else nudges the
  * connection.
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
  * @param restoreTimeout
  *   Upper bound on a `Restore` exchange, which is not retried on expiry — see [[restoreTo]]
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
    restoreTimeout: FiniteDuration,
    initialBackoff: FiniteDuration,
    maxBackoff: FiniteDuration,
    handshakeBudget: FiniteDuration,
    stopping: Deferred[IO, Unit],
    config: RemoteL2Ledger.Config,
    tracer: ContraTracer[IO, RemoteL2LedgerEvent]
) extends L2Ledger[IO] {
    import RemoteL2LedgerCodecs.given

    override implicit def monadF: Monad[IO] = Async[IO]

    given CardanoNetwork.Section = config.cardanoNetwork

    override def registerDeposit(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.RegisterDeposit
    ): IO[RegisterDepositResponse] =
        sendRequest(Request.RegisterDeposit(commandNumber, req)).flatMap {
            case r: L2LedgerResponse.Applied.RegisterDeposit  => IO.pure(r)
            case r: L2LedgerResponse.Rejected.RegisterDeposit => IO.pure(r)
            case r: L2LedgerResponse.UnrecoverableError       => IO.pure(r)
            case other => unexpected("RegisterDeposit", other)
        }

    override def applyDepositDecisions(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyDepositDecisions
    ): IO[ApplyDepositDecisionsResponse] =
        sendRequest(Request.ApplyDepositDecisions(commandNumber, req)).flatMap {
            case r: L2LedgerResponse.Applied.ApplyDepositDecisions => IO.pure(r)
            case r: L2LedgerResponse.UnrecoverableError            => IO.pure(r)
            case other => unexpected("ApplyDepositDecisions", other)
        }

    override def applyTransaction(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyTransaction
    ): IO[ApplyTransactionResponse] =
        sendRequest(Request.ApplyTransaction(commandNumber, req)).flatMap {
            case r: L2LedgerResponse.Applied.ApplyTransaction  => IO.pure(r)
            case r: L2LedgerResponse.Rejected.ApplyTransaction => IO.pure(r)
            case r: L2LedgerResponse.UnrecoverableError        => IO.pure(r)
            case other => unexpected("ApplyTransaction", other)
        }

    /** A remote whose `Applied` variant does not match the command sent is a protocol violation
      * (not one of the four verdicts), so it fail-stops rather than being returned.
      */
    private def unexpected(command: String, response: L2LedgerResponse): IO[Nothing] =
        IO.raiseError(
          RemoteL2LedgerError(s"remote L2 ledger answered $response for a $command command")
        )

    /** Co-anchor the remote with the restored JointLedger command number: send a
      * [[Request.Restore]] so the remote rewinds its own command-number tip (and committed state)
      * to `commandNumber`. Without this the remote keeps its durable position, so the JointLedger
      * re-issues an already-applied command and the remote rejects it as
      * [[L2LedgerResponse.UnrecoverableError.OutOfOrder]] — the crash loop on recovery.
      *
      * Success maps to `Right(())`. A restore failure asking for a command number past the remote's
      * durable tip (`requested > tip`) maps to [[RestoreError.CommandNumberTooHigh]]; any other
      * failure to [[RestoreError.OtherError]]. Both are typed `Left`s, never thrown —
      * `JointLedger.State.recover` treats them as fatal. A broken *transport* or a protocol
      * violation still fail-stops (a raise) like the other requests.
      *
      * Bounded by [[restoreTimeout]], not the ordinary `requestTimeout`, and an expiry is **not**
      * retried — see that parameter for why.
      */
    override def restoreTo(
        commandNumber: L2CommandNumber
    ): EitherT[IO, RestoreError, EvacuationMapHash] =
        EitherT(sendRestoreRequest(Request.Restore(commandNumber)).map {
            case r: RestoreResponse.Restored => Right(r.evacuationMapHash)
            case RestoreResponse.RestoreFailed(requested, tip, reason) =>
                if requested.value > tip.value then
                    Left(RestoreError.CommandNumberTooHigh(requested, tip))
                else Left(RestoreError.OtherError(reason))
        })

    /** Send a [[Request.Restore]] and return the remote's [[RestoreResponse]]. Like
      * [[sendRequest]], transport failure is retried through by [[exchange]] and never seen here;
      * the IO fails only on a broken *transport* — an undecodable frame, or a response whose echoed
      * command number does not match the request. Those are protocol violations, not verdicts, so
      * they fail-stop.
      */
    private def sendRestoreRequest(request: Request.Restore): IO[RestoreResponse] =
        exchange(request, restoreTimeout, retryOnTimeout = false).flatMap { text =>
            decode[RestoreResponse](text) match {
                case Left(err) =>
                    IO.raiseError(
                      RemoteL2LedgerError(
                        s"remote L2 ledger sent an undecodable restore response: ${err.getMessage}"
                      )
                    )
                case Right(response) if response.commandNumber != request.commandNumber =>
                    IO.raiseError(
                      RemoteL2LedgerError(
                        s"remote L2 ledger answered restore command ${response.commandNumber} " +
                            s"but we sent ${request.commandNumber}"
                      )
                    )
                case Right(response) => IO.pure(response)
            }
        }

    /** Send a request and return the remote's total [[L2LedgerResponse]]. Transport failure is
      * retried through by [[exchange]] and never seen here, so a returned response is always a real
      * verdict — [[L2LedgerResponse.Applied]] / [[L2LedgerResponse.Rejected]] /
      * [[L2LedgerResponse.UnrecoverableError]] — which JointLedger interprets. The IO fails only on
      * a broken *transport*: an undecodable frame, or a response whose echoed command number does
      * not match the request (a stray/duplicated frame). Those are protocol violations, not
      * verdicts, so they fail-stop rather than being turned into a response.
      */
    private def sendRequest(request: Request): IO[L2LedgerResponse] =
        exchange(request, requestTimeout, retryOnTimeout = true).flatMap { text =>
            decode[L2LedgerResponse](text) match {
                case Left(err) =>
                    IO.raiseError(
                      RemoteL2LedgerError(
                        s"remote L2 ledger sent an undecodable response: ${err.getMessage}"
                      )
                    )
                case Right(response) if response.commandNumber != request.commandNumber =>
                    IO.raiseError(
                      RemoteL2LedgerError(
                        s"remote L2 ledger answered command ${response.commandNumber} " +
                            s"but we sent ${request.commandNumber}"
                      )
                    )
                case Right(response) => IO.pure(response)
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
    private def exchange(
        request: Request,
        timeout: FiniteDuration,
        retryOnTimeout: Boolean
    ): IO[String] = {
        val message = request.asJson.noSpaces
        mutex.lock.surround {
            def attempt(n: Int): IO[String] =
                acquire
                    .flatMap { conn =>
                        val once = for {
                            _ <- tracer.traceWith(Sending(message))
                            _ <- conn.connection.send(WSFrame.Text(message))
                            // The connection's background fiber keeps `receiveStream` drained into
                            // `incoming`, so the JDK WebSocket always has read-demand outstanding and
                            // the response is never left unread in the socket. The mutex serialises
                            // exchanges, so the next text frame is this request's response.
                            text <- conn.incoming.take
                            _ <- tracer.traceWith(Received(text))
                        } yield text
                        once.timeout(timeout).onError { case _ => drop(conn) }
                    }
                    .handleErrorWith {
                        // Terminal (`retryOnTimeout = false`): re-sending a restore livelocks it
                        // (see `restoreTimeout`). A supervisor restart retries at process
                        // granularity instead, where it is visible and rate-limited.
                        case _: TimeoutException if !retryOnTimeout =>
                            tracer.traceWith(RestoreTimedOut(request.commandNumber, timeout)) >>
                                IO.raiseError(
                                  RemoteL2LedgerError(
                                    "remote L2 ledger did not answer restore to command " +
                                        s"${request.commandNumber} within $timeout"
                                  )
                                )
                        case err =>
                            val wait = backoff(n)
                            // A timeout is not a transport failure: the remote may be fine and the
                            // response simply undelivered in time. Log it distinctly so a receive
                            // stall is unambiguous in the logs, then drop-and-retry like any other
                            // error.
                            val event = err match {
                                case _: TimeoutException =>
                                    ExchangeTimedOut(request.commandNumber, timeout, n, wait)
                                case _ => ConnectionError(n, wait, err)
                            }
                            tracer.traceWith(event) >> IO.sleep(wait) >> attempt(n + 1)
                    }
            // ⛔ The ESCAPE HATCH. `attempt` retries transport failure forever, by design — that is
            // what makes a node tolerate an absent ledger. But it runs inside a cats-actors message
            // handler, which `ActorCell` wraps in `.uncancelable`, so nothing outside can interrupt
            // it: on SIGTERM the actor system cannot terminate and the process has to be
            // force-killed. Racing an internal signal works where cancellation cannot, because the
            // race is *inside* the uncancelable region rather than outside it.
            //
            // `stopping` is completed by a Resource acquired AFTER the ActorSystem, so its finalizer
            // runs BEFORE the system is torn down — the loop is already unwinding by the time the
            // system asks its actors to stop.
            IO.race(stopping.get, attempt(0)).flatMap {
                case Right(response) => IO.pure(response)
                case Left(_) =>
                    IO.raiseError(
                      RemoteL2LedgerShuttingDown(
                        "node is shutting down; abandoning the exchange for command " +
                            s"${request.commandNumber}"
                      )
                    )
            }
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
      *
      * ⛔ The handshake is BOUNDED and a stalled attempt is ABANDONED rather than awaited.
      * `JdkWSClient` builds its socket inside `Resource.make`'s acquire, which cats-effect runs
      * uncancelable, so neither `.timeout` nor a `poll` around `allocated` can interrupt it. A
      * remote that accepts the TCP connection and never completes the WebSocket upgrade therefore
      * blocks the FIRST attempt forever — and because that attempt never returns, the retry ladder
      * in [[exchange]] never engages. Measured: one connection, one log line, then silence
      * indefinitely, while the node still serves HTTP and looks healthy. This is the same defect
      * fixed for the peer transport in `41ddf732`, and `IO.race` cancels only the losing *join*,
      * not the attempt behind it — which is exactly what is wanted.
      */
    private def open: IO[Conn] =
        tracer.traceWith(Connecting(wsUri)) >>
            QuietRelease(wsClient.connectHighLevel(WSRequest(wsUri))).allocated.start
                .flatMap(f =>
                    IO.race(f.joinWithNever, IO.sleep(handshakeBudget)).flatMap {
                        case Left(pair) => cacheConnection(pair)
                        case Right(_)   => abandon(f)
                    }
                )

    /** Give up on a stalled handshake. The attempt is left running — it cannot be cancelled — so
      * anything it eventually produces must still be closed by someone, or it is the exact fd leak
      * this class exists to prevent. A cleanup fiber joins the abandoned attempt and releases
      * whatever it yields; if it never yields, the fiber simply never runs.
      */
    private def abandon(f: FiberIO[(WSConnectionHighLevel[IO], IO[Unit])]): IO[Nothing] =
        f.joinWithNever.flatMap((_, release) => release.attempt.void).start.void >>
            tracer.traceWith(HandshakeStalled(wsUri, handshakeBudget)) >>
            IO.raiseError(
              RemoteL2LedgerError(
                s"WebSocket handshake to $wsUri did not complete within $handshakeBudget"
              )
            )

    /** Install a freshly-allocated connection: start its drain fiber and cache it.
      *
      * Uncancelable: once `allocated` has yielded the (connection, release) pair, a cancellation
      * landing before the release handle reaches `connRef` would strand it and leak the connection.
      */
    private def cacheConnection(pair: (WSConnectionHighLevel[IO], IO[Unit])): IO[Conn] =
        IO.uncancelable { _ =>
            val (connection, release) = pair
            for {
                // A per-connection queue fed by a background fiber that pulls `receiveStream`
                // without pause. Draining it continuously keeps the JDK WebSocket's read-demand
                // (`request(n)`) open, so a response is never left unread in the socket — the
                // receive-stall failure mode of re-pulling `receiveStream.head` afresh per exchange.
                incoming <- Queue.unbounded[IO, String]
                receiveFiber <- connection.receiveStream
                    .collect { case WSFrame.Text(t, _) => t }
                    .foreach(incoming.offer)
                    .compile
                    .drain
                    .start
                conn = Conn(connection, incoming, receiveFiber, release)
                _ <- connRef.set(Some(conn))
                _ <- tracer.traceWith(Connected(wsUri))
            } yield conn
        }

    /** Tell any in-flight exchange to stop retrying and return.
      *
      * ⛔ Must be invoked from a `Resource` acquired **after** the `ActorSystem`, so that its
      * finalizer runs **before** the system is torn down. Acquired earlier, it would fire after the
      * system had already tried (and failed) to stop the actor parked in the retry loop, which is
      * the deadlock it exists to prevent.
      */
    def signalShutdown: IO[Unit] = stopping.complete(()).void

    /** Discard a connection believed broken: clear it from the cache (if still current) and release
      * its resources, ignoring any close error.
      */
    private def drop(conn: Conn): IO[Unit] =
        connRef.update {
            case Some(current) if current eq conn => None
            case other                            => other
        } >> conn.receiveFiber.cancel >> conn.release.attempt.void

}

object RemoteL2Ledger {
    type Config = CardanoNetwork.Section

    /** A live WebSocket connection with its background receive fiber, the queue that fiber feeds,
      * and the finalizer that closes the connection.
      *
      * @param incoming
      *   text frames drained off the connection by [[receiveFiber]]; an exchange takes its response
      *   from here
      * @param receiveFiber
      *   continuously pulls [[connection]]'s `receiveStream` into [[incoming]] for the connection's
      *   whole life, keeping the JDK WebSocket's read-demand open so no response frame is left
      *   unread
      */
    private final case class Conn(
        connection: WSConnectionHighLevel[IO],
        incoming: Queue[IO, String],
        receiveFiber: FiberIO[Unit],
        release: IO[Unit]
    )

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

        /** Instruct the remote to rewind its command-number tip (and committed state) to
          * `commandNumber`. Carries no command payload — the number *is* the instruction. Sent by
          * [[RemoteL2Ledger.restoreTo]] on crash-recovery boot to co-anchor the remote with the
          * restored JointLedger command number.
          */
        final case class Restore(commandNumber: L2CommandNumber) extends Request
    }

    /** The remote's answer to a [[Request.Restore]]: it rewound to the requested command number
      * ([[Restored]] carrying its resulting `tip`, which equals the request on success), or the
      * rewind failed ([[RestoreFailed]] carrying the `requested` number, the remote's current
      * durable `tip`, and a reason). Kept out of [[L2LedgerResponse]] — a restore is a boot-time
      * reconstruction, not a numbered command verdict — but [[commandNumber]] still echoes the
      * requested number so [[RemoteL2Ledger.sendRestoreRequest]] can correlate it with the request.
      */
    sealed trait RestoreResponse {
        def commandNumber: L2CommandNumber
    }

    object RestoreResponse {

        /** `evacuationMapHash` is the remote's [[EvacuationMapHash]] at `tip` — the digest the
          * caller checks its own evacuation map against.
          */
        final case class Restored(tip: L2CommandNumber, evacuationMapHash: EvacuationMapHash)
            extends RestoreResponse {
            def commandNumber: L2CommandNumber = tip
        }
        final case class RestoreFailed(
            requested: L2CommandNumber,
            tip: L2CommandNumber,
            reason: String
        ) extends RestoreResponse {
            def commandNumber: L2CommandNumber = requested
        }
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
      *   How long one exchange may take before the connection is dropped and retried (default 5s).
      *   Kept well under a block cycle so a stuck receive is cut off and retried rather than
      *   stalling the whole (serialised) mutation path.
      * @param restoreTimeout
      *   How long a `Restore` may take (default 10 minutes). It needs its own bound because the
      *   remote answers one by rebuilding its ledger from the whole command log — no snapshots yet
      *   — so the cost is linear in the log, not in the command. SugarRush replays at ~1.3 µs/event
      *   (its `restore_cost_by_log_length` probe: 1.4 ms at 1k, 64 ms at 50k, flat per event), so
      *   the 5s command bound expires past a few million events while this default covers hundreds
      *   of millions. Expiry is **terminal**, not retried: a retry re-sends the restore and the
      *   remote starts the rebuild over, so the pair livelocks and no peer ever boots.
      * @param initialBackoff
      *   Backoff before the first reconnect attempt (default 1s), doubled per attempt
      * @param maxBackoff
      *   Cap on the reconnect backoff (default 30s)
      */
    def create(
        wsUri: String,
        config: Config,
        tracer: ContraTracer[IO, RemoteL2LedgerEvent],
        requestTimeout: FiniteDuration = 5.seconds,
        // An expiry here is terminal: `restoreTo` is deliberately not retried (a retry restarts
        // the rebuild), and every boot calls it, so the node simply fails to start.
        //
        // ⚠️ This value was sized when a Restore replayed the remote's **whole** event log, so the
        // rebuild grew linearly with the head's lifetime. That is no longer how it works: the remote
        // seeds from its newest snapshot and replays only the commands above it, capped by its
        // snapshot interval (20,000 by default), so a rebuild does not grow with the head's age.
        //
        // ⇒ The bound is therefore enormously oversized rather than marginal. It is left as-is
        // deliberately: it costs nothing when nothing is wrong, and the failure it guards against
        // (a boot that never completes) is worse than a boot that takes too long to give up.
        // ⛔ Do not re-derive a rebuild estimate from any figure quoted near this constant: the
        // rebuild cost depends on the snapshot interval and the state's size, and a number measured
        // on one head does not transfer to another.
        restoreTimeout: FiniteDuration = 60.minutes,
        initialBackoff: FiniteDuration = 1.second,
        maxBackoff: FiniteDuration = 30.seconds,
        // Bounds the WebSocket handshake. Generous, because a slow-but-healthy remote must not be
        // abandoned: a ledger rebuilding from its newest snapshot answers in well under this, and
        // the point of the bound is only to stop an attempt that will NEVER complete from parking
        // the connect forever. See `open`.
        handshakeBudget: FiniteDuration = 30.seconds,
    ): Resource[IO, RemoteL2Ledger] =
        for {
            uri <- Resource.eval(IO.fromEither(Uri.fromString(wsUri)))
            // One HttpClient for the whole lifetime — reused across every connection.
            wsClient <- Resource.eval(JdkWSClient.simple[IO])
            connRef <- Resource.make(Ref[IO].of(Option.empty[Conn]))(ref =>
                ref.get.flatMap(_.traverse_(c => c.receiveFiber.cancel >> c.release.attempt.void))
            )
            mutex <- Resource.eval(Mutex[IO])
            // Completed by `stopOnShutdown`, which the caller must acquire AFTER the ActorSystem.
            stopping <- Resource.eval(Deferred[IO, Unit])
        } yield new RemoteL2Ledger(
          uri,
          wsClient,
          mutex,
          connRef,
          requestTimeout,
          restoreTimeout,
          initialBackoff,
          maxBackoff,
          handshakeBudget,
          stopping,
          config,
          tracer
        )
}
