package hydrozoa.multisig.consensus.limiter

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.node.operation.multisig.RateLimits
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.metrics.PeerMetrics
import scala.concurrent.duration.DurationLong

/** FIFO rate-limiter actor sitting between two actors on one lane.
  *
  * Enforces a **spacing gate**: consecutive throttled messages are released at least `period`
  * apart, where `period` is the message's own `minPeriod` divided by the gate multiplier (1.0 on a
  * lane with no [[LimiterGate]]). The limiter times from its own last release, not from the
  * message's timestamp.
  *
  * ⛔ That distinction is the whole contract. Gating on `msg.limiterTimestamp + minPeriod` is an AGE
  * filter: it delays each message by a fixed amount and lets the arrival rate through unchanged, so
  * it bounds latency rather than rate. It coincides with a rate limit only on a lane that is
  * already single-flight, where releasing one message is what creates the next. Do not "simplify"
  * back to it.
  *
  * Spacing is measured on [[cats.effect.IO.monotonic]], so a wall-clock step cannot open or freeze
  * the gate, and the resolution of `limiterTimestamp` is irrelevant.
  *
  * Messages are released in arrival order. One that is not throttled (or is exempt) is forwarded
  * immediately when nothing is held, and queued behind whatever is held otherwise.
  *
  * ⚠️ On a single-flight lane the queue never exceeds one throttled entry. That is an upstream
  * property this class does not enforce, so a second one is traced rather than assumed impossible.
  *
  * ==Failure==
  * This actor is not restarted — `MultisigRegimeManagerBase` escalates every supervisor directive —
  * so a failure takes the node down and anything held is lost. That is the safe outcome for a
  * self-clocked lane: upstream waiting forever on a release that was silently dropped is a stall
  * with nothing in the log, whereas a crash restarts and recovers its position from persistence. ⛔
  * Do not catch exceptions here to "be safe"; swallowing one converts the crash into the stall.
  *
  * @param downstream
  *   the actor the throttled lane terminates at. The limiter's own [[ActorRef]] is what upstream
  *   actors are wired to in place of `downstream`. `ActorRef` is contravariant in its message type,
  *   so the limiter's wider handle is usable wherever the downstream's narrower one is expected.
  * @param gate
  *   opt-in downstream-backlog gating. `None` means a pure spacing gate at the configured period:
  *   no counting, no ticks, multiplier fixed at 1.0. ⛔ Must stay opt-in — a lane that counted
  *   releases but was never sent [[LimiterControl.DownstreamDrained]] would stretch its period
  *   without bound, silently.
  * @param metrics
  *   write-only publication of gate state for the stats endpoints. Nothing in the control path
  *   reads it back; the multiplier acted on is the one in this actor's state.
  */
final case class Limiter[Msg](
    downstream: ActorRef[IO, Msg],
    config: RateLimits.Section,
    tracer: ContraTracer[IO, LimiterEvent],
    gate: Option[LimiterGate] = None,
    metrics: Option[PeerMetrics] = None
) extends Actor[IO, Msg | LimiterControl] {

    given RateLimits.Section = config

    /** Actor-private state, read and written only from inside `receive`. cats-actors drains a
      * mailbox serially, so read-modify-write across a `flatMap` needs no further synchronisation.
      */
    private val stateRef: Ref[IO, Limiter.State[Msg]] =
        Ref.unsafe[IO, Limiter.State[Msg]](Limiter.State.initial[Msg])

    override def preStart: IO[Unit] =
        tracer.traceWith(LimiterEvent.Started)

    override def receive: Receive[IO, Msg | LimiterControl] = PartialFunction.fromFunction {
        case LimiterControl.Tick =>
            stateRef.update(_.copy(tickArmed = false)) >> pumpAndArm

        case LimiterControl.DownstreamDrained =>
            onDrained

        case msg =>
            // Safe by disjointness: `LimiterControl` is matched above and is not part of any lane's
            // message type. `Msg` is erased, so this is a no-op cast.
            enqueue(msg.asInstanceOf[Msg]) >> pumpAndArm
    }

    // ---- queueing -----------------------------------------------------------------------------

    /** Commit the arrival to `stateRef` before anything else looks at it, so the queue is the one
      * source of truth every step below re-reads.
      */
    private def enqueue(msg: Msg): IO[Unit] =
        stateRef.updateAndGet(st => st.copy(queue = st.queue :+ msg)).flatMap { st =>
            val throttledPending = st.queue.count {
                case t: LimiterTimestamp => !t.limiterExempt
                case _                   => false
            }
            // The single-flight invariant says this is 1. Trace, do not drop and do not fail: if
            // the invariant is ever relaxed upstream, FIFO queueing is still correct behaviour.
            if throttledPending > 1 then
                tracer.traceWith(LimiterEvent.QueueDepthUnexpected(throttledPending))
            else IO.unit
        }

    /** Release everything currently due, then arm a tick if something is still waiting. */
    private def pumpAndArm: IO[Unit] =
        pump.flatMap(st => if st.queue.isEmpty || st.tickArmed then IO.unit else armTick(st))

    private def pump: IO[Limiter.State[Msg]] =
        stateRef.get.flatMap { st =>
            st.queue.headOption match {
                case None => IO.pure(st)
                case Some(head) =>
                    head match {
                        case t: LimiterTimestamp if !t.limiterExempt =>
                            IO.monotonic.map(_.toMillis).flatMap { now =>
                                st.lastReleaseMs match {
                                    case Some(last) if now < last + periodMs(t, st) =>
                                        holdBegun(st, t, last + periodMs(t, st) - now)
                                    case _ => releaseHead(st, head, advanceTo = Some(now))
                                }
                            }
                        // Deliberately does NOT restart the spacing clock: an exempt message is
                        // not paced, so letting it reset the clock would let a stream of them
                        // shift the whole schedule.
                        case other => releaseHead(st, other, advanceTo = None)
                    }
            }
        }

    /** Send first, then advance. A send that fails therefore leaves the message queued and the
      * spacing clock unmoved: the state never records a release that did not happen.
      */
    private def releaseHead(
        st: Limiter.State[Msg],
        head: Msg,
        advanceTo: Option[Long]
    ): IO[Limiter.State[Msg]] =
        val released = st.gateState.released + advanceTo.fold(0L)(_ => 1L)
        val advanced = advanceTo match {
            case Some(now) =>
                st.copy(
                  queue = st.queue.tail,
                  lastReleaseMs = Some(now),
                  holdStartedMs = None,
                  gateState = st.gateState.copy(released = released)
                )
            case None => st.copy(queue = st.queue.tail)
        }
        release(head) >> stateRef.set(advanced) >>
            advanceTo.fold(IO.unit)(_ => publishBacklog(released)) >> pump

    /** Trace once when a hold starts, not once per slice — a 5 s hold at a 150 ms slice is 33
      * ticks, and a log line per tick would bury the signal.
      */
    private def holdBegun(
        st: Limiter.State[Msg],
        t: LimiterTimestamp,
        waitMs: Long
    ): IO[Limiter.State[Msg]] =
        if st.holdStartedMs.isDefined then IO.pure(st)
        else
            val name = t.getClass.getSimpleName
            for {
                _ <- tracer.traceWith(LimiterEvent.HoldingMsg(name, waitMs))
                _ <- if gate.isEmpty then IO.unit else IO(metrics.foreach(_.onBlockGateHold()))
                now <- IO.monotonic.map(_.toMillis)
                updated <- stateRef.updateAndGet(_.copy(holdStartedMs = Some(now)))
            } yield updated

    private def armTick(st: Limiter.State[Msg]): IO[Unit] =
        val sliceMs = gate.fold(Limiter.DefaultSliceMs)(_.slice.toMillis)
        for {
            now <- IO.monotonic.map(_.toMillis)
            due = st.queue.headOption match {
                case Some(t: LimiterTimestamp) =>
                    st.lastReleaseMs.fold(now)(_ + periodMs(t, st))
                case _ => now
            }
            waitMs = math.max(0L, math.min(due - now, sliceMs))
            _ <- stateRef.update(_.copy(tickArmed = true))
            // Blocks this mailbox for at most one slice, deliberately. The self-send goes out
            // AFTER the sleep, so anything that arrived meanwhile is processed first.
            _ <- IO.sleep(waitMs.millis)
            _ <- context.self ! LimiterControl.Tick
        } yield ()

    private def release(msg: Msg): IO[Unit] = (downstream ! msg).void

    /** Only a gated lane may write these; otherwise the gauges would carry another lane's numbers.
      */
    private def publishBacklog(released: Long): IO[Unit] =
        if gate.isEmpty then IO.unit else IO(metrics.foreach(_.onBlockGateRelease(released)))

    // ---- the gate -----------------------------------------------------------------------------

    private def periodMs(t: LimiterTimestamp, st: Limiter.State[Msg]): Long =
        val base = t.minPeriod.toMillis
        val m = if gate.isEmpty then 1.0 else st.gateState.multiplier
        if m >= 1.0 then base else math.ceil(base / m).toLong

    private def onDrained: IO[Unit] = gate match {
        // A lane with no gate has no notion of downstream backlog. Nothing sends it drain signals
        // today; ignoring them keeps that from mattering if something ever does.
        case None => IO.unit
        case Some(g) =>
            stateRef.get.flatMap { st =>
                val before = st.gateState.released
                val next = g.observeDrain(st.gateState)
                stateRef.set(st.copy(gateState = next)) >>
                    tracer.traceWith(
                      LimiterEvent.GateUpdated(before, next.residual, next.multiplier)
                    ) >>
                    IO(
                      metrics.foreach(_.onBlockGateUpdate(before, next.residual, next.multiplier))
                    ) >>
                    pumpAndArm
            }
    }
}

object Limiter {
    type Handle[Msg] = ActorRef[IO, Msg]

    /** Slice for a lane with no gate. Its period is static, so slicing only bounds how long the
      * mailbox is blocked.
      */
    private val DefaultSliceMs: Long = 1000L

    /** ⛔ `Option`, not a `-1` sentinel: these hold [[cats.effect.IO.monotonic]] readings, i.e.
      * `System.nanoTime`, whose origin is arbitrary and which the JDK permits to be negative. A
      * `>= 0` test for "have we released yet" would read false forever on such a platform and the
      * gate would never engage.
      *
      * @param lastReleaseMs
      *   monotonic millis of the last throttled release; `None` before the first, which is what
      *   makes the first message due immediately — a freshly started limiter must not invent a
      *   delay.
      * @param holdStartedMs
      *   monotonic millis at which the current hold began; `None` when nothing is held. Exists only
      *   so a hold is traced once rather than once per slice.
      */
    final case class State[Msg](
        lastReleaseMs: Option[Long],
        queue: Vector[Msg],
        tickArmed: Boolean,
        holdStartedMs: Option[Long],
        gateState: LimiterGate.State
    )

    object State {
        def initial[Msg]: State[Msg] = State[Msg](
          lastReleaseMs = None,
          queue = Vector.empty,
          tickArmed = false,
          holdStartedMs = None,
          gateState = LimiterGate.Open
        )
    }
}
