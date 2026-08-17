package hydrozoa.integration.harness

import cats.effect.{Deferred, IO, Ref}
import hydrozoa.multisig.persistence.{ArrivalStamp, BackendStore, Persistence, StoreKey, WriteBatch}
import java.time.Instant

/** Which side of a durable write the injected crash lands on. */
enum CrashVariant:
    /** Crash before the op's write reaches the backend — nothing persisted (the sender re-derives /
      * the user resubmits).
      */
    case Before

    /** Let the op's write land, then crash before the handler's next step (advance cursor / send /
      * dispatch) — the CR4 / CR8 durability-barrier point.
      */
    case After

/** A [[Persistence]] decorator for deterministic crash-recovery testing: it counts the wrapped
  * peer's durable mutations and, at a chosen count, simulates a process death at that exact point.
  *
  * All durable writes in the node funnel through `put` / `delete` / `write` (`Persistence`
  * §Persistence.scala), so a single decorator over one peer's `Persistence` observes every durable
  * op that peer makes — including each `PeerLiaison*.persistInbound` (the CR8 write-before-advance
  * point) and every consensus-actor write — with no changes to the actors or the backend. Under the
  * stage4 `TestControl` clock the op order is deterministic, so "the N-th write" is reproducible.
  *
  * The crash is modelled as: at the N-th durable op, complete a [[Plan.signal]] and then block the
  * op forever ([[IO.never]]). The fixture races the signal and tears the peer's actor subtree down,
  * so the handler never advances, sends, or dispatches past the crash point. On
  * [[CrashVariant.After]] the op's write lands first; on [[CrashVariant.Before]] it does not.
  */
object CrashingPersistence:

    /** A crash plan: fire at the `at`-th durable op (1-based) with `variant` deciding whether that
      * op's write lands first. `signal` is completed when the crash point is reached — the fixture
      * awaits it, then stops the peer's actor subtree and rebuilds it against the same store.
      */
    final case class Plan(at: Int, variant: CrashVariant, signal: Deferred[IO, Unit])

    /** Wrap `inner`, sharing `counter` across all of this peer's durable ops. `plan = None` is an
      * ordinary pass-through (used for the post-restart instance, which must not crash again).
      */
    def wrap(
        inner: Persistence[IO],
        counter: Ref[IO, Int],
        plan: Option[Plan],
    ): Persistence[IO] =
        new Persistence[IO]:
            // Count only durable mutations; reads never advance the counter.
            private def guarded(op: IO[Unit]): IO[Unit] =
                plan match
                    case None => op
                    case Some(Plan(at, variant, signal)) =>
                        counter.updateAndGet(_ + 1).flatMap { n =>
                            if n != at then op
                            else
                                variant match
                                    case CrashVariant.Before => crash(signal)
                                    case CrashVariant.After  => op >> crash(signal)
                        }

            // Signal the crash point, then block forever — the fixture stops this peer's actors,
            // cancelling the blocked op, so nothing past the crash point runs.
            private def crash(signal: Deferred[IO, Unit]): IO[Unit] =
                signal.complete(()).attempt >> IO.never

            def put(key: StoreKey)(value: key.Value): IO[Unit] = guarded(inner.put(key)(value))

            def delete(key: StoreKey): IO[Unit] = guarded(inner.delete(key))

            def write(batch: WriteBatch): IO[Unit] = guarded(inner.write(batch))

            def get(key: StoreKey): IO[Option[key.Value]] = inner.get(key)

            def getOrFail(key: StoreKey): IO[key.Value] = inner.getOrFail(key)

            def arrivalStamp: IO[ArrivalStamp] = inner.arrivalStamp

            def zeroTimes: IO[Map[Int, Long]] = inner.zeroTimes

            def wallClockOf(stamp: ArrivalStamp): IO[Instant] = inner.wallClockOf(stamp)

            def backend: BackendStore[IO] = inner.backend
