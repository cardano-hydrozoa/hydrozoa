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
  * The crash is modelled as: at the N-th durable op, complete a [[Plan.signal]] and return normally
  * WITHOUT blocking — the fixture races the signal and tears the peer's actor subtree down.
  * Blocking the op (e.g. `IO.never`) would stall the actor's mailbox loop so `stop`'s `Terminate`
  * could never be processed, hanging system shutdown; so the peer keeps running for a deterministic
  * few ops after the signal until the fixture stops it. That is sound because everything past the
  * crash point is IN-MEMORY state that a restart discards anyway — the durable store reflects
  * exactly what landed: on [[CrashVariant.After]] the op's write is included, on
  * [[CrashVariant.Before]] it is skipped (the handler continues with un-persisted state that the
  * restart throws away).
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
            // Count only durable mutations; reads never advance the counter. At the crash point,
            // signal WITHOUT blocking — on Before the op is skipped, on After it runs first; the
            // fixture then tears the peer down (blocking here would stall the mailbox so `stop`
            // could never be processed).
            private def guarded(op: IO[Unit]): IO[Unit] =
                plan match
                    case None => op
                    case Some(Plan(at, variant, signal)) =>
                        counter.updateAndGet(_ + 1).flatMap { n =>
                            if n != at then op
                            else
                                variant match
                                    case CrashVariant.Before => signal.complete(()).void
                                    case CrashVariant.After  => op >> signal.complete(()).void
                        }

            def put(key: StoreKey)(value: key.Value): IO[Unit] = guarded(inner.put(key)(value))

            def delete(key: StoreKey): IO[Unit] = guarded(inner.delete(key))

            def write(batch: WriteBatch): IO[Unit] = guarded(inner.write(batch))

            def get(key: StoreKey): IO[Option[key.Value]] = inner.get(key)

            def getOrFail(key: StoreKey): IO[key.Value] = inner.getOrFail(key)

            def arrivalStamp: IO[ArrivalStamp] = inner.arrivalStamp

            def zeroTimes: IO[Map[Int, Long]] = inner.zeroTimes

            def wallClockOf(stamp: ArrivalStamp): IO[Instant] = inner.wallClockOf(stamp)

            def backend: BackendStore[IO] = inner.backend
