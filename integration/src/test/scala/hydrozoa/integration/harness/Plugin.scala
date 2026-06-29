package hydrozoa.integration.harness

import cats.effect.{Deferred, IO}
import cats.syntax.foldable.*
import hydrozoa.lib.logging.ContraTracer

/** A pluggable test extension: carries one tracer arm to be folded into the harness's root
  * [[MultiPeerHeadHarness.Event]] tracer.
  */
sealed trait Plugin:
    def tracer: ContraTracer[IO, MultiPeerHeadHarness.Event]

object Plugin:
    /** Fold N plugins' arms into the single tracer the harness consumes. */
    def tracerOf(plugins: Plugin*): ContraTracer[IO, MultiPeerHeadHarness.Event] =
        plugins.toList.map(_.tracer).combineAll

/** State (typically Refs bundled in `H`) plus an arm that writes to that state on each event. */
final class Capture[H] private (
    val state: H,
    val tracer: ContraTracer[IO, MultiPeerHeadHarness.Event],
) extends Plugin

object Capture:
    /** `handle` closes over `state` and decides what to write per event. */
    def make[H](
        state: H,
    )(
        handle: H => MultiPeerHeadHarness.Event => IO[Unit],
    ): Capture[H] =
        new Capture(state, ContraTracer.emit(handle(state)))

/** A `Deferred[T]` plus an arm that completes it the first time `check` returns `Some`. The
  * predicate typically reads a [[Capture]]'s state (closed over by the caller) — this is the
  * "layered on top" piece.
  */
final class Signal[T] private (
    val signal: Deferred[IO, T],
    val tracer: ContraTracer[IO, MultiPeerHeadHarness.Event],
) extends Plugin:
    def await: IO[T] = signal.get

    /** Manually fire the signal once. Used when the test detects the condition outside the
      * predicate path (e.g. at arming time the condition is already met and no future event
      * would re-trigger the predicate).
      */
    def complete(value: T): IO[Unit] = signal.complete(value).void

object Signal:
    /** Build a signal that fires once `check` returns `Some`. */
    def make[T](
        check: MultiPeerHeadHarness.Event => IO[Option[T]],
    ): IO[Signal[T]] =
        IO.deferred[T].map { deferred =>
            val tracer = ContraTracer.emit[IO, MultiPeerHeadHarness.Event] { event =>
                deferred.tryGet.flatMap {
                    case Some(_) => IO.unit
                    case None =>
                        check(event).flatMap {
                            case Some(t) => deferred.complete(t).void
                            case None    => IO.unit
                        }
                }
            }
            new Signal(deferred, tracer)
        }
