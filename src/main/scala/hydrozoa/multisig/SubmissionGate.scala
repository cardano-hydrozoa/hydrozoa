package hydrozoa.multisig

import cats.effect.{Deferred, IO, Ref}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Admits user submissions while the regime that answers them runs.
  *
  * The HTTP submission route asks the `RequestSequencer` and waits for its answer, with no timeout.
  * At the handoff to the rule-based regime the regime manager stops the sequencer, and a request
  * still reaching it then went to dead letters and left its HTTP call waiting forever. So the
  * regime manager closes this gate first, waits (bounded) for the submissions already admitted to
  * be answered, and only then stops the sequencer. A submission that arrives after the gate closes,
  * or that is still waiting when the sequencer stops, gets `closed` instead.
  */
final class SubmissionGate private (
    admitted: Ref[IO, SubmissionGate.Admitted],
    answerersStopped: Deferred[IO, Unit],
):

    /** Run `submit` if the gate is open, else return `closed`. If the actors that answer
      * submissions stop while `submit` waits, return `closed` instead of waiting forever.
      */
    def admit[A](closed: A)(submit: IO[A]): IO[A] =
        IO.uncancelable { poll =>
            admitted
                .modify(a =>
                    if a.open then (a.copy(inFlight = a.inFlight + 1), true) else (a, false)
                )
                .flatMap { admittedNow =>
                    if !admittedNow then IO.pure(closed)
                    else
                        poll(IO.race(answerersStopped.get, submit))
                            .map(_.fold(_ => closed, identity))
                            .guarantee(admitted.update(a => a.copy(inFlight = a.inFlight - 1)))
                }
        }

    /** Close the gate, then wait up to `bound` for every admitted submission to finish. */
    def closeAndDrain(bound: FiniteDuration): IO[Unit] =
        admitted.update(_.copy(open = false)) >> drained.timeoutTo(bound, IO.unit)

    /** Record that the actors answering submissions are stopped, so nothing waits on them. */
    def markAnswerersStopped: IO[Unit] = answerersStopped.complete(()).void

    private def drained: IO[Unit] =
        admitted.get.flatMap(a =>
            if a.inFlight == 0 then IO.unit else IO.sleep(10.millis) >> drained
        )

object SubmissionGate:

    private final case class Admitted(open: Boolean, inFlight: Int)

    /** An open gate. */
    def apply(): IO[SubmissionGate] =
        for
            admitted <- Ref[IO].of(Admitted(open = true, inFlight = 0))
            answerersStopped <- Deferred[IO, Unit]
        yield new SubmissionGate(admitted, answerersStopped)

    /** An open gate, for a field initialiser. */
    def unsafeOpen(): SubmissionGate =
        new SubmissionGate(
          Ref.unsafe[IO, Admitted](Admitted(open = true, inFlight = 0)),
          Deferred.unsafe[IO, Unit]
        )
