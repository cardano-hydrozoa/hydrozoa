package hydrozoa.integration.harness

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration.Duration

/** Runs an `IO` to completion on the cats-effect [[TestControl]] virtual clock.
  *
  * `TestControl.executeEmbed` is unusable here: every actor runs a 1 s ping loop, so there is
  * always an eligible fiber and it never settles. Drive it the way `ModelBasedSuite` does instead —
  * tick every eligible fiber, and when none is eligible advance the clock to the next timer — until
  * the program (including its resource teardown) has produced a result.
  */
object TestControlDriver:

    def run[A](program: IO[A]): A =
        TestControl
            .execute(program)
            .flatMap(tc =>
                tickUntilAdvancing(tc) >> tc.results.flatMap {
                    case Some(cats.effect.Outcome.Succeeded(value)) => IO.pure(value)
                    case Some(cats.effect.Outcome.Errored(e))       => IO.raiseError(e)
                    case Some(cats.effect.Outcome.Canceled()) =>
                        IO.raiseError(new RuntimeException("inner program canceled"))
                    case None =>
                        IO.raiseError(new RuntimeException("inner program did not terminate"))
                }
            )
            .unsafeRunSync()

    private def tickUntilAdvancing[A](tc: TestControl[A]): IO[Unit] =
        tc.tickOne.flatMap {
            case true => tickUntilAdvancing(tc)
            case false =>
                tc.results.flatMap {
                    case Some(_) => IO.unit
                    case None =>
                        tc.nextInterval.flatMap { next =>
                            if next > Duration.Zero then tc.advance(next) >> tickUntilAdvancing(tc)
                            else
                                IO.raiseError(
                                  new RuntimeException(
                                    "TestControl deadlock: no eligible fibers, no timer"
                                  )
                                )
                        }
                }
        }
