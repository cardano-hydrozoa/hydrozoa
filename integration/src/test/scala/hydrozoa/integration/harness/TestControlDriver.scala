package hydrozoa.integration.harness

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}

/** Runs an `IO` to completion on the cats-effect [[TestControl]] virtual clock.
  *
  * `TestControl.executeEmbed` is unusable here: every actor runs a 1 s ping loop, so there is
  * always an eligible fiber and it never settles. Drive it the way `ModelBasedSuite` does instead —
  * tick every eligible fiber, and when none is eligible advance the clock to the next timer — until
  * the program (including its resource teardown) has produced a result.
  *
  * **Bounded.** The run fails once `horizon` of virtual time has passed. Those same ping loops mean
  * a program that will never finish still always has a next timer, so an unbounded driver cannot
  * tell "slow" from "stuck" and ticks forever. The horizon is counted from the first advance on,
  * because the first advance is the harness's jump from virtual zero to the real epoch
  * (`PreSystem.align`).
  *
  * **Replayable.** The TestContext seed is named on every failure; set `TESTCONTROL_SEED` to it (or
  * pass it as `seed`) to replay the same interleaving (`TestControl.execute` otherwise draws a
  * random one per run). `ModelBasedSuite` reads the same variable, so one setting replays either
  * driver.
  */
object TestControlDriver:

    /** Well past the longest virtual run any suite here makes, and cheap to hit: a stuck head
      * covers hours of virtual time in seconds of wall clock.
      */
    val defaultHorizon: FiniteDuration = 2.hours

    def run[A](
        program: IO[A],
        horizon: FiniteDuration = defaultHorizon,
        seed: Option[String] = sys.env.get("TESTCONTROL_SEED"),
    ): A =
        TestControl
            .execute(program, seed = seed)
            .flatMap(tc =>
                // Printed as well as attached: a test reporter need not show suppressed exceptions.
                val replay =
                    s"TestControl interleaving seed: ${tc.seed} (replay: TESTCONTROL_SEED=${tc.seed})"
                def failWith(e: Throwable): IO[A] =
                    IO(System.err.println(s"[TestControlDriver] ${e.getMessage} — $replay")) >>
                        IO(e.addSuppressed(new RuntimeException(replay))) >> IO.raiseError(e)
                tickUntilAdvancing(tc, horizon, elapsed = None).attempt.flatMap {
                    case Left(e) => failWith(e)
                    case Right(()) =>
                        tc.results.flatMap {
                            case Some(cats.effect.Outcome.Succeeded(value)) => IO.pure(value)
                            case Some(cats.effect.Outcome.Errored(e))       => failWith(e)
                            case Some(cats.effect.Outcome.Canceled()) =>
                                failWith(new RuntimeException("inner program canceled"))
                            case None =>
                                failWith(new RuntimeException("inner program did not terminate"))
                        }
                }
            )
            .unsafeRunSync()

    /** `elapsed` is the virtual time advanced since the first advance, `None` before it. */
    private def tickUntilAdvancing[A](
        tc: TestControl[A],
        horizon: FiniteDuration,
        elapsed: Option[FiniteDuration],
    ): IO[Unit] =
        tc.tickOne.flatMap {
            case true => tickUntilAdvancing(tc, horizon, elapsed)
            case false =>
                tc.results.flatMap {
                    case Some(_) => IO.unit
                    case None =>
                        tc.nextInterval.flatMap { next =>
                            val after = elapsed.map(_ + next)
                            if next <= Duration.Zero then
                                IO.raiseError(
                                  new RuntimeException(
                                    "TestControl deadlock: no eligible fibers, no timer"
                                  )
                                )
                            else if after.exists(_ > horizon) then
                                IO.raiseError(
                                  new RuntimeException(
                                    s"TestControl: the program was still running after $horizon " +
                                        "of virtual time"
                                  )
                                )
                            else
                                tc.advance(next) >>
                                    tickUntilAdvancing(
                                      tc,
                                      horizon,
                                      after.orElse(Some(Duration.Zero))
                                    )
                        }
                }
        }
