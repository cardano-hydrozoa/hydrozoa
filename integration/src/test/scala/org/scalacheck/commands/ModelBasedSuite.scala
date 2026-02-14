package org.scalacheck.commands

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import org.scalacheck.{Gen, Prop, Shrink}
import org.slf4j.Logger
import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}

/** TODO:
  *   - Add statistics:
  *     - number of commands in cases
  *     - types of commands
  *   - Reproducibility (now seeding is broken)
  *   - Shrinking?
  */

// ===================================
// Command typeclasses
// ===================================

/** Model-side facet: state transitions, preconditions, scheduling. These members depend only on the
  * abstract model state — no SUT interaction.
  *
  * @tparam Cmd
  *   the command type
  * @tparam Result
  *   the result produced by [[runState]]
  * @tparam State
  *   the model-state type
  */
trait ModelCommand[Cmd, Result, State] {

    /** Returns the result and the new [[State]] after this command has run. */
    def runState(cmd: Cmd, state: State): (Result, State)

    /** Precondition that decides if this command is allowed to run when the model is in the
      * provided state.
      */
    def preCondition(cmd: Cmd, state: State): Boolean = true

    /** Virtual-time delay to advance *before* this command's run is executed. Defaults to
      * [[Duration.Zero]] (no delay).
      */
    def delay(cmd: Cmd): FiniteDuration = Duration.Zero
}

/** SUT-side facet: executes the command against the system under test. */
trait SutCommand[Cmd, Result, Sut] {

    /** Executes the command against the SUT and returns the result. */
    def run(cmd: Cmd, sut: Sut): IO[Result]
}

/** Postcondition facet: verifies the SUT result against the model's expectation.
  *
  * The default [[postCondition]] calls [[ModelCommand.runState]] (via the implicit instance) to
  * obtain the expected result and the state-after, then dispatches to [[onSuccessCheck]] or
  * [[onFailureCheck]].
  */
trait CommandProp[Cmd, Result, State] {

    def postCondition(
        cmd: Cmd,
        stateBefore: State,
        result: Either[Throwable, Result]
    )(implicit mc: ModelCommand[Cmd, Result, State]): Prop = {
        val (expectedResult, stateAfter) = mc.runState(cmd, stateBefore)
        result match
            case Right(realResult) =>
                onSuccessCheck(cmd, expectedResult, stateBefore, stateAfter, realResult)
            case Left(e) =>
                onFailureCheck(cmd, expectedResult, stateBefore, stateAfter, e)
    }

    def onSuccessCheck(
        cmd: Cmd,
        expectedResult: Result,
        stateBefore: State,
        stateAfter: State,
        result: Result
    ): Prop = Prop.passed

    def onFailureCheck(
        cmd: Cmd,
        expectedResult: Result,
        stateBefore: State,
        stateAfter: State,
        err: Throwable
    ): Prop = Prop.exception(err)
}

// ===================================
// AnyCommand — type-erased command wrapper
// ===================================

/** A command with its [[Result]] and [[Cmd]] type erased. This is what the test runner operates on.
  *
  * Constructed via the companion [[AnyCommand.apply]] factory, which captures the three typeclass
  * instances and wires [[runPC]] in a type-safe way before erasing [[Result]].
  */
final class AnyCommand[State, Sut](
    /** Precondition check. */
    val preCondition: State => Boolean,
    /** Advances the model state (the Result half of runState is erased). */
    val advanceState: State => State,
    /** Time delay to advance before running. */
    val delay: FiniteDuration,
    /** Runs the command and returns a postcondition predicate over the state *before* the command
      * ran.
      */
    val runPC: Sut => IO[State => Prop],
    val cmd: Any,
    private val repr: String
) {
    override def toString: String = repr
}

object AnyCommand {

    /** Package a concrete command value together with its three typeclass instances into an
      * [[AnyCommand]], erasing the [[Result]] type.
      */
    def apply[Cmd, Result, State, Sut](cmd: Cmd)(implicit
        mc: ModelCommand[Cmd, Result, State],
        sc: SutCommand[Cmd, Result, Sut],
        cp: CommandProp[Cmd, Result, State]
    ): AnyCommand[State, Sut] =
        new AnyCommand(
          preCondition = state => mc.preCondition(cmd, state),
          advanceState = state => mc.runState(cmd, state)._2,
          delay = mc.delay(cmd),
          runPC = sut => {
              import Prop.propBoolean
              sc.run(cmd, sut).attempt.map { r => (s: State) =>
                  mc.preCondition(cmd, s) ==> cp.postCondition(cmd, s, r)
              }
          },
          cmd = cmd,
          repr = cmd.toString
        )
}

// ===================================
// NoOp
// ===================================

/** Produce a no-op [[AnyCommand]]: does nothing, advances no state, always passes. */
def noOp[State, Sut]: AnyCommand[State, Sut] =
    new AnyCommand(
      preCondition = _ => true,
      advanceState = s => s,
      delay = Duration.Zero,
      runPC = _ => IO.pure(_ => Prop.passed),
      cmd = (),
      repr = "NoOp"
    )

// ===================================
// Command generation strategy
// ===================================

/** Abstracts the command-generation strategy. Different properties can supply different
  * implementations to drive generation differently (e.g. only invalid events, only minor blocks,
  * etc.).
  */
trait CommandGen[State, Sut]:
    /** Generates the next command based on the current model state. */
    def genNextCommand(state: State): Gen[AnyCommand[State, Sut]]

    /** Predicate for the target state. */
    def targetStatePrecondition(targetState: State): Boolean = true

// ===================================
// ModelBasedSuite
// ===================================

/** Yet another better/different Commands:
  *   - Support for CE [[IO]] and [[TestControl]]
  *   - Modular commands via typeclasses to separate concerns:
  *     - [[ModelCommand]]: state transitions, preconditions, scheduling
  *     - [[SutCommand]]: execution against the SUT
  *     - [[CommandProp]]: postcondition checks
  *   - [[Env]] was added to facilitate running on environments which you can't control
  *   - Better failure reporting (commands list, the last executed command)
  *
  * Limitations:
  *   - Parallel commands have been removed - we need a different approach
  */
trait ModelBasedSuite {

    private val logger: Logger = org.slf4j.LoggerFactory.getLogger(ModelBasedSuite.getClass)

    /** Represent [some parts of] the environment on which a test case is run.
      *
      * The flow of every test case is as follows (a bit simplified):
      *
      * initEnv -> Env -> genInitState -> State -> startupSut -> Sut -> ... Command.runPC ->
      * shutdownSut -> Prop
      */
    type Env

    /** The model state type.  Must be immutable. */
    type State

    /** A type representing one instance of the system under test (SUT). */
    type Sut

    // ===================================
    // Environment, model, and commands
    // ===================================

    /** Initialize the environment and return information needed for the test suite.
      */
    def initEnv: Env

    /** A generator that should produce an initial [[State]] instance that is usable by
      * [[startupSut]] to create a new system under test.
      *
      * TODO: I guess we may want to have multiple initial state generator/preonditions
      */
    def genInitialState(env: Env): Gen[State]

    /** The precondition for the initial state, when no commands yet have run. */
    def initialStatePreCondition(state: State): Boolean = true

    /** A generator that, given the current model state, should produce a suitable command. The
      * command should always be well-formed, but MUST NOT necessarily be correct, i.e. it may be
      * expected to error upon running against the SUT. The correctness is indicated by
      * [[ModelCommand.preCondition]] and the expected behavior of SUT is checked based on that
      * flag.
      */
    def commandGen: CommandGen[State, Sut]

    /** A custom command generator modificator like resize or something like that (noop by default).
      */
    def commandGenTweaker: [A] => Gen[A] => Gen[A] = [A] => (g: Gen[A]) => g

    // ===================================
    // SUT
    // ===================================

    /** Decides if [[startupSut]] should be allowed to be called with the specified state value.
      * This can be used to limit the number of co-existing [[Sut]] instances.
      *
      * If you want to allow only one [[Sut]] instance to exist at any given time (a singleton
      * [[Sut]]), implement this method the following way:
      *
      * {{{
      *  def canCreateNewSut(inactiveSuts: Iterable[State]
      *    runningSuts: Iterable[State]
      *  ) = inactiveSuts.isEmpty && runningSuts.isEmpty
      * }}}
      */
    def canStartupNewSut(
    ): Boolean

    /** Create a new [[Sut]] instance with an internal state that corresponds to the provided
      * abstract state instance.
      */
    def startupSut(state: State): IO[Sut]

    /** Shutdown the SUT instance, and release any resources related to it. May also run some checks
      * upon shutting SUT down, for which the latest state can be used.
      *
      * TODO: split up: preShutdownCondition / shutdownSut
      */
    def shutdownSut(lastState: State, sut: Sut): IO[Prop]

    // ===================================
    // TestControl
    // ===================================

    /** Whether to use [[TestControl]] for time manipulation. When true (default), delays declared
      * by commands are advanced via the [[TestControl]] virtual clock rather than real
      * [[IO.sleep]]. Set to false for scenarios requiring a real backend (e.g. Yaci), where real
      * wall-clock time must elapse.
      */
    def useTestControl: Boolean

    /** How long to let actors settle (process pending messages) before each command runs when using
      * [[TestControl]]. The inner program always sleeps this duration first; the outer ticks
      * through it (giving actors their ping cycles), then advances the remainder. Total virtual
      * time per command = max(delay, settling).
      *
      * Only used when [[useTestControl]] is true.
      */
    def settling: FiniteDuration = 1.second

    // ===================================
    // Property entry point
    // ===================================

    /** A property that attests that SUT complies to the model.
      */
    final def property(): Prop = {

        val suts = collection.mutable.Map.empty[AnyRef, Option[State => IO[Sut]]]

        Prop.forAll(genTestCase) { testCase =>
            logger.info("Executing the next test case...")

            try {
                val sutId = suts.synchronized {
                    // TODO: now, when we have imperation initEnv we can revert this part
                    // val inactiveSuts = suts.values.collect { case (state, None) => state }
                    // val runningSuts = suts.values.collect { case (state, Some(_)) => state }
                    if canStartupNewSut() then {
                        val sutId = new AnyRef
                        suts += (sutId -> None)
                        Some(sutId)
                    } else None
                }

                sutId match {
                    case Some(id) =>

                        if suts.contains(id) then {
                            val _ = suts.put(id, Some(startupSut))
                            val prop = runTestCase(testCase, startupSut)
                            logger.info("Test case property is evaluated.")
                            prop

                        } else {
                            logger.error("WARNING: you should never not see that")
                            Prop.undecided
                        }

                    case None =>
                        // Here we might wait until canCreateNewSut is true
                        logger.error("WARNING: you should never not see that")
                        Prop.undecided
                }
            } catch {
                case e: Throwable =>
                    suts.synchronized {
                        suts.clear()
                    }
                    throw e
            }
        }
    }

    // ===================================
    // TestCase generation
    // ===================================

    private type Commands = List[AnyCommand[State, Sut]]

    /** @param initialState
      *   the initial state
      * @param commands
      *   sequential commands (now the only type of commands supported)
      */
    private case class TestCase(
        initialState: State,
        commands: Commands
    )

    /** Test case generator.
      */
    private def genTestCase: Gen[TestCase] = {
        import Gen.sized

        /** Generates the sequence of commands of size [[size]] using initial state
          * [[initialState]].
          *
          * @return
          *   the FINAL state and the list of commands
          */
        def sizedCmds(initialState: State)(size: Int): Gen[(State, Commands)] = {
            val l: List[Unit] = List.fill(size)(())
            l.foldLeft(Gen.const((initialState, Nil: Commands))) { (g, _) =>
                for {
                    (s0, cs) <- g
                    c <- commandGen.genNextCommand(s0).suchThat(_.preCondition(s0))
                } yield (c.advanceState(s0), cs :+ c)
            }
        }

        def precondition(targetState: State, tc: TestCase): Boolean =
            initialStatePreCondition(tc.initialState)
                && cmdsPrecond(tc.initialState, tc.commands)._2
                && this.commandGen.targetStatePrecondition(targetState)

        /** Checks all preconditions and evaluates the final state.
          *
          * @return
          *   the final state and the && over preconditions (the state was used for parallel
          *   commands)
          */
        @tailrec
        def cmdsPrecond(s: State, cmds: Commands): (State, Boolean) = cmds match {
            case Nil                          => (s, true)
            case c :: cs if c.preCondition(s) => cmdsPrecond(c.advanceState(s), cs)
            case _                            => (s, false)
        }

        for {
            _ <- Gen.const(
              logger.debug("Initializing the environment and generate the test case...")
            )
            s0: State <- genInitialState(initEnv)
            (s, seqCmds) <- commandGenTweaker(sized(sizedCmds(s0)))
            tc = TestCase(s0, seqCmds)
            if precondition(s, tc)
        } yield tc
    }

    // ===================================
    // Test case runner
    // ===================================

    private def runTestCase(
        testCase: TestCase,
        startupSut: State => IO[Sut]
    ): Prop = {
        val size = testCase.commands.size
        logger.debug(s"Sequential Commands:\n${prettyCmdsRes(testCase.commands, size)}\n")

        val (_sut, p, s, lastCmd, _) =
            if useTestControl
            then runCommandsWithTestControl(testCase, startupSut)
            else runCommandsPlain(testCase, startupSut)

        p.flatMap { r =>
            if r.failure then
                logger.warn(
                  "Property is falsified (see the labels down below). Additional information: \n" +
                      s"Initial state:\n  ${testCase.initialState}\n" +
                      s"Last state:\n  ${s}\n" +
                      s"Sequential Commands:\n${prettyCmdsRes(testCase.commands, lastCmd)}\n" +
                      s"Last executed command: $lastCmd"
                )
            Prop(_ => r)
        } :| "Property failed, see the log above for details"
    }

    private def prettyCmdsRes(rs: List[AnyCommand[State, Sut]], lastCmd: Int) = {
        val maxNumberWidth = "%d".format(lastCmd).length
        val lineLayout = "  %%%dd. %%s".format(maxNumberWidth)
        val cs = rs.zipWithIndex.map { case (r, i) =>
            lineLayout.format(i + 1, r)
        }
        if cs.isEmpty then "  <no commands>"
        else cs.mkString("\n")
    }

    /** Plain execution without [[TestControl]]. Command delays are real [[IO.sleep]]s. Used for
      * backends like Yaci where wall-clock time must elapse.
      */
    private def runCommandsPlain(
        testCase: TestCase,
        startupSut: State => IO[Sut]
    ): (Sut, Prop, State, Int, TestCase) = {
        logger.debug("Using plain IO to run the test case...")
        val io = for {
            initial <- startupSut(testCase.initialState).map(sut =>
                (sut, Prop.proved, testCase.initialState, 0)
            )
            result <- testCase.commands.foldLeft(IO.pure(initial)) { case (acc, c) =>
                acc.flatMap { case (sut, p, s, lastCmd) =>
                    // Short-circuit: if the property has already failed, skip remaining commands
                    val currentResult = p.apply(Gen.Parameters.default)
                    if currentResult.failure then IO.pure((sut, p, s, lastCmd))
                    else
                        IO.sleep(c.delay) >> c.runPC(sut).map { pred =>
                            (sut, p && pred(s), c.advanceState(s), lastCmd + 1)
                        }
                }
            }
            (sut, prop, s, lastCmd) = result
            shutdownProp <- shutdownSut(s, sut)
        } yield (sut, prop && shutdownProp, s, lastCmd, testCase)

        io.unsafeRunSync()
    }

    /** [[TestControl]]-based execution. Command delays are advanced via the virtual clock using
      * [[TestControl#advance]], which skips over the per-actor ping loops that would otherwise
      * cause [[TestControl#tickAll]] to iterate once per second (or somewhat) of virtual time.
      *
      * Protocol: the inner IO (running on the TestControl runtime) and the outer driver (on the
      * real runtime) communicate via a pair of [[java.util.concurrent.atomic.AtomicReference]]s:
      *   - [[pendingDelay]]: the inner program writes the delay for the current command here before
      *     blocking on [[gate]]. The outer driver reads it to know how much to advance.
      *   - [[gate]]: a volatile flag. The inner program spins on it (yielding via [[IO.cede]]). The
      *     outer driver sets it to true after advancing time, unblocking the inner.
      *
      * After each command completes, the inner signals completion by writing [[Duration.Zero]] to
      * [[pendingDelay]] and blocking on the gate again. The outer ticks until it sees that signal,
      * runs postCondition, and releases the gate for the next command.
      */
    private def runCommandsWithTestControl(
        testCase: TestCase,
        startupSut: State => IO[Sut]
    ): (Sut, Prop, State, Int, TestCase) = {
        import java.util.concurrent.atomic.AtomicReference

        // The inner writes a Some(delay) to request a time advance before its command runs.
        // None means "not ready yet" / "waiting for outer to tick".
        val pendingDelay = new AtomicReference[Option[FiniteDuration]](None)
        // The outer sets this to true to release the inner after advancing time.
        val gate = new AtomicReference[Boolean](false)

        logger.debug("Using TestControl to run the test case...")

        // The inner program: newSut, then for each command:
        //   1. sleep(settling) — keeps inner busy while outer ticks actors
        //   2. signal delay — outer reads and advances (delay - settling)
        //   3. wait for gate — outer releases after advancing
        //   4. run command
        // Finally shutdownSut.
        val innerIO: IO[(Sut, Prop, State, Int, TestCase)] = for {
            initial <- startupSut(testCase.initialState).map(sut =>
                (sut, Prop.proved, testCase.initialState, 0)
            )
            result <- testCase.commands.foldLeft(IO.pure(initial)) { case (acc, c) =>
                acc.flatMap { case (sut, p, s, lastCmd) =>
                    // Short-circuit: if the property has already failed, skip remaining commands
                    val currentResult = p.apply(Gen.Parameters.default)
                    if currentResult.failure then IO.pure((sut, p, s, lastCmd))
                    else
                        for {
                            // Sleep for the settling window so the outer can tick actors
                            // and let pending messages propagate before the advancing the time
                            // and after, before running the command.
                            _ <- IO.sleep(settling)
                            _ <- IO(pendingDelay.set(Some(c.delay)))
                            _ <- IO.cede.whileM_(IO(!gate.get))
                            _ <- IO(gate.set(false))
                            _ <- IO.sleep(settling)
                            pred <- c.runPC(sut)
                        } yield (sut, p && pred(s), c.advanceState(s), lastCmd + 1)
                }
            }
            (sut, prop, s, lastCmd) = result
            shutdownProp <- shutdownSut(s, sut)
        } yield (sut, prop && shutdownProp, s, lastCmd, testCase)

        // Outer driver: start the inner on the TestControl runtime, then drive it command by
        // command. Between commands, we advance the virtual clock by the declared delay.
        //
        // We cannot use tickAll/tick here: after actors are created, the per-actor ping loop
        // (every 1s) and the inner gate spin (IO.cede.whileM_) both produce immediately-eligible
        // fibers that cause tick/tickAll to loop too long. Instead, we use tickOne in a loop,
        // stopping when the inner signals via the shared AtomicReferences.
        //
        // tickOne runs on the outer (real) runtime and synchronously executes one step of the
        // inner runtime. advance also runs on the outer runtime and moves the inner clock.
        val outerIO: IO[(Sut, Prop, State, Int, TestCase)] = for {
            // 1. Start the inner on the mocked runtime. It's paused — nothing runs yet.
            tc <- TestControl.execute(innerIO)
            totalAdvanced <- IO(new java.util.concurrent.atomic.AtomicLong(0L))

            // 2. Pump until the first signal.
            // Tick the inner until it posts the first delay request (i.e. newSut has completed
            // and the first command's gate spin has started), or until the program finishes
            // (possible when cs is empty: newSut → shutdownSut with no commands in between).
            _ <- tickUntil(
              tc,
              IO(pendingDelay.get().isDefined).flatMap { pending =>
                  if pending then IO.pure(true)
                  else tc.results.map(_.isDefined)
              }
            )

            // 3. Guard: if the inner already finished (empty command list), skip the loop.
            // Drive each command: read delay → advance → release gate → tick until next signal.
            // If the program already finished (e.g. cs was empty), skip the command loop.
            _ <- tc.results.flatMap {
                case Some(_) => IO.unit //   empty command list, done
                case None    =>
                    // 4. Otherwise, run per-command iteration.
                    // Note that the outer doesn't need the command object.
                    // It just needs to iterate the right number of times.
                    testCase.commands.foldLeft(IO.unit) { (acc, _u) =>
                        acc >> tc.results.flatMap {
                            case Some(_) => IO.unit // already finished, skip remaining
                            case None =>
                                for {
                                    // 5. Read the delay the inner posted and clear it atomically
                                    delay <- IO(pendingDelay.getAndSet(None).get)
                                    _ <- IO(totalAdvanced.addAndGet(delay.toNanos): Unit)

                                    // 6. The inner already slept for `settling` before
                                    // signalling and going to sleep after we advance the time.
                                    // Advance the remainder so total virtual
                                    // time equals max(delay, settling * 2).
                                    // When delay < settling * 2
                                    // the settling sleep already covered it; no further
                                    // advance is needed.
                                    _ <- {
                                        val remaining = delay - settling * 2
                                        if remaining > Duration.Zero then tc.advance(remaining)
                                        else IO.unit
                                    }
                                    // 7. Release the inner to execute the command.
                                    _ <- IO(gate.set(true))

                                    // 8. Pump until the next signal.
                                    // Tick until either:
                                    //   - the inner posts the next delay (next command's gate spin), or
                                    //   - the inner program finishes (results become available).
                                    _ <- tickUntil(
                                      tc,
                                      IO(pendingDelay.get().isDefined).flatMap { pending =>
                                          if pending then IO.pure(true)
                                          else tc.results.map(_.isDefined)
                                      }
                                    )
                                } yield ()
                        }
                    }
            }
            // 9. If results aren't available yet (shouldn't happen, but be safe), drain remaining.
            _ <- tickUntil(tc, tc.results.map(_.isDefined))
            // 10. Extract the result
            result <- tc.results
            _ <- IO {
                val nanos = totalAdvanced.get
                val days = nanos / 86_400_000_000_000L

                ModelBasedSuite.addSimulatedNanos(nanos)
                logger.info(s"---- TC ---- seed: ${tc.seed}  simulated: ${days} days")
            }
        } yield result match {
            case Some(cats.effect.Outcome.Succeeded(value)) => value
            case Some(cats.effect.Outcome.Errored(e))       => throw e
            case Some(cats.effect.Outcome.Canceled()) =>
                throw new RuntimeException("Inner program was canceled")
            case None =>
                throw new RuntimeException(
                  "Inner program did not produce a result (deadlock or non-termination)"
                )
        }

        outerIO.unsafeRunSync()
    }

    /** Tick the inner runtime one fiber at a time until [[done]] returns true. When no fibers are
      * immediately eligible (tickOne returns false) but the predicate is still false, advance the
      * virtual clock to the next scheduled task to avoid a hard deadlock.
      */
    private def tickUntil[A](tc: TestControl[A], done: IO[Boolean]): IO[Unit] =
        done.flatMap {
            case true => IO.unit
            case false =>
                tc.tickOne.flatMap {
                    case true  => tickUntil(tc, done)
                    case false =>
                        // Here is no immediately eligible fibers. Advance to the next scheduled task
                        // (e.g. a ping or a sleep) so we don't spin forever.
                        // NB: nextInterval: how long until the nearest sleeper wakes up?
                        tc.nextInterval.flatMap { next =>
                            if next > Duration.Zero then tc.advance(next) >> tickUntil(tc, done)
                            else
                                // nextInterval == 0 and no eligible tasks means deadlock.
                                IO.raiseError(
                                  new RuntimeException(
                                    "TestControl deadlock: no eligible fibers and predicate not satisfied"
                                  )
                                )
                        }
                }
        }
}

object ModelBasedSuite {
    private val totalSimulatedNanos = new java.util.concurrent.atomic.AtomicLong(0L)
    private val startNanoTime = System.nanoTime()

    // TODO: make optional
    Runtime.getRuntime.addShutdownHook(new Thread {
        override def run(): Unit = {
            val simNanos = totalSimulatedNanos.get

            val simDays = simNanos / 86_400_000_000_000L
            val realNanos = System.nanoTime() - startNanoTime
            val realSecs = realNanos / 1_000_000_000L
            val realMins = realSecs / 60L
            val realRemSec = realSecs % 60L

            println
            println(
              s"---- TestControl ---- GRAND TOTAL simulated time: ${simDays} days (across all test cases)"
            )
            println(
              s"---- TestControl ---- GRAND TOTAL real time:      ${realMins}m ${realRemSec}s"
            )

        }
    })

    private[commands] def addSimulatedNanos(nanos: Long): Unit =
        totalSimulatedNanos.addAndGet(nanos): Unit
}
