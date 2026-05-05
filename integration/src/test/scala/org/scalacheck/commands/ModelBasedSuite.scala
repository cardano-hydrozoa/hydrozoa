package org.scalacheck.commands

import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import ch.qos.logback.classic.Level
import hydrozoa.lib.logging.Logging
import org.scalacheck.{Gen, Prop, Shrink}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}

/** Utility for temporarily changing log levels. */
object LoggingControl {

    /** Temporarily suspends ALL loggers with a warning message.
      *
      * @param reason
      *   A description of why logging is being suspended (e.g., "command generation")
      * @param block
      *   The code to execute with logging suspended
      */
    def withSuppressedLogs[A](reason: String)(block: => A): A = {
        import ch.qos.logback.classic.LoggerContext

        import scala.jdk.CollectionConverters.*

        val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
        val allLoggers = loggerContext.getLoggerList.asScala.toList

        // Save original levels
        val originalLevels = allLoggers.map(logger => (logger, logger.getLevel))

        // Print warning before suspending logs
        println(s"⚠️  All loggers are suspended while $reason...")

        try {
            allLoggers.foreach(_.setLevel(Level.OFF))
            block
        } finally {
            originalLevels.foreach { case (logger, level) => logger.setLevel(level) }
        }
    }
}

/** TODO:
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
// CommandLabel — classification typeclass
// ===================================

/** Maps a command value to a short string label used for statistics.
  *
  * Unlike [[AnyCommand.toString]] (which may include per-instance data like block numbers), the
  * label should identify only the semantically relevant category of a command.
  *
  * @tparam Cmd
  *   the command type
  */
trait CommandLabel[Cmd]:
    def label(cmd: Cmd): String

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
    private val repr: String,
    val label: String
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
        cp: CommandProp[Cmd, Result, State],
        cl: CommandLabel[Cmd]
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
          repr = cmd.toString,
          label = cl.label(cmd)
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
      repr = "NoOp",
      label = "NoOp"
    )

// ===================================
// Command generation strategy
// ===================================

/** Abstracts the command-generation strategy. Different properties can supply different
  * implementations to drive generation differently (e.g. only invalid events, only minor blocks,
  * etc.).
  *
  * Scenario is a sequence of commands of some legth, where the tail typically contains NoOps.
  */
trait ScenarioGen[State, Sut]:
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

    private val logger = org.slf4j.LoggerFactory.getLogger("org.scalacheck.commands.ModelBasedSuite")
    private val loggerIO = Logging.loggerIO("org.scalacheck.commands.ModelBasedSuite")

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
    def scenarioGen: ScenarioGen[State, Sut]

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

    /** When true, command delays are driven via [[TestControl]]'s virtual clock (simulating days in
      * milliseconds). When false, delays are real [[IO.sleep]] (needed for Yaci/public backends).
      *
      * Inner IO runs on the TestControl runtime; outer driver runs on the real runtime. They
      * coordinate via an `AtomicReference[(FiniteDuration, Deferred[IO,Unit])]`: inner posts
      * `(delay, gate)` and blocks on `gate.get`; outer reads, advances the clock by exactly
      * `delay`, completes the gate, then calls `tickUntil` — which ticks until `tickOne → false`
      * (all actor fibers drained) before checking that the inner has posted the next signal.
      *
      * See `docs/testcontrol-driver.md` for full design documentation.
      */
    def useTestControl: Boolean

    // ===================================
    // Property entry point
    // ===================================

    /** A property that attests that SUT complies to the model.
      */
    final def property(): Prop = {

        val suts = collection.mutable.Map.empty[AnyRef, Option[State => IO[Sut]]]

        Prop.forAll(genTestCase) { testCase =>
            logger.info(
              "\n\n\n\n\n\n\n\n ---------------------------------------------- " +
                  "Executing the next test case..."
            )

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
                            logger.error("WARNING: you should never see that")
                            Prop.undecided
                        }

                    case None =>
                        // Here we might wait until canCreateNewSut is true
                        logger.error("WARNING: you should never see that")
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
                    // TODO: do we need that suchThat?
                    c <- scenarioGen.genNextCommand(s0) // .suchThat(_.preCondition(s0))
                    s1 = c.advanceState(s0)
                } yield (s1, cs :+ c)
            }
        }

        def precondition(targetState: State, tc: TestCase): Boolean = {
            // We don't want to see those logs again
            LoggingControl.withSuppressedLogs("precondition checking") {
                initialStatePreCondition(tc.initialState)
                && cmdsPrecond(tc.initialState, tc.commands)._2
                &&
                this.scenarioGen.targetStatePrecondition(targetState)
            }
        }

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
        logger.info(s"Sequential Commands:\n${prettyCmdsRes(testCase.commands, size)}\n")

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
            else ModelBasedSuite.recordTestCase(testCase.commands.map(_.label))
            Prop(_ => r)
        } :| "Property failed, see the log above for details"
    }

    private def prettyCmdsRes(rs: List[AnyCommand[State, Sut]], lastCmd: Int) = {
        def formatDuration(d: FiniteDuration): String = {
            val totalSeconds = d.toSeconds
            val hours = totalSeconds / 3600
            val minutes = (totalSeconds % 3600) / 60
            val seconds = totalSeconds % 60

            if hours > 0 then f"${hours}h${minutes}%02dm${seconds}%02ds"
            else if minutes > 0 then f"${minutes}m${seconds}%02ds"
            else f"${seconds}s"
        }

        val maxNumberWidth = "%d".format(lastCmd).length

        // Calculate cumulative times and find max time width
        val cumulativeTimes = rs.scanLeft(0.seconds)((acc, cmd) => acc + cmd.delay).tail
        val timeStrings = cumulativeTimes.map(formatDuration)
        val maxTimeWidth = if timeStrings.nonEmpty then timeStrings.map(_.length).max else 0

        val lineLayout = "  %%%ds  %%%dd. %%s".format(maxTimeWidth, maxNumberWidth)
        val cs = rs.zipWithIndex.zip(timeStrings).map { case ((r, i), timeStr) =>
            lineLayout.format(timeStr, i + 1, r)
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
            (sut, p, s, lastCmd) = initial
            result <- testCase.commands.foldLeft(IO.pure((sut, p, s, lastCmd, false))) {
                case (acc, c) =>
                    acc.flatMap { case (sut, p, s, lastCmd, hasFailed) =>
                        // Short-circuit: if the property has already failed, skip remaining commands
                        if hasFailed then IO.pure((sut, p, s, lastCmd, true))
                        else
                            IO.sleep(c.delay) >> c.runPC(sut).map { pred =>
                                val newProp = p && pred(s)
                                // Only check the NEW predicate, not the entire accumulated property
                                // Suppress logs during predicate evaluation and state advancement
                                val (newPredResult, newState) =
                                    LoggingControl.withSuppressedLogs(
                                      "predicate evaluation and state advancement"
                                    ) {
                                        val predResult = pred(s).apply(Gen.Parameters.default)
                                        val nextState = c.advanceState(s)
                                        (predResult, nextState)
                                    }
                                (sut, newProp, newState, lastCmd + 1, newPredResult.failure)
                            }
                    }
            }
            (sut, prop, s, lastCmd, _) = result
            shutdownProp <- shutdownSut(s, sut)
        } yield (sut, prop && shutdownProp, s, lastCmd, testCase)

        io.unsafeRunSync()
    }

    /** [[TestControl]]-based execution. Command delays are advanced via the virtual clock, allowing
      * the test to simulate hours of protocol time in milliseconds of wall time.
      *
      * ## Inner / outer split
      *
      * The inner [[IO]] runs on the TestControl runtime (virtual clock). The outer driver runs on
      * the real runtime and advances the virtual clock via [[TestControl#advance]].
      *
      * ## Communication protocol
      *
      * The two sides communicate via a pair of [[java.util.concurrent.atomic.AtomicReference]]s:
      *   - `pendingDelay`: inner posts `Some(delay)` here, then blocks on `gate`. Outer reads it to
      *     know how far to advance the virtual clock.
      *   - `gate`: outer sets to `true` after advancing; inner spins on it with [[IO.cede]].
      *
      * ## Per-command flow
      *
      * Inner:
      *   1. Allocate a fresh `Deferred[IO, Unit]` gate.
      *   2. Post `(delay, gate)` to `pendingDelay`.
      *   3. Block on `gate.get` — truly suspended, not immediately eligible.
      *   4. Run the command and evaluate the postcondition.
      *
      * Outer:
      *   1. Read `(delay, gate)` from `pendingDelay`.
      *   2. Advance the virtual clock by exactly `delay` (skipped when zero).
      *   3. `gate.complete(())` — unblock the inner.
      *   4. `tickUntil`: tick until `tickOne → false` (all fibers exhausted), then assert signal.
      *
      * `totalAdvanced` equals the sum of all command delays; no drift from `nextInterval`.
      */
    private def runCommandsWithTestControl(
        testCase: TestCase,
        startupSut: State => IO[Sut]
    ): (Sut, Prop, State, Int, TestCase) = {
        import java.util.concurrent.atomic.AtomicReference

        // Inner writes Some((delay, gate)) before blocking on gate.get.
        // Outer reads, advances clock, drains, then gate.complete(()) releases inner.
        val pendingDelay = new AtomicReference[Option[(FiniteDuration, Deferred[IO, Unit])]](None)

        logger.debug("Using TestControl to run the test case...")

        val innerIO: IO[(Sut, Prop, State, Int, TestCase)] = for {
            initial <- startupSut(testCase.initialState).map(sut =>
                (sut, Prop.proved, testCase.initialState, 0, false)
            )
            result <- testCase.commands.foldLeft(IO.pure(initial)) { case (acc, c) =>
                acc.flatMap { case (sut, p, s, lastCmd, hasFailed) =>
                    if hasFailed then IO.pure((sut, p, s, lastCmd, true))
                    else
                        for {
                            gate <- Deferred[IO, Unit]
                            _ <- IO(pendingDelay.set(Some((c.delay, gate))))
                            _ <- loggerIO.info("Suspend on the gate...")
                            _ <- gate.get
                            _ <- loggerIO.info("Running the command...")
                            pred <- c.runPC(sut)
                            (newPredResult, newState) = LoggingControl.withSuppressedLogs(
                              "predicate evaluation and state advancement"
                            ) {
                                val predResult = pred(s).apply(Gen.Parameters.default)
                                val nextState = c.advanceState(s)
                                (predResult, nextState)
                            }
                        } yield (sut, p && pred(s), newState, lastCmd + 1, newPredResult.failure)
                }
            }
            (sut, prop, s, lastCmd, _) = result
            // Sentinel: signal outer that all commands are done before entering shutdownSut.
            // shutdownSut calls waitForIdle → IO.sleep, which needs nextInterval advances.
            // The sentinel lets the outer switch from tickUntil (strict) to tickUntilAdvancing.
            shutdownGate <- Deferred[IO, Unit]
            _ <- IO(pendingDelay.set(Some((Duration.Zero, shutdownGate))))
            _ <- shutdownGate.get
            shutdownProp <- shutdownSut(s, sut)
        } yield (sut, prop && shutdownProp, s, lastCmd, testCase)

        // Outer driver: advances the virtual clock and drives tickOne loops between commands.
        // tickAll / tick are not used — they iterate indefinitely over the per-actor 1 s ping loop.
        // Instead, drainAll/tickUntil stop when the inner signals via pendingDelay (a Deferred gate).
        val outerIO: IO[(Sut, Prop, State, Int, TestCase)] = for {
            // 1. Start the inner on the mocked runtime. It's paused — nothing runs yet.
            tc <- TestControl.execute(innerIO)
            totalAdvanced <- IO(new java.util.concurrent.atomic.AtomicLong(0L))

            // 2. Pump until the first signal.
            // Tick the inner until it posts the first delay request (i.e. newSut has completed
            // and the first command's gate spin has started), or until the program finishes
            // (possible when cs is empty: newSut → shutdownSut with no commands in between).
            // Since we use IO.sleep to travel from the epoch start to the start time, we might
            // need to advance time - here is why we use tickUntilAdvancing here.
            _ <- tickUntilAdvancing(
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
                case Some(_) => loggerIO.info("empty command list, done")
                case None    =>
                    // 4. Otherwise, run per-command iteration.
                    // Note that the outer doesn't need the command object.
                    // It just needs to iterate the right number of times.
                    testCase.commands.foldLeft(IO.unit) { (acc, _u) =>
                        acc >> tc.results.flatMap {
                            case Some(_) => IO.unit // already finished, skip remaining
                            case None =>
                                for {
                                    // 5. Read (delay, gate) the inner posted and clear atomically.
                                    ret <- IO(pendingDelay.getAndSet(None).get)
                                    (delay, gate) = ret
                                    _ <- IO(totalAdvanced.addAndGet(delay.toNanos): Unit)

                                    // 6. Advance exactly `delay` (tc.advance requires > 0).
                                    _ <-
                                        if delay > Duration.Zero then tc.advance(delay) else IO.unit

                                    // 7. Release the inner to run the command.
                                    _ <- loggerIO.info("Opening the gate")
                                    _ <- gate.complete(())


                                    // 8. Tick until all fibers exhaust, then check next signal.
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
            // 9. Complete the sentinel gate (inner is blocked on it before shutdownSut), then
            // drain shutdown using tickUntilAdvancing — shutdownSut calls waitForIdle → IO.sleep.
            // If no sentinel (startup crashed / empty commands already resolved), just drain.
            _ <- IO(pendingDelay.getAndSet(None)).flatMap {
                case Some((_, gate)) => gate.complete(())
                case None            => IO.unit
            } >> tickUntilAdvancing(tc, tc.results.map(_.isDefined))

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

    /** Strict variant: ticks until `tickOne` returns `false` (all eligible fibers exhausted), then
      * checks `done`. If `done` is false at that point, raises an error — between commands the SUT
      * must always reach the next signal without needing a clock advance.
      * Never calls [[tickUntilAdvancing]].
      */
    private def tickUntil[A](tc: TestControl[A], done: IO[Boolean]): IO[Unit] =
        tc.tickOne.flatMap {
            case true  => tickUntil(tc, done)
            case false =>
                done.flatMap {
                    case true  => loggerIO.info("tickUntil is done")
                    case false =>
                        val msg =
                            "tickUntil: fibers exhausted but signal not received — SUT deadlock or unexpected IO.sleep"
                        loggerIO.error(msg) >> IO.raiseError(new RuntimeException(msg))
                }
        }

    /** Drives the inner until all immediately-eligible fibers are exhausted, then checks `done`.
      * Falls back to `tc.nextInterval` when no fiber is immediately eligible and `done` is false —
      * logs a warning each time, since this should only occur during startup (`IO.sleep` advances).
      */
    private def tickUntilAdvancing[A](tc: TestControl[A], done: IO[Boolean]): IO[Unit] =
        tc.tickOne.flatMap {
            case true => tickUntilAdvancing(tc, done)
            case false =>
                done.flatMap {
                    case true =>
                        loggerIO.info("tickUntilAdvancing is done")
                    case false =>
                        tc.nextInterval.flatMap { next =>
                            if next > Duration.Zero then
                                loggerIO.warn(
                                    s"tickUntilAdvancing: no eligible fibers — advancing $next to next timer"
                                ) >> tc.advance(next) >> tickUntilAdvancing(tc, done)
                            else {
                                val msg = "TestControl deadlock: no eligible fibers and predicate not satisfied"
                                loggerIO.error(msg) >> IO.raiseError(new RuntimeException(msg))
                            }
                        }
                }
        }

}

object ModelBasedSuite {
    private val totalSimulatedNanos = new java.util.concurrent.atomic.AtomicLong(0L)
    private val startNanoTime = System.nanoTime()

    // Accumulated stats from passed test cases: list of (sequenceLength, labelCounts)
    private val passedTestCases =
        new java.util.concurrent.CopyOnWriteArrayList[List[String]]()

    private[commands] def recordTestCase(labels: List[String]): Unit =
        passedTestCases.add(labels): Unit

    // TODO: make optional
    Runtime.getRuntime.addShutdownHook(new Thread {
        override def run(): Unit = {
            val simNanos = totalSimulatedNanos.get

            val simSecs = simNanos / 1_000_000_000L
            val simMins = simSecs / 60L
            val simHours = simMins / 60L
            val simDays = simHours / 24L
            val simRemHours = simHours % 24L
            val simRemMins = simMins % 60L
            val simRemSecs = simSecs % 60L

            val simTimeStr =
                if simDays > 0 then f"${simDays}d ${simRemHours}h ${simRemMins}m ${simRemSecs}s"
                else if simHours > 0 then f"${simHours}h ${simRemMins}m ${simRemSecs}s"
                else if simMins > 0 then f"${simMins}m ${simRemSecs}s"
                else f"${simSecs}s"

            val realNanos = System.nanoTime() - startNanoTime
            val realSecs = realNanos / 1_000_000_000L
            val realMins = realSecs / 60L
            val realRemSec = realSecs % 60L

            println
            println(
              s"---- TestControl ---- GRAND TOTAL simulated time: $simTimeStr (across all test cases)"
            )
            println(
              s"---- TestControl ---- GRAND TOTAL real time:      ${realMins}m ${realRemSec}s"
            )

            val cases = passedTestCases.toArray(Array.empty[List[String]])
            if cases.nonEmpty then {
                val lengths = cases.map(_.count(_ != "NoOp"))
                val totalCases = cases.length
                val avgLen = lengths.sum.toDouble / totalCases
                val maxLen = lengths.max

                val labelCounts = cases.flatten
                    .groupBy(identity)
                    .map { case (label, occurrences) => label -> occurrences.length }
                    .toList
                    .sortBy(_._1)

                val totalCommands = labelCounts.map(_._2).sum

                println
                println(s"---- Command stats ---- passed test cases: $totalCases")
                println(f"---- Command stats ---- sequence length: avg=${avgLen}%.1f  max=$maxLen")
                println(s"---- Command stats ---- command distribution (total $totalCommands):")
                val labelWidth = labelCounts.map(_._1.length).maxOption.getOrElse(0)
                labelCounts.foreach { case (label, count) =>
                    val pct = count.toDouble / totalCommands * 100
                    val paddedLabel = label.padTo(labelWidth, ' ')
                    println(f"  $paddedLabel  $count%6d  ($pct%5.1f%%)")
                }
            }
        }
    })

    private[commands] def addSimulatedNanos(nanos: Long): Unit =
        totalSimulatedNanos.addAndGet(nanos): Unit
}
