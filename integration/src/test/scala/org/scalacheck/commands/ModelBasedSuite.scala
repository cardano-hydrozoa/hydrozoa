package org.scalacheck.commands

import cats.MonadThrow
import cats.data.StateT
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Resource}
import cats.syntax.all.*
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, debug, error, info, warn}
import org.scalacheck.{Gen, Prop, PropertyM}
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}

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

    /** Returns the result and the new [[State]] after this command has run. The effect monad [[M]]
      * and tracer are supplied by the caller.
      */
    def runState[M[_]: MonadThrow](cmd: Cmd)(using ContraTracer[M, Slf4jMsg]): StateT[M, State, Result]

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

    def postCondition[M[_]: MonadThrow](
        cmd: Cmd,
        stateBefore: State,
        result: Either[Throwable, Result]
    )(using mc: ModelCommand[Cmd, Result, State], tracer: ContraTracer[M, Slf4jMsg]): M[Prop] =
        mc.runState[M](cmd).run(stateBefore).map { case (stateAfter, expectedResult) =>
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
// AdvanceState — rank-2 state-advance function
// ===================================

/** Rank-2 advance-state function: the caller supplies the effect monad [[M]] and a matching
  * tracer at use time.
  */
trait AdvanceState[State]:
    def apply[M[_]: MonadThrow](state: State)(using ContraTracer[M, Slf4jMsg]): M[State]

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
    /** Advances the model state (rank-2: caller chooses M and tracer). */
    val advanceState: AdvanceState[State],
    /** Time delay to advance before running. */
    // TODO: this is used in SUT but comes from CommandModel instance which is misleading
    val delay: FiniteDuration,
    /** Runs the command against the SUT and returns a postcondition function. The tracer is
      * supplied at call time — the runner decides whether model-side logging is emitted.
      */
    val runPC: (Sut, ContraTracer[IO, Slf4jMsg]) => IO[State => IO[Prop]],
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
          advanceState = new AdvanceState[State]:
              def apply[M[_]: MonadThrow](s: State)(using ContraTracer[M, Slf4jMsg]): M[State] =
                  mc.runState[M](cmd).runS(s),
          delay = mc.delay(cmd),
          runPC = (sut, tracer) =>
              given ContraTracer[IO, Slf4jMsg] = tracer
              sc.run(cmd, sut).attempt.map { r => (s: State) =>
                  if mc.preCondition(cmd, s) then cp.postCondition[IO](cmd, s, r)
                  else IO.pure(Prop.passed)
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
      advanceState = new AdvanceState[State]:
          def apply[M[_]: MonadThrow](s: State)(using ContraTracer[M, Slf4jMsg]): M[State] =
              MonadThrow[M].pure(s),
      delay = Duration.Zero,
      runPC = (_, _) => IO.pure(_ => IO.pure(Prop.passed)),
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
    def genNextCommand(state: State): PropertyM[IO, AnyCommand[State, Sut]]

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

    private given log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(
          Slf4jMsgFormat.humanFormat("org.scalacheck.commands.ModelBasedSuite")
        )

    /** Represent [some parts of] the environment on which a test case is run.
      *
      * The flow of every test case is as follows (a bit simplified):
      *
      * initEnv -> Env -> genInitState -> State -> sutResource -> Sut -> ... Command.runPC ->
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
    def initEnv: PropertyM[IO, Env]

    /** A generator that should produce an initial [[State]] instance that is usable by
      * [[sutResource]] to create a new system under test.
      *
      * TODO: I guess we may want to have multiple initial state generator/preonditions
      */
    def genInitialState(env: Env): PropertyM[IO, State]

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

    /** Hook fired once per test case after the command sequence has been generated and before SUT
      * startup. The default implementation logs the flat command sequence with cumulative time;
      * suites can override to print extra diagnostics (e.g. a per-peer command table) and may call
      * `super.onTestCaseGenerated(...)` to keep the default log alongside their own.
      */
    def onTestCaseGenerated(initialState: State, commands: List[AnyCommand[State, Sut]]): IO[Unit] =
        log.info(s"Sequential Commands:\n${prettyCmdsRes(commands, commands.size)}\n")

    // ===================================
    // SUT
    // ===================================

    /** Decides if [[sutResource]] should be allowed to be called with the specified state value.
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
      * abstract state instance. Returned as a `Resource` so cleanup (e.g. open ports, RocksDB
      * handles) runs even when a command throws or the test is cancelled before [[shutdownSut]].
      */
    def sutResource(state: State): Resource[IO, Sut]

    /** Drain the SUT and run final assertions before the [[sutResource]] resource is finalized.
      *
      * Called inside the `Resource.use` block after all commands have run. `lastState` is available
      * for assertions and to size drain timeouts. Cleanup that does not need `lastState` (fiber
      * cancellation, server shutdown, actor system termination) belongs in the [[sutResource]]
      * `Resource` finalizer instead.
      *
      * The default implementation returns `Prop.proved`; suites that put all cleanup in the
      * [[sutResource]] finalizer do not need to override this.
      */
    def beforeFinalize(lastState: State, sut: Sut): IO[Prop] = IO.pure(Prop.proved)

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
        val suts = collection.mutable.Map.empty[AnyRef, Option[State => Resource[IO, Sut]]]
        given (Prop => Prop) = identity

        PropertyM.monadicIO {
            for {
                testCase <- genTestCase
                prop     <- PropertyM.run {
                    for {
                        _ <- log.info(
                               "\n\n\n\n\n\n\n\n ---------------------------------------------- " +
                                   "Executing the next test case..."
                             )
                        sutId <- IO {
                                     suts.synchronized {
                                         if canStartupNewSut() then {
                                             val sutId = new AnyRef
                                             suts += (sutId -> None)
                                             Some(sutId)
                                         } else None
                                     }
                                 }
                        prop <- sutId match {
                                    case Some(id) =>
                                        if suts.contains(id) then {
                                            val _ = suts.put(id, Some(sutResource))
                                            runTestCase(testCase, sutResource)
                                                .handleErrorWith { e =>
                                                    IO(suts.synchronized(suts.clear())) >>
                                                        IO.raiseError(e)
                                                }
                                        } else
                                            log.error("WARNING: you should never see that")
                                                .as(Prop.undecided)
                                    case None =>
                                        log.error("WARNING: you should never see that")
                                            .as(Prop.undecided)
                                }
                    } yield prop
                }
            } yield prop
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

    /** Test case generator. */
    private def genTestCase: PropertyM[IO, TestCase] = {
        import PropertyM.{pick, pre, run}
        import Gen.sized

        def sizedCmds(initialState: State)(size: Int): PropertyM[IO, (State, Commands)] =
            List.fill(size)(()).foldLeft[PropertyM[IO, (State, Commands)]](
              run(IO.pure((initialState, Nil: Commands)))
            ) { (pm, _) =>
                for {
                    sc       <- pm
                    (s0, cs) = sc
                    c        <- scenarioGen.genNextCommand(s0)
                    s1       <- run(c.advanceState[IO](s0))
                } yield (s1, cs :+ c)
            }

        def cmdsPrecond(s: State, cmds: Commands): IO[Boolean] = cmds match {
            case Nil                          => IO.pure(true)
            case c :: cs if c.preCondition(s) =>
                c.advanceState[IO](s).flatMap(s1 => cmdsPrecond(s1, cs))
            case _                            => IO.pure(false)
        }

        for {
            env             <- initEnv
            s0              <- genInitialState(env)
            n               <- pick[IO, Int](commandGenTweaker[Int](sized(n => Gen.const(n))))
            sc              <- sizedCmds(s0)(n)
            (s, seqCmds)    = sc
            tc               = TestCase(s0, seqCmds)
            _               <- pre[IO](initialStatePreCondition(tc.initialState))
            precondMet      <- run(cmdsPrecond(tc.initialState, tc.commands))
            _               <- pre[IO](precondMet)
            _               <- pre[IO](scenarioGen.targetStatePrecondition(s))
        } yield tc
    }

    // ===================================
    // Test case runner
    // ===================================

    private def runTestCase(
        testCase: TestCase,
        sutResource: State => Resource[IO, Sut]
    ): IO[Prop] = for {
        _ <- onTestCaseGenerated(testCase.initialState, testCase.commands)
        result <-
            if useTestControl
            then runCommandsWithTestControl(testCase, sutResource)
            else runCommandsPlain(testCase, sutResource)
        (_sut, prop, s, lastCmd, _) = result
        r = prop.apply(Gen.Parameters.default)
        _ <- if r.failure then
                 log.warn(
                   "Property is falsified (see the labels down below). Additional information: \n" +
                       s"Initial state:\n  ${testCase.initialState}\n" +
                       s"Last state:\n  ${s}\n" +
                       s"Sequential Commands:\n${prettyCmdsRes(testCase.commands, lastCmd)}\n" +
                       s"Last executed command: $lastCmd"
                 )
             else IO(ModelBasedSuite.recordTestCase(testCase.commands.map(_.label)))
    } yield prop :| "Property failed, see the log above for details"

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
        sutResource: State => Resource[IO, Sut]
    ): IO[(Sut, Prop, State, Int, TestCase)] = for {
        _      <- log.debug("Using plain IO to run the test case...")
        result <- sutResource(testCase.initialState).use { sut =>
                      val initial = (sut, Prop.proved: Prop, testCase.initialState, 0, false)
                      for {
                          result <- testCase.commands.foldLeft(IO.pure(initial)) { case (acc, c) =>
                                        acc.flatMap { case (sut, p, s, lastCmd, hasFailed) =>
                                            if hasFailed then IO.pure((sut, p, s, lastCmd, true))
                                            else for {
                                                _        <- IO.sleep(c.delay)
                                                // Predicate evaluation re-runs the model-side
                                                // runState internally; advanceState walks the
                                                // model forward. Silence model-side logs from
                                                // both — they would duplicate the per-command
                                                // logs already emitted by the SUT side.
                                                pred     <- c.runPC(sut, ContraTracer.nullTracer)
                                                pp       <- pred(s)
                                                newState <- c.advanceState[IO](s)(using
                                                                MonadThrow[IO],
                                                                ContraTracer.nullTracer
                                                            )
                                                predResult = pp.apply(Gen.Parameters.default)
                                            } yield (sut, p && pp, newState, lastCmd + 1, predResult.failure)
                                        }
                                    }
                          (sut, prop, s, lastCmd, _) = result
                          beforeFinalizeProp <- beforeFinalize(s, sut)
                      } yield (sut, prop && beforeFinalizeProp, s, lastCmd, testCase)
                  }
    } yield result

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
        sutResource: State => Resource[IO, Sut]
    ): IO[(Sut, Prop, State, Int, TestCase)] = {
        import java.util.concurrent.atomic.AtomicReference

        // Inner writes Some((delay, gate)) before blocking on gate.get.
        // Outer reads, advances clock, drains, then gate.complete(()) releases inner.
        val pendingDelay = new AtomicReference[Option[(FiniteDuration, Deferred[IO, Unit])]](None)

        val innerIO: IO[(Sut, Prop, State, Int, TestCase)] =
            sutResource(testCase.initialState).use { sut =>
                val initial = (sut, Prop.proved: Prop, testCase.initialState, 0, false)
                for {
                    result <- testCase.commands.foldLeft(IO.pure(initial)) { case (acc, c) =>
                                  acc.flatMap { case (sut, p, s, lastCmd, hasFailed) =>
                                      if hasFailed then IO.pure((sut, p, s, lastCmd, true))
                                      else for {
                                          gate     <- Deferred[IO, Unit]
                                          _        <- IO(pendingDelay.set(Some((c.delay, gate))))
                                          _        <- log.info("Suspend on the gate...")
                                          _        <- gate.get
                                          _        <- log.info("Running the command...")
                                          // Silence model-side logs from predicate evaluation +
                                          // state advancement — see runCommandsPlain for the
                                          // rationale.
                                          predFn   <- c.runPC(sut, ContraTracer.nullTracer)
                                          pp       <- predFn(s)
                                          newState <- c.advanceState[IO](s)(using
                                                          MonadThrow[IO],
                                                          ContraTracer.nullTracer
                                                      )
                                          predResult = pp.apply(Gen.Parameters.default)
                                      } yield (sut, p && pp, newState, lastCmd + 1, predResult.failure)
                                  }
                              }
                    (sut, prop, s, lastCmd, _) = result
                    // Sentinel: signal outer that all commands are done before entering shutdownSut.
                    // shutdownSut calls waitForIdle → IO.sleep, which needs nextInterval advances.
                    // The sentinel lets the outer switch from tickUntil (strict) to tickUntilAdvancing.
                    shutdownGate <- Deferred[IO, Unit]
                    _            <- IO(pendingDelay.set(Some((Duration.Zero, shutdownGate))))
                    _            <- shutdownGate.get
                    beforeFinalizeProp <- beforeFinalize(s, sut)
                } yield (sut, prop && beforeFinalizeProp, s, lastCmd, testCase)
            }

        // Outer driver: advances the virtual clock and drives tickOne loops between commands.
        // tickAll / tick are not used — they iterate indefinitely over the per-actor 1 s ping loop.
        // Instead, drainAll/tickUntil stop when the inner signals via pendingDelay (a Deferred gate).
        for {
            _ <- log.debug("Using TestControl to run the test case...")
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
              },
              advanceTracer = log
            )

            // 3. Guard: if the inner already finished (empty command list), skip the loop.
            // Drive each command: read delay → advance → release gate → tick until next signal.
            // If the program already finished (e.g. cs was empty), skip the command loop.
            _ <- tc.results.flatMap {
                case Some(_) => log.info("empty command list, done")
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
                                    _ <- log.info("Opening the gate")
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
            } >> tickUntilAdvancing(
              tc,
              tc.results.map(_.isDefined),
              advanceTracer = ContraTracer.nullTracer
            )

            // 10. Extract the result
            result <- tc.results
            days   <- IO {
                          val nanos = totalAdvanced.get
                          ModelBasedSuite.addSimulatedNanos(nanos)
                          nanos / 86_400_000_000_000L
                      }
            _      <- log.info(s"---- TC ---- seed: ${tc.seed}  simulated: ${days} days")
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
    }

    /** Strict variant: ticks until `tickOne` returns `false` (all eligible fibers exhausted), then
      * checks `done`. If `done` is false at that point, raises an error — between commands the SUT
      * must always reach the next signal without needing a clock advance. Never calls
      * [[tickUntilAdvancing]].
      */
    private def tickUntil[A](tc: TestControl[A], done: IO[Boolean]): IO[Unit] =
        tc.tickOne.flatMap {
            case true => tickUntil(tc, done)
            case false =>
                done.flatMap {
                    case true => log.info("tickUntil is done")
                    case false =>
                        val msg =
                            "tickUntil: fibers exhausted but signal not received — SUT deadlock or unexpected IO.sleep"
                        log.error(msg) >> IO.raiseError(new RuntimeException(msg))
                }
        }

    /** Drives the inner until all immediately-eligible fibers are exhausted, then checks `done`.
      * Falls back to `tc.nextInterval` when no fiber is immediately eligible and `done` is false.
      *
      * Used in two legitimate phases: the startup pump (fast-forwards `IO.sleep` to the current
      * time — one or two advances per trial, worth logging) and the shutdown drain (`waitForIdle`
      * polls with `IO.sleep`, per-actor 1-second ping loops fire — hundreds to thousands of small
      * advances per trial, would drown the log). The caller passes the tracer that the per-advance
      * warn should route through: [[log]] from the startup pump, [[ContraTracer.nullTracer]] from
      * the shutdown drain. A genuine deadlock (no eligible fibers + `nextInterval == 0`) is still
      * surfaced via the framework [[log]] and `IO.raiseError` regardless of the passed tracer.
      */
    private def tickUntilAdvancing[A](
        tc: TestControl[A],
        done: IO[Boolean],
        advanceTracer: ContraTracer[IO, Slf4jMsg]
    ): IO[Unit] =
        tc.tickOne.flatMap {
            case true => tickUntilAdvancing(tc, done, advanceTracer)
            case false =>
                done.flatMap {
                    case true =>
                        log.info("tickUntilAdvancing is done")
                    case false =>
                        tc.nextInterval.flatMap { next =>
                            if next > Duration.Zero then
                                advanceTracer.warn(
                                  s"tickUntilAdvancing: no eligible fibers — advancing $next to next timer"
                                ) >> tc.advance(next) >> tickUntilAdvancing(tc, done, advanceTracer)
                            else {
                                val msg =
                                    "TestControl deadlock: no eligible fibers and predicate not satisfied"
                                log.error(msg) >> IO.raiseError(new RuntimeException(msg))
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
