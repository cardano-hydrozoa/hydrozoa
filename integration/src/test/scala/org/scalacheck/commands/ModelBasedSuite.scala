package org.scalacheck.commands

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.syntax.all.catsSyntaxFlatMapOps
import org.scalacheck.{Gen, Prop, Shrink}
import org.slf4j.Logger
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
      runPC = _ => IO.println(">> NoOp").as(_ => Prop.passed),
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

// ===================================
// ModelBasedSuite
// ===================================

/** Yet another better/different Commands:
  *   - Support for CE [[IO]] and [[TestControl]]
  *   - Modular commands via typeclasses to separate concerns:
  *     - [[ModelCommand]]: state transitions, preconditions, scheduling
  *     - [[SutCommand]]: execution against the SUT
  *     - [[CommandProp]]: postcondition checks
  *   - [[AnyCommand]] erases the per-command [[Result]] type so the runner loop is monomorphic.
  *
  * Limitations:
  *   - Parallel commands have been removed - we need a different approach
  */
trait ModelBasedSuite {

    val logger: Logger = org.slf4j.LoggerFactory.getLogger(ModelBasedSuite.getClass)

    /** The model state type.  Must be immutable. */
    type State

    /** A type representing one instance of the system under test (SUT). */
    type Sut

    /** Decides if [[newSut]] should be allowed to be called with the specified state value. This
      * can be used to limit the number of co-existing [[Sut]] instances.
      *
      * If you want to allow only one [[Sut]] instance to exist at any given time (a singleton
      * [[Sut]]), implement this method the following way:
      *
      * {{{
      *  def canCreateNewSut(candidateState: State, inactiveSuts: Iterable[State]
      *    runningSuts: Iterable[State]
      *  ) = inactiveSuts.isEmpty && runningSuts.isEmpty
      * }}}
      */
    def canCreateNewSut(
        candidateState: State,
        inactiveSuts: Iterable[State],
        runningSuts: Iterable[State]
    ): Boolean

    /** Create a new [[Sut]] instance with an internal state that corresponds to the provided
      * abstract state instance.
      */
    def newSut(state: State): IO[Sut]

    /** Shutdown the SUT instance, and release any resources related to it. May also run some checks
      * upon shutting SUT down.
      */
    def shutdownSut(state: State, sut: Sut): IO[Prop]

    /** The precondition for the initial state, when no commands yet have run. */
    def initialPreCondition(state: State): Boolean = true

    /** A generator that should produce an initial [[State]] instance that is usable by [[newSut]]
      * to create a new system under test.
      */
    def genInitialState: Gen[State]

    /** A generator that, given the current model state, should produce a suitable command. The
      * command should always be well-formed, but MUST NOT necessarily be correct, i.e. it may be
      * expected to error upon running against the SUT. The correctness is indicated by
      * [[ModelCommand.preCondition]] and the expected behavior of SUT is checked based on that
      * flag.
      */
    def commandGen: CommandGen[State, Sut]

    /** Whether to use [[TestControl]] for time manipulation. When true (default), delays declared
      * by commands are advanced via the [[TestControl]] virtual clock rather than real
      * [[IO.sleep]]. Set to false for scenarios requiring a real backend (e.g. Yaci), where real
      * wall-clock time must elapse.
      *
      * TODO: make a parameter
      */
    def useTestControl: Boolean = true

    /** A property that attests that SUT complies to the model.
      *
      * The parameter [[threadCount]] specifies the number of commands that might be executed in
      * parallel. Defaults to one, which means the commands will only be run serially for the same
      * Sut instance.
      *
      * Distinct Sut instances might still receive commands in parallel, if the
      * [[Test.Parameters.workers]] parameter is larger than one.
      *
      * Setting [[threadCount]] higher than one enables ScalaCheck to reveal thread-related issues
      * in your system under test. When setting [[threadCount]] larger than one, ScalaCheck must
      * evaluate all possible command interleavings (and the end State instances they produce),
      * since parallel command execution is non-deterministic.
      *
      * ScalaCheck tries out all possible end states with the postcondition of the very last command
      * executed (there is always exactly one command executed after all parallel command
      * executions).
      *
      * If it fails to find an end state that satisfies the postcondition, the test fails. However,
      * the number of possible end states grows rapidly with increasing values of [[threadCount]].
      * Therefore, the lengths of the parallel command sequences are limited so that the number of
      * possible end states don't exceed [[maxParComb]]. The default value of [[maxParComb]] is
      * 1000000.
      */
    final def property(threadCount: Int = 1, maxParComb: Int = 1000000): Prop = {

        // TODO: saving IO[Sut] doesn't make much sense
        val suts = collection.mutable.Map.empty[AnyRef, (State, Option[IO[Sut]])]

        Prop.forAll(genActions(threadCount, maxParComb)) { as =>

            logger.info("Running the next test case...")

            logger.trace(
              as.seqCmds.foldRight(
                "---- Sequential actions ------------------------------------\n"
              )((cmd, acc) => s"$acc\t - $cmd\n")
            )

            try {
                val sutId = suts.synchronized {
                    val inactiveSuts = suts.values.collect { case (state, None) => state }
                    val runningSuts = suts.values.collect { case (state, Some(_)) => state }
                    if canCreateNewSut(as.initialState, inactiveSuts, runningSuts) then {
                        val sutId = new AnyRef
                        suts += (sutId -> (as.initialState -> None))
                        Some(sutId)
                    } else None
                }

                sutId match {
                    case Some(id) =>

                        if suts.contains(id) then {
                            val ioSut = newSut(as.initialState)
                            val _ = suts.put(id, as.initialState -> Some(ioSut))
                            runActions(ioSut, as)
                        } else {
                            logger.error("WARNING: you should never not see that")
                            Prop.undecided
                        }

                    case None =>
                        // TODO: Block until canCreateNewSut is true
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
    // Actions generation
    // ===================================

    private type Commands = List[AnyCommand[State, Sut]]

    /** @param initialState
      *   the initial state
      * @param seqCmds
      *   sequentional commands, come first
      * @param parCmds
      *   optional parallel commands, come after sequential commands
      */
    private case class Actions(
        initialState: State,
        seqCmds: Commands,
        parCmds: List[Commands]
    )

    /** Actions generator.
      */
    private def genActions(threadCount: Int, maxParComb: Int): Gen[Actions] = {
        import Gen.{const, listOfN, sized}

        /** Generates the sequence of commands of size [[size]] using initial state
          * [[initialState]].
          *
          * @return
          *   the FINAL state and the list of commands
          */
        def sizedCmds(initialState: State)(size: Int): Gen[(State, Commands)] = {
            val l: List[Unit] = List.fill(size)(())
            l.foldLeft(const((initialState, Nil: Commands))) { (g, _) =>
                for {
                    (s0, cs) <- g
                    // TODO: this implies I was wrong in the comment for preCondition!
                    c <- commandGen.genNextCommand(s0).suchThat(_.preCondition(s0))
                } yield (c.advanceState(s0), cs :+ c)
            }
        }

        /** Checks all preconditions and evaluates the final state.
          * @return
          *   the final state and the && over preconditions.
          */
        def cmdsPrecond(s: State, cmds: Commands): (State, Boolean) = cmds match {
            case Nil                          => (s, true)
            case c :: cs if c.preCondition(s) => cmdsPrecond(c.advanceState(s), cs)
            case _                            => (s, false)
        }

        def actionsPrecond(as: Actions): Boolean = {
            // TODO: why is that?
            as.parCmds.sizeIs != 1
            && as.parCmds.forall(_.nonEmpty)
            && initialPreCondition(as.initialState)
            && (cmdsPrecond(as.initialState, as.seqCmds) match {
                // The final state of seqCmds is used for parCmds if they exist
                case (s, true) => as.parCmds.forall(cmdsPrecond(s, _)._2)
                case _         => false
            })
        }

        /** The size of parallel commands is determined by the number of parallel threads
          * [[threadCount]] and the limit of all possible state combinations [[maxParComb]].
          *
          *  This is a conservative overestimate of the actual state space, used to limit commands
          *  generation.
          *
          * The Key Insight
          *
          * When you have n threads running in parallel, and you're tracking their
          * relative execution order, here's what happens:
          *
          * Thread 1 (i=1)
          *
          *   - It's the first thread, so there's only 1 way it can be ordered (it's
          *     alone)
          *   - Across m rounds: 1 × 1 × ... × 1 = 1^m = 1 possibility
          *
          * Thread 2 (i=2)
          *
          *   - Thread 2 can be either:
          *     - Before thread 1, or
          *     - After thread 1
          *   - That's 2 positions for thread 2
          *   - In each of m rounds, thread 2 can be in either position
          *   - Total: 2 × 2 × ... × 2 = 2^m possibilities
          *
          * Thread 3 (i=3)
          *
          *   - Thread 3 can be:
          *     - Position 1: Before both thread 1 and thread 2
          *     - Position 2: Between thread 1 and thread 2
          *     - Position 3: After both thread 1 and thread 2
          *   - That's 3 positions for thread 3
          *   - In each of m rounds, thread 3 independently chooses one of these 3
          *     positions
          *   - Total: 3 × 3 × ... × 3 = 3^m possibilities
          *
          * Thread i (general case)
          *
          *   - When thread i joins, there are already (i-1) threads
          *   - Thread i can be inserted in i different positions:
          *     - Before all existing threads
          *     - Between any two existing threads (i-2 positions)
          *     - After all existing threads
          *     - Total: 1 + (i-2) + 1 = i positions
          *   - Across m rounds, thread i makes this choice m times
          *   - Total: i^m possibilities
          *
          * Each thread's choices are independent of other threads' choices (in
          * terms of counting possibilities). So we use the multiplication principle:
          *
          * Total combinations =
          *             (choices for thread 1) × (choices for thread 2) × ...
          *               × (choices for thread n)
          *     = 1^m × 2^m × 3^m × ... × n^m
          *     = ∏(i=1 to n) i^m
          *     = (n!)^m
          *
          * Concrete Example: n=3, m=2
          *
          * Let's verify with 3 threads, 2 rounds:
          *
          * Round 1 choices:
          *   - Thread 1: 1 position
          *   - Thread 2: 2 positions (before/after thread 1)
          *   - Thread 3: 3 positions (before all, between 1&2, after all)
          *
          * Round 2 choices:
          *   - Same: 1 × 2 × 3 possibilities
          *
          * Total across both rounds:
          * (1 × 2 × 3) × (1 × 2 × 3) = 6 × 6 = 36
          *
          * {{{
          *       m=1    m=2      m=3        m=4           m=5
          *  n=1    1      1        1          1             1
          *  n=2    2      4        8         16            32
          *  n=3    6     36      216       1296          7776
          *  n=4   24    576    13824     331776       7962624
          *  n=5  120  14400  1728000  207360000   24883200000
          * }}}
          */
        val parSz = {

            /** The upper bound on the number of distinct state combinations that can arise from
              * [[n]] threads each executing [[m]] parallel commands. It's used to prevent
              * generating too many test cases by limiting parSz (parallel size) based on
              * maxParComb.
              *
              * @param n
              * @param m
              * @return
              */
            def seqs(n: Long, m: Long): Long =
                if n == 1 then 1 else math.round(math.pow(n.toDouble, m.toDouble)) * seqs(n - 1, m)

            if threadCount < 2 then 0
            else {
                var parSz = 1
                while seqs(threadCount.toLong, parSz.toLong) < maxParComb do parSz += 1
                parSz
            }
        }

        val g = for {
            s0 <- genInitialState
            (s1, seqCmds) <- sized(sizedCmds(s0))
            // TODO: remove
            parCmds <-
                if parSz <= 0
                then const(Nil)
                else listOfN(threadCount, sizedCmds(s1)(parSz).map(_._2))
        } yield Actions(s0, seqCmds, parCmds)

        g.suchThat(actionsPrecond)
    }

    // ===================================
    // Actions runner
    // ===================================

    private def runActions(ioSut: IO[Sut], as: Actions): Prop = {

        val maxLength = as.parCmds.map(_.length).foldLeft(as.seqCmds.length)(_.max(_))
        val (_sut, p, s, lastCmd) = runSeqCmds(ioSut, as.initialState, as.seqCmds)

        p.flatMap { r =>
            if r.failure then
                logger.warn(
                  "Property is falsified (see the labels down below). Additional information: \n" +
                      s"Initial state:\n  ${as.initialState}\n" +
                      s"Last state:\n  ${s}\n" +
                      s"Sequential Commands:\n${prettyCmdsRes(as.seqCmds, maxLength)}\n" +
                      s"Last executed command: $lastCmd"
                )
            Prop(_ => r)
        } :| "Property failed, see the log for details"
    }

    /** Short-circuit property AND operator. (Should maybe be in Prop module) */
    private def propAnd(p1: => Prop, p2: => Prop) = p1.flatMap { r =>
        if r.success then Prop.secure(p2) else Prop(_ => r)
    }

    private def prettyCmdsRes(rs: List[AnyCommand[State, Sut]], maxLength: Int) = {
        val maxNumberWidth = "%d".format(maxLength).length
        val lineLayout = "  %%%dd. %%s".format(maxNumberWidth)
        val cs = rs.zipWithIndex.map { case (r, i) =>
            lineLayout.format(i + 1, r)
        }
        if cs.isEmpty then "  <no commands>"
        else cs.mkString("\n")
    }

    // TODO: Added the last succeeded command (last `Int`) - do we need it though? It doesn't work properly now.
    private def runSeqCmds(
        ioSut: IO[Sut],
        s0: State,
        cs: Commands
    ): (Sut, Prop, State, Int) = {
        if useTestControl then runSeqCmdsWithTestControl(ioSut, s0, cs)
        else runSeqCmdsPlain(ioSut, s0, cs)
    }

    /** Plain execution without [[TestControl]]. Command delays are real [[IO.sleep]]s. Used for
      * backends like Yaci where wall-clock time must elapse.
      */
    private def runSeqCmdsPlain(
        ioSut: IO[Sut],
        s0: State,
        cs: Commands
    ): (Sut, Prop, State, Int) = {
        val io = for {
            initial <- ioSut.map(sut => (sut, Prop.proved, s0, 0))
            result <- cs.foldLeft(IO.pure(initial)) { case (acc, c) =>
                acc >>= { case (sut, p, s, lastCmd) =>
                    IO.sleep(c.delay) >> c.runPC(sut).map { pred =>
                        (sut, propAnd(p, pred(s)), c.advanceState(s), lastCmd + 1)
                    }
                }
            }
            (sut, prop, s, lastCmd) = result
            shutdownProp <- shutdownSut(s, sut)
        } yield (sut, propAnd(prop, shutdownProp), s, lastCmd)

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
    private def runSeqCmdsWithTestControl(
        ioSut: IO[Sut],
        s0: State,
        cs: Commands
    ): (Sut, Prop, State, Int) = {
        import java.util.concurrent.atomic.AtomicReference

        // Shared mutable state between inner and outer runtimes.
        // The inner writes a Some(delay) to request a time advance before its command runs.
        // None means "not ready yet" / "waiting for outer to tick".
        val pendingDelay = new AtomicReference[Option[FiniteDuration]](None)
        // The outer sets this to true to release the inner after advancing time.
        val gate = new AtomicReference[Boolean](false)

        // How long to let actors settle (process pending messages) before each delay
        // advance. The inner sleeps for this duration first; the outer ticks through it
        // (giving actors their ping cycles), then reads the declared delay and advances
        // only the remainder (delay - settling). Total virtual time per command = delay.
        val settling = 1.seconds

        // The inner program: newSut, then for each command:
        //   1. sleep(settling) — keeps inner busy while outer ticks actors
        //   2. signal delay — outer reads and advances (delay - settling)
        //   3. wait for gate — outer releases after advancing
        //   4. run command
        // Finally shutdownSut.
        val innerIO: IO[(Sut, Prop, State, Int)] = for {
            initial <- ioSut.map(sut => (sut, Prop.proved, s0, 0))
            result <- cs.foldLeft(IO.pure(initial)) { case (acc, c) =>
                acc >>= { case (sut, p, s, lastCmd) =>
                    // If the command declares a delay large enough, sleep first so the
                    // outer can tick actors during the settling window.
                    (if c.delay >= settling then IO.sleep(settling) else IO.unit) >>
                        IO(pendingDelay.set(Some(c.delay))) >>
                        IO.cede.whileM_(IO(!gate.get)) >>
                        IO(gate.set(false)) >>
                        c.runPC(sut).map { pred =>
                            (sut, propAnd(p, pred(s)), c.advanceState(s), lastCmd + 1)
                        }
                }
            }
            (sut, prop, s, lastCmd) = result
            shutdownProp <- shutdownSut(s, sut)
        } yield (sut, propAnd(prop, shutdownProp), s, lastCmd)

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
        val outerIO: IO[(Sut, Prop, State, Int)] = for {
            tc <- TestControl.execute(innerIO)
            totalAdvanced <- IO(new java.util.concurrent.atomic.AtomicLong(0L))

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

            // Drive each command: read delay → advance → release gate → tick until next signal.
            // If the program already finished (e.g. cs was empty), skip the command loop.
            _ <- tc.results.flatMap {
                case Some(_) => IO.unit
                case None =>
                    cs.foldLeft(IO.unit) { (acc, _c) =>
                        acc >> tc.results.flatMap {
                            case Some(_) => IO.unit // already finished, skip remaining
                            case None =>
                                for {
                                    delay <- IO(pendingDelay.getAndSet(None).get)
                                    _ <- IO(totalAdvanced.addAndGet(delay.toNanos): Unit)
                                    // The inner already slept for `settling` before signalling
                                    // this delay (when delay >= settling), so only advance the
                                    // remainder. For small delays the inner skipped the sleep,
                                    // so advance the full amount.
                                    _ <- {
                                        val remaining =
                                            if delay >= settling then delay - settling else delay
                                        if remaining > Duration.Zero then tc.advance(remaining)
                                        else IO.unit
                                    }
                                    // Release the inner to execute the command.
                                    _ <- IO(gate.set(true))
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
            // If results aren't available yet (shouldn't happen, but be safe), drain remaining.
            _ <- tickUntil(tc, tc.results.map(_.isDefined))
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
                        // No immediately eligible fibers. Advance to the next scheduled task
                        // (e.g. a ping or a sleep) so we don't spin forever.
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
