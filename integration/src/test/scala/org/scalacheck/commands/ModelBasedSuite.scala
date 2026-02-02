package org.scalacheck.commands

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.catsSyntaxFlatMapOps
import org.scalacheck.{Gen, Prop, Shrink}

/** Yet another better/different [[Commands]]:
  *   - Support for [[IO]] genActions and [[TestControl]]
  *   - Haskell State-like commands with runState
  *   - Readable and pretty output
  *
  * Limitations:
  *   - Parallel commands are not supported yet.
  */
trait ModelBasedSuite {

    /** The model state type. Must be immutable. The [[State]] type should model (some part) of the
      * system under test (SUT).
      *
      * It should only contain details needed for command generation, specifying our pre- and
      * post-conditions, and for creating SUT instances.
      */
    type State

    /** A type representing one instance of the system under test (SUT). The [[Sut]] type should be
      * a proxy/facade to the actual system under test and is therefore, by definition, a mutable
      * type. It is used by the [[Command.run]] method to execute commands in the system under test.
      * It should be possible to have any number of co-existing instances of the [[Sut]] type, as
      * long as [[canCreateNewSut]] isn't violated, and each [[Sut]] instance should be a proxy to a
      * distinct SUT instance. There should be no dependencies between the [[Sut]] instances, as
      * they might be used in parallel by ScalaCheck. [[Sut]] instances are created by [[newSut]]
      * and destroyed by [[shutdownSut]]. [[newSut]] and [[shutdownSut]] might be called at any time
      * by ScalaCheck, as long as [[canCreateNewSut]] isn't violated.
      */
    type Sut

    /** Decides if [[newSut]] should be allowed to be called with the specified state value. This
      * can be used to limit the number of co-existing [[Sut]] instances. The list of existing
      * states represents the initial states (not the current states) for all [[Sut]] instances that
      * are active for the moment. If this method is implemented incorrectly, for example if it
      * returns false even if the list of existing states is empty, ScalaCheck might hang.
      *
      * If you want to allow only one [[Sut]] instance to exist at any given time (a singleton
      * [[Sut]]), implement this method the following way:
      *
      * {{{
      *  def canCreateNewSut(candidateState: State, inactiveSuts: Iterable[State]
      *    runningSuts: Iterable[State]
      *  ) = inactiveSuts.isEmpty && runningSuts.isEmpty
      * }}}
      *
      * @param candidateState
      *   the initial state of the SUT that the suite wants to create
      * @param inactiveSuts
      *   the initial states for SUTs that are planned to be run
      * @param runningSuts
      *   the initial states for SUTs that are currently active
      */
    def canCreateNewSut(
        candidateState: State,
        inactiveSuts: Iterable[State],
        runningSuts: Iterable[State]
    ): Boolean

    /** Create a new [[Sut]] instance with an internal state that corresponds to the provided
      * abstract state instance.
      *
      * The provided state is guaranteed to fulfill [[initialPreCondition]], and [[newSut]] will
      * never be called if [[canCreateNewSut]] is not true for the [[state]].
      */
    def newSut(state: State): IO[Sut]

    /** Shutdown the SUT instance, and release any resources related to it. May also run some checks
      * upon shutting SUT down.
      */
    def shutdownSut(sut: Sut): IO[Prop]

    /** The precondition for the initial state, when no commands yet have run.
      *
      * This is used by ScalaCheck when command sequences are shrunk and the first state might
      * differ from what is returned from [[genInitialState]].
      */
    def initialPreCondition(state: State): Boolean

    /** A generator that should produce an initial [[State]] instance that is usable by [[newSut]]
      * to create a new system under test. The state returned by this generator is always checked
      * with the [[initialPreCondition]] method before it is used.
      */
    def genInitialState: Gen[State]

    /** A generator that, given the current model state, should produce a suitable Command instance.
      * The command should always be well-formed, but MUST NOT be correct, i.e. it may be expected
      * to error upon running against the SUT. The correctness is indicated by
      * [[Command.preCondition]] and the expected behavior of SUT is checked based on that flag.
      */
    def genCommand(state: State): Gen[Command]

    // ===================================
    // StateLikeCommand
    // ===================================

    trait Command:

        /** A result of running the command against the SUT. The [[Result]] type should be
          * immutable, and it should encode everything about the command run that is necessary to
          * know in order to correctly implement the [[Command.postCondition]] method.
          */
        type Result

        // TODO: rework, rename num

        /** The sequential number of command. */
        def id: Int

        /** Executes the command against the SUT and returns the result of the command run. The
          * result value is later used for verifying that the command behaved according to the
          * specification, by the [[Command.postCondition]] method.
          */
        def run(sut: Sut): IO[Result]

        /** Returns a new [[State]] instance that represents the state of the system after this
          * command has run, given the system was in the provided state before the run.
          *
          * @param state
          * @return
          */
        def runState(state: State): (Result, State)

        /** Precondition that decides if this command is allowed to run when the system under test
          * is in the provided state.
          */
        def preCondition(state: State): Boolean

        /** Postcondition that decides if this command produced the correct result or not, given the
          * system was in the provided state before the command ran.
          */
        def postCondition(stateBefore: State, result: Either[Throwable, Result]): Prop =

            val (expectedResult, stateAfter) = runState(stateBefore)
            result match
                case Right(realResult: Result) =>
                    onSuccessCheck(
                      expectedResult,
                      stateBefore,
                      stateAfter,
                      realResult
                    )
                case Left(e) =>
                    onFailureCheck(expectedResult, stateBefore, stateAfter, e)

        def onSuccessCheck(
            expectedResult: Result,
            stateBefore: State,
            stateAfter: State,
            result: Result
        ): Prop = Prop.passed

        def onFailureCheck(
            expectedResult: Result,
            stateBefore: State,
            stateAfter: State,
            err: Throwable
        ): Prop = Prop.exception(err)

        /** Wraps the run and postCondition methods in order not to leak the dependent Result type.
          * @param sut
          * @return
          *   the predicate on State in IO
          */
        private[commands] def runPC(sut: Sut): IO[State => Prop] = {
            import Prop.propBoolean
            for {
                r <- run(sut).attempt
            } yield s => preCondition(s) ==> postCondition(s, r)
        }

    /** A command that never should throw an exception on execution. */
    trait SuccessCommand extends Command {
        def postCondition(state: State, result: Result): Prop

        final override def postCondition(state: State, result: Either[Throwable, Result]): Prop =
            result match {
                case Left(e)  => Prop.exception(e)
                case Right(r) => postCondition(state, r)
            }
    }

    /** A command that doesn't return a result, only succeeds or fails. */
    trait UnitCommand extends Command {
        final type Result = Unit

        def postCondition(state: State, success: Boolean): Prop

        final override def postCondition(state: State, result: Either[Throwable, Result]): Prop =
            postCondition(state, result.isRight)
    }

    final case class NoOp(override val id: Int) extends Command {
        type Result = Unit
        override def run(sut: Sut): IO[Result] = IO.pure(())
        override def runState(state: State): (Unit, State) = ((), state)
        override def preCondition(state: State) = true
        override def postCondition(stateBefore: State, result: Either[Throwable, Result]) = true
    }

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
      * ScalaCheck tries out all possible end states with the [[Command.postCondition]] function of
      * the very last command executed (there is always exactly one command executed after all
      * parallel command executions).
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
            // TODO: remove
            println("----ACTIONS SEQ------------------------------------")
            as.seqCmds.foreach(println)
            println("----ACTIONS PAR------------------------------------")
            as.parCmds.foreach(_.foreach(println))

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
                            println("WARNING: you should never not see that")
                            Prop.undecided
                        }

                    case None =>
                        // TODO: Block until canCreateNewSut is true
                        println("WARNING: you should never not see that")
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

    private type Commands = List[Command]

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
                    c <- genCommand(s0).suchThat(_.preCondition(s0))
                } yield (c.runState(s0)._2, cs :+ c)
            }
        }

        /** Checks all preconditions and evaluates the final state.
          * @return
          *   the final state and the && over preconditions.
          */
        def cmdsPrecond(s: State, cmds: Commands): (State, Boolean) = cmds match {
            case Nil                          => (s, true)
            case c :: cs if c.preCondition(s) => cmdsPrecond(c.runState(s)._2, cs)
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
            // (s1, seqCmds) <- resize(100, sized(sizedCmds(s0)))
            (s1, seqCmds) <- sized(sizedCmds(s0))

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
        println(s"lastCmd=$lastCmd")
        val l =
            s"Initial state:\n  ${as.initialState}\n" +
                s"Sequential Commands:\n${prettyCmdsRes(as.seqCmds, maxLength)}\n" +
                // TODO: fix me
                // s"Sequential Commands:\n${prettyCmdsRes(as.seqCmds, maxLength)}\n" +
                s"Last executed command: $lastCmd"

        p :| l
    }

    /** Short-circuit property AND operator. (Should maybe be in Prop module) */
    private def propAnd(p1: => Prop, p2: => Prop) = p1.flatMap { r =>
        if r.success then Prop.secure(p2) else Prop(_ => r)
    }

    // TODO: Added the last succeeded command (last `Int`) - do we need it though?
    private def runSeqCmds(
        ioSut: IO[Sut],
        s0: State,
        cs: Commands
    ): (Sut, Prop, State, Int) = {
        val io = for {
            initial <- ioSut.map(sut => (sut, Prop.proved, s0, 0))
            result <- cs.foldLeft(IO.pure(initial)) { case (acc, c) =>
                acc >>= { case (sut, p, s, lastCmd) =>
                    c.runPC(sut).map { pred =>
                        (sut, propAnd(p, pred(s)), c.runState(s)._2, lastCmd + 1)
                    }
                }
            }
            (sut, prop, s, lastCmd) = result
            shutdownProp <- shutdownSut(sut)
        } yield (sut, propAnd(prop, shutdownProp), s, lastCmd)
        io.unsafeRunSync()
    }

    private def prettyCmdsRes(rs: List[Command], maxLength: Int) = {
        val maxNumberWidth = "%d".format(maxLength).length
        val lineLayout = "  %%%dd. %%s".format(maxNumberWidth)
        val cs = rs.zipWithIndex.map { case (r, i) =>
            lineLayout.format(i + 1, r)
        }
        if cs.isEmpty then "  <no commands>"
        else cs.mkString("\n")
    }
}
