package org.scalacheck.commands

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalacheck.{Gen, Prop, Shrink}
import scala.util.{Success, Try}

/** TODO: WIP
  *
  * Yet another better/different [[Commands]]:
  *   - Haskell State-like commands with runState
  *   - Readable and pretty output (TODO)
  *   - Support for [[IO]] genActions and [[TestControl]]
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

    /** Decides if [[newSut]] should be allowed to be called with the specified state instance. This
      * can be used to limit the number of co-existing [[Sut]] instances. The list of existing
      * states represents the initial states (not the current states) for all [[Sut]] instances that
      * are active for the moment. If this method is implemented incorrectly, for example if it
      * returns false even if the list of existing states is empty, ScalaCheck might hang.
      *
      * If you want to allow only one [[Sut]] instance to exist at any given time (a singleton
      * [[Sut]]), implement this method the following way:
      *
      * {{{
      *  def canCreateNewSut(newState: State, initSuts: Traversable[State]
      *    runningSuts: Traversable[Sut]
      *  ) = {
      *    initSuts.isEmpty && runningSuts.isEmpty
      *  }
      * }}}
      *
      * @param newState
      *   TODO: why do we need it?
      * @param initSuts
      *   TODO:
      * @param runningSuts
      *   TODO:
      */
    def canCreateNewSut(
        newState: State,
        initSuts: Iterable[State],
        runningSuts: Iterable[IO[Sut]]
    ): Boolean

    /** Create a new [[Sut]] instance with an internal state that corresponds to the provided
      * abstract state instance.
      *
      * The provided state is guaranteed to fulfill [[initialPreCondition]], and [[newSut]] will
      * never be called if [[canCreateNewSut]] is not true for the given state.
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
          *
          * TODO: this is not the case NB: I guess the description in the [[Command]] is misleading.
          * The reason this exists is that one could generate wrong commands that should error. So
          * you have "right" commands and "bad" commands. This method allows the test suite tell
          * them apart to see what expect * from the SUT.
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
        ): Prop = Prop.falsified

        /** Wraps the run and postCondition methods in order not to leak the dependent Result type.
          * TODO:
          * @param sut
          * @return
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
                case Left(e)       => Prop.exception(e)
                case Right(result) => postCondition(state, result)
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
                    val initSuts = suts.values.collect { case (state, None) => state }
                    val runningSuts = suts.values.collect { case (_, Some(sut)) => sut }
                    if canCreateNewSut(as.initialState, initSuts, runningSuts) then {
                        val sutId = new AnyRef
                        suts += (sutId -> (as.initialState -> None))
                        Some(sutId)
                    } else None
                }
                sutId match {
                    case Some(id) =>
                        val ioSut = newSut(as.initialState)

                        def removeSut(): Unit = {
                            val _ = suts.synchronized {
                                suts -= id
                                IO.delay(for {
                                    sut <- ioSut
                                    _ <- shutdownSut(sut)
                                } yield ())
                                    .unsafeRunSync() // TODO: unsafeRunSync
                            }
                        }

                        val doRun = suts.synchronized {
                            if suts.contains(id) then {
                                suts += (id -> (as.initialState -> Some(ioSut)))
                                true
                            } else false
                        }

                        if doRun then {
                            runActions(ioSut, as, removeSut())
                        } else {
                            println("WARNING: you should never not see that")
                            removeSut()
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
      *   sequention commands, come first
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
          * Each thread i can be in one of i different positions relative to other threads when we
          * have m parallel rounds:
          *   - In each of m rounds, thread i has i possible relative orderings
          *   - So thread i contributes i^m possibilities
          *   - Total: ∏(i=1 to n) i^m = 1^m × 2^m × 3^m × ... × n^m
          *
          * This is a conservative overestimate of the actual state space, used to limit test
          * generation.
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

    private def runActions(ioSut: IO[Sut], as: Actions, finalize: => Unit): Prop = {

        /** Short-circuit property AND operator. (Should maybe be in Prop module) */
        def propAnd(p1: => Prop, p2: => Prop) = p1.flatMap { r =>
            if r.success then Prop.secure(p2) else Prop(_ => r)
        }

        val maxLength = as.parCmds.map(_.length).foldLeft(as.seqCmds.length)(_.max(_))

        try {
            // TODO run the shutdown property here
            val (_sut, p1, s, lastCmd) = runSeqCmds(ioSut, as.initialState, as.seqCmds)
            val l1 =
                s"Initial state:\n  ${as.initialState}\n" +
                    // s"Sequential Commands:\n${prettyCmdsRes(as.seqCmds zip rs1, maxLength)}\n" +
                    // TODO: fix me
                    // s"Sequential Commands:\n${prettyCmdsRes(as.seqCmds, maxLength)}\n" +
                    s"Last executed command: $lastCmd"

            if as.parCmds.isEmpty
            then p1 :| l1
            else {
                ???
                // propAnd(
                //  p1.flatMap { r =>
                //      if !r.success then finalize; Prop(_ => r)
                //  } :| l1, {
                //      try {
                //          val (p2, rs2) = runParCmds(ioSut, s, as.parCmds)
                //          val l2 = rs2.map(prettyCmdsRes(_, maxLength)).mkString("\n\n")
                //          p2 :| l1 :| s"Parallel Commands (starting in state = ${s})\n$l2"
                //      } finally finalize
                //  }
                // )
            }
        } finally if as.parCmds.isEmpty then finalize
    }

    // TODO: Added the last succeeded command (last `Int`) - do we need it though?
    private def runSeqCmds(
        ioSut: IO[Sut],
        s0: State,
        cs: Commands
    ): (Sut, Prop, State, Int) =
        cs.foldLeft(ioSut.map(sut => (sut, Prop.proved, s0, 0))) { case (acc, c) =>
            acc.flatMap { case (sut, p, s, lastCmd) =>
                c.runPC(sut).map { pred =>
                    (sut, p && pred(s), c.runState(s)._2, lastCmd + 1)
                }
            }
        }.unsafeRunSync() // FIXME:

    // private def runParCmds(
    //    sut: Sut,
    //    s: State,
    //    pcmds: List[Commands]
    // ): (Prop, List[List[(Command, Try[String])]]) = {
    //    import concurrent.*
    //    val tp = java.util.concurrent.Executors.newFixedThreadPool(pcmds.size)
    //    implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(tp)
    //    val memo = collection.mutable.Map.empty[(State, List[Commands]), List[State]]
    //
    //    // [1,2,3] -- [f(1,[2,3]), f(2,[1,3]), f(3,[1,2])]
    //    def scan[T, U](xs: List[T])(f: (T, List[T]) => U): List[U] = xs match {
    //        case Nil     => Nil
    //        case y :: ys => f(y, ys) :: scan(ys) { case (x, xs) => f(x, y :: xs) }
    //    }
    //
    //    def endStates(scss: (State, List[Commands])): List[State] = {
    //        val (s, css) = (scss._1, scss._2.filter(_.nonEmpty))
    //        (memo.get((s, css)), css) match {
    //            case (Some(states), _) => states
    //            case (_, Nil)          => List(s)
    //            case (_, cs :: Nil) =>
    //                List(cs.init.foldLeft(s) { case (s0, c) => c.runState(s0)._2 })
    //            case _ =>
    //                val inits = scan(css) { case (cs, x) =>
    //                    (cs.head.runState(s)._2, cs.tail :: x)
    //                }
    //                val states = inits.distinct.flatMap(endStates).distinct
    //                memo += (s, css) -> states
    //                states
    //        }
    //    }
    //
    //    def run(
    //        endStates: List[State],
    //        cs: Commands
    //    ): Future[(Prop, List[(Command, Try[String])])] = Future {
    //        if cs.isEmpty then (Prop.proved, Nil)
    //        else
    //            blocking {
    //                val rs = cs.init.map(_.runPC(sut)._1)
    //                val (r, pf) = cs.last.runPC(sut)
    //                (Prop.atLeastOne(endStates.map(pf)*), cs.zip(rs :+ r))
    //            }
    //    }
    //
    //    try {
    //        val res = Future.traverse(pcmds)(run(endStates(s -> pcmds), _)) map { l =>
    //            val (ps, rs) = l.unzip
    //            (Prop.atLeastOne(ps*), rs)
    //        }
    //        Await.result(res, concurrent.duration.Duration.Inf)
    //    } finally {
    //        tp.shutdown()
    //    }
    // }

    private def prettyCmdsRes(rs: List[(Command, Try[String])], maxLength: Int) = {
        val maxNumberWidth = "%d".format(maxLength).length
        val lineLayout = "  %%%dd. %%s".format(maxNumberWidth)
        val cs = rs.zipWithIndex.map { case (r, i) =>
            lineLayout.format(
              i + 1,
              r match {
                  case (c, Success("()")) => c.toString
                  case (c, Success(r))    => s"$c => $r"
                  case (c, r)             => s"$c => $r"
              }
            )
        }
        if cs.isEmpty then "  <no commands>"
        else cs.mkString("\n")
    }

    // ===================================
    // Shrinking (not used for now)
    // ===================================

    /** Override this to provide a custom Shrinker for your internal [[State]]. By default no
      * shrinking is done for [[State]].
      */
    def shrinkState: Shrink[State] = implicitly

    private implicit val shrinkActions: Shrink[Actions] = Shrink[Actions] { as =>
        val shrinkedCmds: Stream[Actions] =
            Shrink.shrink(as.seqCmds).map(cs => as.copy(seqCmds = cs)) append
                Shrink.shrink(as.parCmds).map(cs => as.copy(parCmds = cs))

        Shrink.shrinkWithOrig[State](as.initialState)(shrinkState) flatMap { state =>
            shrinkedCmds.map(_.copy(initialState = state))
        } map { as => ensurePreconditions(as) }
    }

    private def ensurePreconditions(a: Actions): Actions = {
        def filterCommandSequence(s: State, commands: Commands): Commands =
            commands match {
                case cmd :: cmds =>
                    if cmd.preCondition(s) then
                        cmd :: filterCommandSequence(cmd.runState(s)._2, cmds)
                    else filterCommandSequence(s, cmds)
                case Nil => Nil
            }

        val filteredSequentialCommands = filterCommandSequence(a.initialState, a.seqCmds)
        val stateAfterSequentialCommands = filteredSequentialCommands
            .foldLeft(a.initialState) { case (st, cmd) => cmd.runState(st)._2 }

        val filteredParallelCommands = a.parCmds
            .map(commands => {
                filterCommandSequence(stateAfterSequentialCommands, commands)
            })
            .filter(_.nonEmpty)

        filteredParallelCommands match {
            case List(singleThreadedContinuation) =>
                val seqCmds = filteredSequentialCommands ++ singleThreadedContinuation
                Actions(a.initialState, seqCmds, Nil)
            case _ =>
                Actions(a.initialState, filteredSequentialCommands, filteredParallelCommands)
        }
    }
}
