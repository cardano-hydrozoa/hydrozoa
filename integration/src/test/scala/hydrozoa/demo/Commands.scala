package hydrozoa.demo

import org.scalacheck.*
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed

import scala.util.{Failure, Success, Try}

/** An API for stateful testing in ScalaCheck.
  *
  * For an implementation overview, see the examples in ScalaCheck's source tree.
  *
  * @since 1.12.0
  */
trait Commands {

    /** The abstract state type. Must be immutable. The [[State]] type should model the state of the
      * system under test (SUT). It should only contain details needed for specifying our pre- and
      * post-conditions, and for creating [[Sut]] instances.
      */
    type State

    /** A type representing one instance of the system under test (SUT). The [[Sut]] type should be
      * a proxy to the actual system under test and is therefore, by definition, a mutable type. It
      * is used by the [[Command.run]] method to execute commands in the system under test. It
      * should be possible to have any number of co-existing instances of the [[Sut]] type, as long
      * as [[canCreateNewSut]] isn't violated, and each [[Sut]] instance should be a proxy to a
      * distinct SUT instance. There should be no dependencies between the [[Sut]] instances, as
      * they might be used in parallel by ScalaCheck. [[Sut]] instances are created by [[newSut]]
      * and destroyed by [[destroySut]]. [[newSut]] and [[destroySut]] might be called at any time
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
      */
    def canCreateNewSut(
        newState: State,
        initSuts: Traversable[State],
        runningSuts: Traversable[Sut]
    ): Boolean

    /** Create a new [[Sut]] instance with an internal state that corresponds to the provided
      * abstract state instance. The provided state is guaranteed to fulfill
      * [[initialPreCondition]], and [[newSut]] will never be called if [[canCreateNewSut]] is not
      * true for the given state.
      */
    def newSut(state: State): Sut

    /** Destroy the system represented by the given [[Sut]] instance, and release any resources
      * related to it.
      */
    def destroySut(sut: Sut): Unit

    /** The precondition for the initial state, when no commands yet have run. This is used by
      * ScalaCheck when command sequences are shrunk and the first state might differ from what is
      * returned from [[genInitialState]].
      */
    def initialPreCondition(state: State): Boolean

    /** A generator that should produce an initial [[State]] instance that is usable by [[newSut]]
      * to create a new system under test. The state returned by this generator is always checked
      * with the [[initialPreCondition]] method before it is used.
      */
    def genInitialState: Gen[State]

    /** A generator that, given the current abstract state, should produce a suitable Command
      * instance.
      */
    def genCommand(state: State): Gen[Command]

    /** A type representing the commands that can run in the system under test. This type should be
      * immutable and implement the equality operator properly.
      */
    trait Command {

        /** An abstract representation of the result of running this command in the system under
          * test. The [[Result]] type should be immutable and it should encode everything about the
          * command run that is necessary to know in order to correctly implement the
          * [[[Command!.postCondition* postCondition]]] method.
          */
        type Result

        /** Executes the command in the system under test, and returns a representation of the
          * result of the command run. The result value is later used for verifying that the command
          * behaved according to the specification, by the [[Command!.postCondition* postCondition]]
          * method.
          */
        def run(sut: Sut): Result

        /** Returns a new [[State]] instance that represents the state of the system after this
          * command has run, given the system was in the provided state before the run.
          */
        def nextState(state: State): State

        /** Precondition that decides if this command is allowed to run when the system under test
          * is in the provided state.
          */
        def preCondition(state: State): Boolean

        /** Postcondition that decides if this command produced the correct result or not, given the
          * system was in the provided state before the command ran.
          */
        def postCondition(state: State, result: Try[Result]): Prop

        /** Wraps the run and postCondition methods in order not to leak the dependent Result type.
          */
        private[Commands] def runPC(sut: Sut): (Try[String], State => Prop) = {
            import Prop.propBoolean
            val r = Try(run(sut))
            (r.map(_.toString), s => preCondition(s) ==> postCondition(s, r))
        }
    }

    /** A command that never should throw an exception on execution. */
    trait SuccessCommand extends Command {
        def postCondition(state: State, result: Result): Prop
        final override def postCondition(state: State, result: Try[Result]) =
            result match {
                case Success(result) => postCondition(state, result)
                case Failure(e)      => Prop.exception(e)
            }
    }

    /** A command that doesn't return a result, only succeeds or fails. */
    trait UnitCommand extends Command {
        final type Result = Unit
        def postCondition(state: State, success: Boolean): Prop
        final override def postCondition(state: State, result: Try[Unit]) =
            postCondition(state, result.isSuccess)
    }

    /** A command that doesn't do anything */
    case object NoOp extends Command {
        type Result = Null
        def run(sut: Sut) = null
        def nextState(state: State) = state
        def preCondition(state: State) = true
        def postCondition(state: State, result: Try[Null]) = true
    }

    /** A command that runs a sequence of other commands. All commands (and their post conditions)
      * are executed even if some command fails. Note that you probably can't use this method if
      * you're testing in parallel (`threadCount` larger than 1). This is because ScalaCheck regards
      * each command as atomic, even if the command is a sequence of other commands.
      */
    def commandSequence(head: Command, snd: Command, rest: Command*): Command =
        new CommandSequence(head, snd, rest*)

    /** A command that runs a sequence of other commands */
    private final class CommandSequence(val head: Command, snd: Command, rest: Command*)
        extends SuccessCommand {
        /* The tail of the command sequence */
        val tail: Command =
            if (rest.isEmpty) snd else new CommandSequence(snd, rest.head, rest.tail*)

        type Result = (Try[head.Result], Try[tail.Result])

        def run(sut: Sut): Result = (Try(head.run(sut)), Try(tail.run(sut)))

        def nextState(state: State): State = tail.nextState(head.nextState(state))

        def preCondition(state: State): Boolean =
            head.preCondition(state) && tail.preCondition(head.nextState(state))

        def postCondition(state: State, result: Result): Prop =
            head.postCondition(state, result._1) &&
                tail.postCondition(head.nextState(state), result._2)
    }

    /** A property that can be used to test this [[Commands]] specification.
      *
      * The parameter `threadCount` specifies the number of commands that might be executed in
      * parallel. Defaults to one, which means the commands will only be run serially for the same
      * [[Sut]] instance. Distinct [[Sut]] instances might still receive commands in parallel, if
      * the [[Test.Parameters.workers]] parameter is larger than one. Setting `threadCount` higher
      * than one enables ScalaCheck to reveal thread-related issues in your system under test.
      *
      * When setting `threadCount` larger than one, ScalaCheck must evaluate all possible command
      * interleavings (and the end [[State]] instances they produce), since parallel command
      * execution is non-deterministic. ScalaCheck tries out all possible end states with the
      * [[Command.postCondition]] function of the very last command executed (there is always
      * exactly one command executed after all parallel command executions). If it fails to find an
      * end state that satisfies the postcondition, the test fails. However, the number of possible
      * end states grows rapidly with increasing values of `threadCount`. Therefore, the lengths of
      * the parallel command sequences are limited so that the number of possible end states don't
      * exceed `maxParComb`. The default value of `maxParComb` is 1000000.
      */
    final def property(threadCount: Int = 1, maxParComb: Int = 1000000): Prop = {
        val suts = collection.mutable.Map.empty[AnyRef, (State, Option[Sut])]
        val g = actions(threadCount, maxParComb, 500)
        val as = g
            .apply(
              Parameters.default,
              Seed.fromBase64("QquIyEzeWlhTG6U2J1BLXhOCZxx4eLm9nUDYdlw9LjO=").get
            )
            .get

        println("--------------------------------------------")
        println(as.s)
        as.seqCmds.foreach(println)

        //System.exit(0)

        try {
            val sutId = suts.synchronized {
                val initSuts = suts.values.collect { case (state, None) => state }
                val runningSuts = suts.values.collect { case (_, Some(sut)) => sut }
                if (canCreateNewSut(as.s, initSuts, runningSuts)) {
                    val sutId = new AnyRef
                    suts += (sutId -> (as.s -> None))
                    Some(sutId)
                } else None
            }
            sutId match {
                case Some(id) =>
                    val sut = newSut(as.s)
                    def removeSut(): Unit = {
                        suts.synchronized {
                            suts -= id
                            destroySut(sut)
                        }
                    }
                    val doRun = suts.synchronized {
                        if (suts.contains(id)) {
                            suts += (id -> (as.s -> Some(sut)))
                            true
                        } else false
                    }
                    if (doRun) runActions(sut, as, removeSut())
                    else {
                        removeSut()
                        Prop.undecided
                    }

                case None => // NOT IMPLEMENTED Block until canCreateNewSut is true
                    println("NOT IMPL")
                    Prop.undecided
            }
        } catch {
            case e: Throwable =>
                suts.synchronized { suts.clear() }
                throw e
        }
    }

    /** Override this to provide a custom Shrinker for your internal [[State]]. By default no
      * shrinking is done for [[State]].
      */
    def shrinkState: Shrink[State] = implicitly

    // Private methods //
    private type Commands = List[Command]

    private case class Actions(
        s: State,
        seqCmds: Commands,
        parCmds: List[Commands]
    )

    private implicit val shrinkActions: Shrink[Actions] = Shrink[Actions] { as =>
        val shrinkedCmds: Stream[Actions] =
            Shrink.shrink(as.seqCmds).map(cs => as.copy(seqCmds = cs)) append
                Shrink.shrink(as.parCmds).map(cs => as.copy(parCmds = cs))

        Shrink.shrinkWithOrig[State](as.s)(shrinkState) flatMap { state =>
            shrinkedCmds.map(_.copy(s = state))
        } map { as => ensurePreconditions(as) }
    }

    private def ensurePreconditions(a: Actions): Actions = {
        def filterCommandSequence(s: State, commands: Commands): Commands =
            commands match {
                case cmd :: cmds =>
                    if (cmd.preCondition(s))
                        cmd :: filterCommandSequence(cmd.nextState(s), cmds)
                    else
                        filterCommandSequence(s, cmds)
                case Nil => Nil
            }

        val filteredSequentialCommands = filterCommandSequence(a.s, a.seqCmds)
        val stateAfterSequentialCommands = filteredSequentialCommands
            .foldLeft(a.s) { case (st, cmd) => cmd.nextState(st) }

        val filteredParallelCommands = a.parCmds
            .map(commands => {
                filterCommandSequence(stateAfterSequentialCommands, commands)
            })
            .filter(_.nonEmpty)

        filteredParallelCommands match {
            case List(singleThreadedContinuation) =>
                val seqCmds = filteredSequentialCommands ++ singleThreadedContinuation
                Actions(a.s, seqCmds, Nil)
            case _ =>
                Actions(a.s, filteredSequentialCommands, filteredParallelCommands)
        }
    }

    private def runSeqCmds(sut: Sut, s0: State, cs: Commands): (Prop, State, List[Try[String]]) =
        cs.foldLeft((Prop.proved, s0, List[Try[String]]())) { case ((p, s, rs), c) =>
            val (r, pf) = c.runPC(sut)
            (p && pf(s), c.nextState(s), rs :+ r)
        }

    private def runParCmds(
        sut: Sut,
        s: State,
        pcmds: List[Commands]
    ): (Prop, List[List[(Command, Try[String])]]) = {
        import concurrent.*
        val tp = java.util.concurrent.Executors.newFixedThreadPool(pcmds.size)
        implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(tp)
        val memo = collection.mutable.Map.empty[(State, List[Commands]), List[State]]

        def endStates(scss: (State, List[Commands])): List[State] = {
            val (s, css) = (scss._1, scss._2.filter(_.nonEmpty))
            (memo.get((s, css)), css) match {
                case (Some(states), _) => states
                case (_, Nil)          => List(s)
                case (_, cs :: Nil) =>
                    List(cs.init.foldLeft(s) { case (s0, c) => c.nextState(s0) })
                case _ =>
                    val inits = scan(css) { case (cs, x) =>
                        (cs.head.nextState(s), cs.tail :: x)
                    }
                    val states = inits.distinct.flatMap(endStates).distinct
                    memo += (s, css) -> states
                    states
            }
        }

        def run(
            endStates: List[State],
            cs: Commands
        ): Future[(Prop, List[(Command, Try[String])])] = Future {
            if (cs.isEmpty) (Prop.proved, Nil)
            else
                blocking {
                    val rs = cs.init.map(_.runPC(sut)._1)
                    val (r, pf) = cs.last.runPC(sut)
                    (Prop.atLeastOne(endStates.map(pf)*), cs.zip(rs :+ r))
                }
        }

        try {
            val res = Future.traverse(pcmds)(run(endStates(s -> pcmds), _)) map { l =>
                val (ps, rs) = l.unzip
                (Prop.atLeastOne(ps*), rs)
            }
            Await.result(res, concurrent.duration.Duration.Inf)
        } finally { tp.shutdown() }
    }

    /** Formats a list of commands with corresponding results */
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
        if (cs.isEmpty)
            "  <no commands>"
        else
            cs.mkString("\n")
    }

    /** A property that runs the given actions in the given SUT */
    private def runActions(sut: Sut, as: Actions, finalize: => Unit): Prop = {
        val maxLength = as.parCmds.map(_.length).foldLeft(as.seqCmds.length)(_.max(_))
        try {
            val (p1, s, rs1) = runSeqCmds(sut, as.s, as.seqCmds)
            val l1 =
                s"Initial State:\n  ${as.s}\nSequential Commands:\n${prettyCmdsRes(as.seqCmds zip rs1, maxLength)}"
            if (as.parCmds.isEmpty) p1 :| l1
            else
                propAnd(
                  p1.flatMap { r =>
                      if (!r.success) finalize; Prop(_ => r)
                  } :| l1, {
                      try {
                          val (p2, rs2) = runParCmds(sut, s, as.parCmds)
                          val l2 = rs2.map(prettyCmdsRes(_, maxLength)).mkString("\n\n")
                          p2 :| l1 :| s"Parallel Commands (starting in state = ${s})\n$l2"
                      } finally finalize
                  }
                )
        } finally if (as.parCmds.isEmpty) finalize
    }

    /** [[Actions]] generator */
    private def actions(threadCount: Int, maxParComb: Int, size: Int): Gen[Actions] = {
        import Gen.{const, listOfN, sized}

        def sizedCmds(s: State)(sz: Int): Gen[(State, Commands)] = {
            val l: List[Unit] = List.fill(sz)(())
            l.foldLeft(const((s, Nil: Commands))) { case (g, ()) =>
                for {
                    (s0, cs) <- g
                    c <- genCommand(s0).suchThat(_.preCondition(s0))
                } yield (c.nextState(s0), cs :+ c)
            }
        }

        def cmdsPrecond(s: State, cmds: Commands): (State, Boolean) = cmds match {
            case Nil                          => (s, true)
            case c :: cs if c.preCondition(s) => cmdsPrecond(c.nextState(s), cs)
            case _                            => (s, false)
        }

        def actionsPrecond(as: Actions) =
            as.parCmds.length != 1 && as.parCmds.forall(_.nonEmpty) &&
                initialPreCondition(as.s) && (cmdsPrecond(as.s, as.seqCmds) match {
                    case (s, true) => as.parCmds.forall(cmdsPrecond(s, _)._2)
                    case _         => false
                })

        val g = for {
            s0 <- genInitialState
            (s1, seqCmds) <- sizedCmds(s0)(size)
        } yield Actions(s0, seqCmds, Nil)

        g.suchThat(actionsPrecond)
    }

    /** [1,2,3] -- [f(1,[2,3]), f(2,[1,3]), f(3,[1,2])] */
    private def scan[T, U](xs: List[T])(f: (T, List[T]) => U): List[U] = xs match {
        case Nil     => Nil
        case y :: ys => f(y, ys) :: scan(ys) { case (x, xs) => f(x, y :: xs) }
    }

    /** Short-circuit property AND operator. (Should maybe be in Prop module) */
    private def propAnd(p1: => Prop, p2: => Prop) = p1.flatMap { r =>
        if (r.success) Prop.secure(p2) else Prop(_ => r)
    }

}
