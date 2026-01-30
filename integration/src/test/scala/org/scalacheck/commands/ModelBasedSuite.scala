package org.scalacheck.commands

import org.scalacheck.{Gen, Prop}

import scala.util.{Failure, Success, Try}

/** TODO: WIP
  *
  * Yet another better/different [[Commands]]:
  *   - Haskell State-like commands
  *   - Readable and pretty output
  *   - Support for [[IO]] actions and [[TestControl]] -
  * Limitations:
  *   - Supports only sequential commands
  */
trait ModelBasedSuite extends Commands {

    // ===================================
    // StateLikeCommand
    // ===================================

    trait Command0 extends Command:

        def id: Int

        private[commands] def runPC0(sut: Sut): (Try[String], State => Prop) = {
            import Prop.propBoolean
            val r = Try(run(sut))
            (r.map(_.toString), s => preCondition(s) ==> postCondition(s, r))
        }

    final case class NoOp0(override val id: Int) extends Command0 {
        type Result = Unit

        override def run(sut: Sut) = ()

        override def nextState(state: State) = state

        override def preCondition(state: State) = true

        override def postCondition(state: State, result: Try[Unit]) = true
    }

    /** State-like Command that uses Haskell-style `runState` instead of `nextState`.
      */
    trait StateLikeCommand extends Command0:

        final override def nextState(state: State): State = runState(state)._2

        def runState(state: State): (Result, State)

        /** NB: I guess the description in the [[Command]] is misleading. The reason this exists is
          * that one could generate wrong commands that should error. So you have "right" commands
          * and "bad" commands. This method allows the test suite tell them apart to see what expect
          * from the SUT.
          */
        override def preCondition(state: State): Boolean = true

        final override def postCondition(stateBefore: State, result: Try[Result]): Prop =
            val (expectedResult, stateAfter) = runState(stateBefore)
            result match
                case Success(realResult: Result) =>
                    postConditionSuccess(
                      expectedResult,
                      stateBefore,
                      stateAfter,
                      realResult
                    )
                case Failure(e) =>
                    postConditionFailure(expectedResult, stateBefore, stateAfter, e)

        def postConditionSuccess(
            expectedResult: Result,
            stateBefore: State,
            stateAfter: State,
            result: Result
        ): Prop = Prop.passed

        def postConditionFailure(
            expectedResult: Result,
            stateBefore: State,
            stateAfter: State,
            err: Throwable
        ): Prop = Prop.falsified

    final def property0(): Prop = {
        val suts = collection.mutable.Map.empty[AnyRef, (State, Option[Sut])]

        Prop.forAll(actions()) { as =>
            println("----------------------------------------")
            as.seqCmds.foreach(println)
            println("----------------------------------------")

            try {
                val sutId = suts.synchronized {
                    val initSuts = suts.values.collect { case (state, None) => state }
                    val runningSuts = suts.values.collect { case (_, Some(sut)) => sut }
                    if canCreateNewSut(as.s, initSuts, runningSuts) then {
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
                            if suts.contains(id) then {
                                suts += (id -> (as.s -> Some(sut)))
                                true
                            } else false
                        }
                        if doRun then {
                            runActions(sut, as, removeSut())
                        } else {
                            print("you should not see that")
                            removeSut()
                            Prop.undecided
                        }

                    case None => // NOT IMPLEMENTED Block until canCreateNewSut is true
                        println("NOT IMPL")
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

    private type Commands0 = List[Command0]

    override def genCommand(s: State): Gen[Command0]

    private case class Actions(
        s: State,
        seqCmds: Commands0
    )

    private def actions(): Gen[Actions] = {
        import Gen.{const, sized}

        def sizedCmds(s: State)(sz: Int): Gen[(State, Commands0)] = {
            val l: List[Unit] = List.fill(sz)(())
            l.foldLeft(const((s, Nil: Commands0))) { case (g, ()) =>
                for {
                    (s0, cs) <- g
                    c <- genCommand(s0).suchThat(_.preCondition(s0))
                } yield (c.nextState(s0), cs :+ c)
            }
        }

        def cmdsPrecond(s: State, cmds: Commands0): (State, Boolean) = cmds match {
            case Nil                          => (s, true)
            case c :: cs if c.preCondition(s) => cmdsPrecond(c.nextState(s), cs)
            case _                            => (s, false)
        }

        def actionsPrecond(as: Actions): Boolean =
            initialPreCondition(as.s) && (cmdsPrecond(as.s, as.seqCmds) match {
                case (s, true) => true
                case _         => false
            })

        val g = for {
            s0 <- genInitialState
            (s1, seqCmds) <- sized(sizedCmds(s0))
            // (s1, seqCmds) <- resize(100, sized(sizedCmds(s0)))
        } yield Actions(s0, seqCmds)

        g.suchThat(actionsPrecond)
    }

    private def runActions(sut: Sut, as: Actions, finalize: => Unit): Prop = {
        val maxLength = as.seqCmds.length
        val (p1, s, rs1, lastCmd) = runSeqCmds(sut, as.s, as.seqCmds)
        val l1 = s"Initial state:\n  ${as.s}\n" +
            s"Sequential commands:\n${prettyCmdsRes(as.seqCmds zip rs1, maxLength)}\n" +
            s"Last executed command: $lastCmd"
        p1 :| l1
    }

    // Added the last succeded command (last `Int`)
    private def runSeqCmds(
        sut: Sut,
        s0: State,
        cs: Commands0
    ): (Prop, State, List[Try[String]], Int) =
        cs.foldLeft((Prop.proved, s0, List[Try[String]](), 0)) { case ((p, s, rs, lastCmd), c) =>
            val (r, pf) = c.runPC0(sut)
            (p && pf(s), c.nextState(s), rs :+ r, lastCmd + 1)
        }

    private def prettyCmdsRes(rs: List[(Command0, Try[String])], maxLength: Int) = {
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
}
