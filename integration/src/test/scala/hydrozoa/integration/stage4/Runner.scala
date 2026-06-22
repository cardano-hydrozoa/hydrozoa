package hydrozoa.integration.stage4

import cats.data.StateT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import hydrozoa.integration.stage4.Model.*
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalacheck.commands.AnyCommand
import org.scalacheck.{Prop, PropertyM, Test, YetAnotherProperties}

import scala.concurrent.duration.DurationInt

object Stage4Runner:

    def generateCommands(
        initialState: ModelState,
        n: Int
    )(using ContraTracer[IO, Slf4jMsg]): PropertyM[IO, (ModelState, List[AnyCommand[ModelState, Stage4Sut]])] =
        val step: StateT[[x] =>> PropertyM[IO, x], ModelState, AnyCommand[ModelState, Stage4Sut]] =
            for
                state    <- StateT.get[[x] =>> PropertyM[IO, x], ModelState]
                cmd      <- StateT.liftF(Stage4ScenarioGen.genNextCommand(state))
                newState <- StateT.liftF(PropertyM.run(cmd.advanceState[IO](state)))
                _        <- StateT.set[[x] =>> PropertyM[IO, x], ModelState](newState)
            yield cmd
        step.replicateA(n).run(initialState)

    def renderTable(
        initialState: ModelState,
        commands: List[AnyCommand[ModelState, Stage4Sut]]
    )(using ContraTracer[IO, Slf4jMsg]): IO[String] =
        val peers = initialState.params.multiNodeConfig.nodeConfigs.keys.toSeq.sortBy(p => p: Int)
        val startEpoch = initialState.currentModelTime.getEpochSecond

        val colWidth = 36
        val stepWidth = 4

        // Strip peer/requestId noise; keep just type + variant
        def compactLabel(label: String): String = label match
            case s if s.startsWith("L2Tx") =>
                val parts = s.dropWhile(_ != '(').drop(1).dropWhile(_ != ',').drop(1).trim
                val strategy = parts.takeWhile(_ != ',')
                s"L2Tx:$strategy"
            case s if s.startsWith("RegisterDeposit") =>
                val absEpoch =
                    "abs=(\\d+)".r.findFirstMatchIn(s).map(_.group(1).toLong).getOrElse(0L)
                s"Deposit(abs~+${absEpoch - startEpoch}s)"
            case s if s.startsWith("Delay") => "Delay"
            case s                          => s

        val divider =
            s"+${"-" * (stepWidth + 2)}+${peers.map(_ => s"-${"-" * colWidth}-").mkString("+")}+"
        val header = s"| ${"Step".padTo(stepWidth, ' ')} |${peers
                .map { p =>
                    val mean = initialState.params.meanInterArrivalTimes(p)
                    s" Peer ${p: Int} (μ=${mean.toSeconds}s)".padTo(colWidth + 2, ' ')
                }
                .mkString("|")}|"

        val peerRegex = "peer=(\\d+)".r

        val legend =
            "Legend: μ=mean inter-arrival  +t=peer clock since start  Δ=sampled delay  #N=gen order"

        // Collect (genSeqNum, cmd, nextState) for all non-NoOp commands
        commands.zipWithIndex
            .foldLeft(IO.pure(
              (initialState, List.empty[(Int, AnyCommand[ModelState, Stage4Sut], ModelState)])
            )) { case (ioAcc, (cmd, idx)) =>
                ioAcc.flatMap { case (state, acc) =>
                    cmd.advanceState[IO](state).map { nextState =>
                        val entry =
                            if cmd.label != "NoOp" then acc :+ (idx + 1, cmd, nextState) else acc
                        (nextState, entry)
                    }
                }
            }
            .map { case (_, rows) =>
                // Sort by global clock, ties broken by generation order. With the single global clock
                // the model time after each command is shared across peers, so this sort matches the
                // SUT virtual timeline.
                val sorted = rows.sortBy { (genSeq, _, nextState) =>
                    (nextState.currentModelTime.getEpochSecond, genSeq)
                }

                val rowLines = sorted.zipWithIndex.map { case ((genSeq, cmd, nextState), displayIdx) =>
                    val actingPeer =
                        peerRegex.findFirstMatchIn(cmd.label).map(m => HeadPeerNumber(m.group(1).toInt))
                    s"| ${(displayIdx + 1).toString.padTo(stepWidth, ' ')} |" + peers.map { p =>
                        val cell =
                            if actingPeer.contains(p) then
                                val t = nextState.currentModelTime.getEpochSecond - startEpoch
                                val d = cmd.delay.toSeconds
                                s"#$genSeq ${compactLabel(cmd.label)} +${t}s Δ${d}s"
                                    .take(colWidth)
                                    .padTo(colWidth, ' ')
                            else " " * colWidth
                        s" $cell |"
                    }.mkString
                }

                (List(divider, header, divider) ++ rowLines ++ List(divider, legend)).mkString("\n")
            }

    def printTable(
        initialState: ModelState,
        commands: List[AnyCommand[ModelState, Stage4Sut]]
    )(using ContraTracer[IO, Slf4jMsg]): IO[Unit] =
        renderTable(initialState, commands).flatMap(IO.println)

@main def stage4PrintCommandSequence(): Unit =
    given ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("Stage4.Runner"))
    val prop = PropertyM.monadicIO {
        for
            initialState <- PropertyM.pick[IO, ModelState](Stage4Suite.genInitialState(
                nPeers = 3,
                meanInterArrivalTime = p =>
                    (p: Int) match
                        case 0 => 30.seconds
                        case 1 => 8.seconds
                        case _ => 15.seconds
            ))
            commandsResult <- Stage4Runner.generateCommands(initialState, 300)
            (_, commands)   = commandsResult
            _              <- PropertyM.run(Stage4Runner.printTable(initialState, commands))
        yield Prop.passed
    }
    val _ = Test.check(Test.Parameters.default.withMinSuccessfulTests(1), prop)

// ===================================
// Test entry points
// ===================================

// N.B.: The tests default to 10 commands. Mark longer ones with (extended) to filter them out in CI
object Stage4Properties extends YetAnotherProperties("Integration Stage 4"):

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p
            .withWorkers(1)
            .withMinSuccessfulTests(100)
            .withPropFilter(Some("Twenty-peers head works \\(quick\\)"))

    val _ = property("Two-peers head works") =
        Stage4Suite(label = "stage4-two-peers", nPeers = 2).property()

    // TODO: re-enable once stage4's coil flow is ported off `PeerStack` onto MRM (see commits
    // porting head peers; coil peers still need a follow-up). Coil-mixed scenarios stay disabled
    // until then to keep the harness on one wiring path.
    val _ = property("Two-heads-one-coil works (DISABLED, pending MRM port of coil flow)") =
        Prop.passed

    val _ = property("Three-peers head works") =
        Stage4Suite(label = "stage4-three-peers", nPeers = 3).property()

    val _ = property("Twenty-peers head works") =
        Stage4Suite(label = "stage4-twenty-peers", nPeers = 20).property()

    // WebSocket transport variant: real-clock run over real WS connections. Reuses the stage1
    // takeoff trick — `genInitialState` anchors `startTime` at `Instant.now() + 60s` when
    // `useTestControl = false`, and `sutResource` sleeps the wall clock until that anchor, so
    // model time and wall clock coincide at command 1. Inter-arrival delays from the
    // superposition generator now elapse in real time.
    val _ = property("Two-peers head works WS") = Stage4Suite(
      label = "stage4-ws-two-peers",
      nPeers = 2,
      transportMode = TransportMode.WebSocket(),
      backendMode = BackendMode.RocksDb()
    ).property()

    // Extended variants: large command sequences or high peer counts
    val _ = property("Two-peers head works (extended)") =
        Stage4Suite(label = "stage4-two-peers-extended", nPeers = 2, nCommands = 500).property()

    val _ =
        property("Two-heads-one-coil works (extended) (DISABLED, pending MRM port of coil flow)") =
            Prop.passed

    val _ = property("Three-peers head works (extended)") =
        Stage4Suite(label = "stage4-three-peers-extended", nPeers = 3, nCommands = 500).property()

    val _ = property("Twenty-peers head works (extended)") =
        Stage4Suite(label = "stage4-twenty-peers-extended", nPeers = 20, nCommands = 500).property()

    val _ = property("Two-peers head works WS (extended)") = Stage4Suite(
      label = "stage4-ws-two-peers-extended",
      nPeers = 2,
      nCommands = 500,
      transportMode = TransportMode.WebSocket(),
      backendMode = BackendMode.RocksDb()
    ).property()

    val _ = property("Ten-peers head works WS (extended)") = Stage4Suite(
      label = "stage4-ws-ten-peers-extended",
      nPeers = 10,
      nCommands = 500,
      transportMode = TransportMode.WebSocket(),
      backendMode = BackendMode.RocksDb()
    ).property()

    val _ = property("Two-heads-one-coil works WS (DISABLED, pending MRM port of coil flow)") =
        Prop.passed

// ===================================
// Diagnostic: termination check
// ===================================

object Stage4Diagnostics extends YetAnotherProperties("Integration Stage 4 Diagnostics"):

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withWorkers(1).withMinSuccessfulTests(1)


    val _ = property("Two-peers 1 command") =
        Stage4Suite(label = "diag-1", nPeers = 2, nCommands = 1).property()
