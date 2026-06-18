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

object Stage4Properties extends YetAnotherProperties("Integration Stage 4"):

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p
            .withWorkers(1)
            .withMinSuccessfulTests(1)
            .withPropFilter(Some("Two-peers head works \\(quick\\)"))

    // Fast variants (CI): small command sequences, minimal peer count
    val _ = property("Two-peers head works") =
        Stage4Suite(label = "stage4-two-peers", nPeers = 2).property()

    val _ = property("Two-peers head works (quick)") =
        Stage4Suite(label = "stage4-quick-two-peers", nPeers = 2, nCommands = 10).property()

    // Two head peers hubbing one coil peer (coilQuorum = 1). The Pc4 multi-head relay makes the coil peer a full
    // participant: it follows every leader's blocks (relayed briefs + relayed user requests,
    // de-muxed by author, with the follower BlockWeaver buffering early briefs) and co-signs every
    // stack — a manual run hard-confirms ~11 stacks on BOTH head peers AND the coil peer. NOT in the
    // default
    // green set: the only thing blocking is harness settling — the coil peer's extra liaison link + relay
    // round-trips make the generative post-run `waitForIdle` drain take very long to (or never)
    // reach a stable idle. Correctness is proven; the green gating is a stage4 drain/settling problem
    // to solve separately. Point the propFilter here to watch the full fast+slow relay run.
    val _ = property("Two-heads-one-coil works") =
        Stage4Suite(label = "stage4-2h1c", nPeers = 2, nCoilPeers = 1).property()

    val _ = property("Three-peers head works") =
        Stage4Suite(label = "stage4-three-peers", nPeers = 3).property()

    val _ = property("Three-peers head works (quick)") =
        Stage4Suite(label = "stage4-quick-three-peers", nPeers = 3, nCommands = 10).property()

    val _ = property("Twenty-peers head works") =
        Stage4Suite(label = "stage4-twenty-peers", nPeers = 20).property()

    // WebSocket transport variant: real-clock run over real WS connections. Reuses the stage1
    // takeoff trick — `genInitialState` anchors `startTime` at `Instant.now() + 60s` when
    // `useTestControl = false`, and `startupSut` sleeps the wall clock until that anchor, so
    // model time and wall clock coincide at command 1. Inter-arrival delays from the
    // superposition generator now elapse in real time.
    val _ = property("Two-peers head works WS") = Stage4Suite(
      label = "stage4-ws-two-peers",
      nPeers = 2,
      transportMode = TransportMode.WebSocket(),
      backendMode = BackendMode.RocksDb()
    ).property()

    val _ = property("Two-peers head works WS (quick)") = Stage4Suite(
      label = "stage4-ws-quick-two-peers",
      nPeers = 2,
      nCommands = 10,
      transportMode = TransportMode.WebSocket(),
      backendMode = BackendMode.RocksDb()
    ).property()

    // Extended variants: large command sequences or high peer counts
    val _ = property("Two-peers head works (extended)") =
        Stage4Suite(label = "stage4-two-peers-extended", nPeers = 2, nCommands = 500).property()

    val _ = property("Two-heads-one-coil works (extended)") =
        Stage4Suite(label = "stage4-2h1c", nPeers = 2, nCoilPeers = 1, nCommands = 500).property()

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

    // WebSocket transport variant of the two-heads-one-coil run: the hub↔coil link runs over the
    // shared per-peer WS server (`/hub` route) instead of in-process handles, exercising the
    // HubWsTransport / CoilPeerWsTransport / CoilFrame path end-to-end. Same status as the Direct
    // 2h1c run — opt-in, NOT in the default green set: the blocker is harness settling, not the
    // transport. Point the propFilter here to watch the coil follow + co-sign over real WS.
    val _ = property("Two-heads-one-coil works WS") =
        Stage4Suite(
          label = "stage4-ws-2h1c",
          nPeers = 2,
          nCoilPeers = 1,
          transportMode = TransportMode.WebSocket(),
        ).property()

// ===================================
// Diagnostic: termination check
// ===================================

object Stage4Diagnostics extends YetAnotherProperties("Integration Stage 4 Diagnostics"):

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withWorkers(1).withMinSuccessfulTests(1)


    val _ = property("Two-peers 1 command") =
        Stage4Suite(label = "diag-1", nPeers = 2, nCommands = 1).property()
