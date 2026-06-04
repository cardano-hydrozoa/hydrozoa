package hydrozoa.integration.stage4

import hydrozoa.integration.stage4.Model.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalacheck.commands.{AnyCommand, LoggingControl}
import org.scalacheck.{Test, YetAnotherProperties}

import scala.concurrent.duration.DurationInt

object Stage4Runner:

    def generateCommands(
        initialState: ModelState,
        n: Int
    ): (ModelState, List[AnyCommand[ModelState, Stage4Sut]]) =
        List.fill(n)(()).foldLeft((initialState, Nil: List[AnyCommand[ModelState, Stage4Sut]])) {
            case ((state, cmds), _) =>
                val cmd = Stage4ScenarioGen.genNextCommand(state).sample.get
                (cmd.advanceState(state), cmds :+ cmd)
        }

    def renderTable(
        initialState: ModelState,
        commands: List[AnyCommand[ModelState, Stage4Sut]]
    ): String =
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

        // Collect (genSeqNum, cmd, nextState) for all non-NoOp commands
        val rows = commands.zipWithIndex
            .foldLeft(
              (initialState, List.empty[(Int, AnyCommand[ModelState, Stage4Sut], ModelState)])
            ) { case ((state, acc), (cmd, idx)) =>
                val nextState = cmd.advanceState(state)
                val entry = if cmd.label != "NoOp" then acc :+ (idx + 1, cmd, nextState) else acc
                (nextState, entry)
            }
            ._2

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

        val legend =
            "Legend: μ=mean inter-arrival  +t=peer clock since start  Δ=sampled delay  #N=gen order"

        (List(divider, header, divider) ++ rowLines ++ List(divider, legend)).mkString("\n")

    def printTable(
        initialState: ModelState,
        commands: List[AnyCommand[ModelState, Stage4Sut]]
    ): Unit =
        println(renderTable(initialState, commands))

@main def stage4PrintCommandSequence(): Unit =
    LoggingControl.withSuppressedLogs("stage4 command sequence generation") {
        val gen = Stage4Suite.genInitialState(
          nPeers = 3,
          meanInterArrivalTime = p =>
              (p: Int) match
                  case 0 => 30.seconds
                  case 1 => 8.seconds
                  case _ => 15.seconds
        )
        val initialState = LazyList.continually(gen.sample).flatten.head
        val (_, commands) = Stage4Runner.generateCommands(initialState, 300)
        Stage4Runner.printTable(initialState, commands)
    }

// ===================================
// Test entry points
// ===================================

object Stage4Properties extends YetAnotherProperties("Integration Stage 4"):

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p
            // .withPropFilter(Some("Two-peers head works"))
            // .withPropFilter(Some("Three-peers head works"))
            // .withPropFilter(Some("Twenty-peers head works"))
            .withPropFilter(Some("Two-peers head works WS"))
            // .withPropFilter(Some("Ten-peers head works WS"))
            // .withInitialSeed(SOk,.eed.fromBase64("uOllVn-lTcPloHDUuC3_x8oVjOgUbTR7vUoBi3T71gF=").get)
            // .withInitialSeed(Seed.fromBase64("wZ2FQc_Iv2duN06RHMXFg7014XeEirS_K2-wY0RN38O=").get)
            // .withInitialSeed(Seed.fromBase64("7wf2XaHHBHdGl4XOoIpW8PvN2t8XFcR0fFE0RBX6pWG=").get)
            // .withInitialSeed(Seed.fromBase64("Irdkn14LUINcIDjKQOxKuN-GF2399UOCwL-C11NVESJ=").get)
            .withWorkers(1)
            .withMinSuccessfulTests(1)

    val _ = property("Two-peers head works") =
        Stage4Suite(label = "stage4-two-peers", nPeers = 2).property()

    val _ = property("Three-peers head works") =
        Stage4Suite(label = "stage4-three-peers", nPeers = 3).property()

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

    val _ = property("Ten-peers head works WS") = Stage4Suite(
      label = "stage4-ws-ten-peers",
      nPeers = 10,
      transportMode = TransportMode.WebSocket(),
      backendMode = BackendMode.RocksDb()
    ).property()
