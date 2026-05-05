package hydrozoa.integration.stage4

import hydrozoa.integration.stage4.Model.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalacheck.commands.{AnyCommand, LoggingControl}

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

    def printTable(initialState: ModelState, commands: List[AnyCommand[ModelState, Stage4Sut]]): Unit =
        val peers = initialState.params.multiNodeConfig.nodeConfigs.keys.toSeq.sortBy(p => p: Int)
        val startEpoch = initialState.modelTimeFor(peers.head).getEpochSecond

        val colWidth = 36
        val stepWidth = 4

        // Strip peer/requestId noise; keep just type + variant
        def compactLabel(label: String): String = label match
            case s if s.startsWith("L2Tx") =>
                val parts = s.dropWhile(_ != '(').drop(1).dropWhile(_ != ',').drop(1).trim
                val strategy = parts.takeWhile(_ != ',')
                s"L2Tx:$strategy"
            case s if s.startsWith("RegisterDeposit") =>
                val absEpoch = "abs=(\\d+)".r.findFirstMatchIn(s).map(_.group(1).toLong).getOrElse(0L)
                s"Deposit(abs~+${absEpoch - startEpoch}s)"
            case s if s.startsWith("Delay") => "Delay"
            case s                          => s

        val divider = s"+${"-" * (stepWidth + 2)}+${peers.map(_ => s"-${"-" * colWidth}-").mkString("+")}+"
        val header = s"| ${"Step".padTo(stepWidth, ' ')} |${peers.map { p =>
            val mean = initialState.params.meanInterArrivalTimes(p)
            s" Peer ${p: Int} (μ=${mean.toSeconds}s)".padTo(colWidth + 2, ' ')
        }.mkString("|")}|"

        println(divider)
        println(header)
        println(divider)

        val peerRegex = "peer=(\\d+)".r

        // Collect (genSeqNum, cmd, nextState) for all non-NoOp commands
        val rows = commands.zipWithIndex
            .foldLeft((initialState, List.empty[(Int, AnyCommand[ModelState, Stage4Sut], ModelState)])) {
                case ((state, acc), (cmd, idx)) =>
                    val nextState = cmd.advanceState(state)
                    val entry = if cmd.label != "NoOp" then acc :+ (idx + 1, cmd, nextState) else acc
                    (nextState, entry)
            }
            ._2

        // Sort by acting peer's simulated clock, ties broken by generation order
        val sorted = rows.sortBy { (genSeq, cmd, nextState) =>
            val t = peerRegex.findFirstMatchIn(cmd.label)
                .map(m => nextState.modelTimeFor(HeadPeerNumber(m.group(1).toInt)).getEpochSecond)
                .getOrElse(0L)
            (t, genSeq)
        }

        sorted.zipWithIndex.foreach { case ((genSeq, cmd, nextState), displayIdx) =>
            val actingPeer = peerRegex.findFirstMatchIn(cmd.label).map(m => HeadPeerNumber(m.group(1).toInt))
            val row = s"| ${(displayIdx + 1).toString.padTo(stepWidth, ' ')} |" + peers.map { p =>
                val cell = if actingPeer.contains(p) then
                    val t = nextState.modelTimeFor(p).getEpochSecond - startEpoch
                    val d = cmd.delay.toSeconds
                    s"#$genSeq ${compactLabel(cmd.label)} +${t}s Δ${d}s".take(colWidth).padTo(colWidth, ' ')
                else " " * colWidth
                s" $cell |"
            }.mkString
            println(row)
        }
        println(divider)
        println("Legend: μ=mean inter-arrival  +t=peer clock since start  Δ=sampled delay  #N=gen order")

@main def stage4PrintCommandSequence(): Unit =
    LoggingControl.withSuppressedLogs("stage4 command sequence generation") {
        val gen = Stage4Suite.genInitialState(
            nPeers = 3,
            meanInterArrivalTime = p => (p: Int) match
                case 0 => 30.seconds
                case 1 => 8.seconds
                case _ => 15.seconds
        )
        val initialState = LazyList.continually(gen.sample).flatten.head
        val (_, commands) = Stage4Runner.generateCommands(initialState, 300)
        Stage4Runner.printTable(initialState, commands)
    }
