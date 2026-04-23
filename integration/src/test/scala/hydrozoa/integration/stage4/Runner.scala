package hydrozoa.integration.stage4

import cats.data.ReaderT
import hydrozoa.config.head.InitParamsType
import hydrozoa.config.head.initialization.{InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.multisig.timing.generateYaciTxTiming
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.stage4.Model.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.multisig.backend.cardano.yaciTestSauceGenesis
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.eutxol2.toUtxos
import hydrozoa.multisig.ledger.event.RequestNumber
import org.scalacheck.Gen
import org.scalacheck.commands.{AnyCommand, LoggingControl}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{TransactionInput, Utxos}
import test.{SeedPhrase, TestPeers, given}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Stage4Runner:

    def genInitialState(
        nPeers: Int = 2,
        absorptionSlack: FiniteDuration = 60.seconds,
        meanInterArrivalTime: HeadPeerNumber => FiniteDuration = _ => 12.seconds,
    ): Gen[ModelState] =
        val cardanoNetwork = CardanoNetwork.Preprod
        val testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nPeers)
        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)

        val generateHeadStartTime = ReaderT((tp: TestPeers) =>
            Gen.const(BlockCreationEndTime(java.time.Instant.now().quantize(tp.slotConfig)))
        )

        val generateHeadConfigBootstrap_ = generateHeadConfigBootstrap(
            generateHeadParams = generateHeadParameters(generateTxTiming = generateYaciTxTiming),
            generateInitializationParameters = InitParamsType.TopDown(
                InitializationParametersGenTopDown.GenWithDeps(
                    generateGenesisUtxosL1 = ReaderT((tp: TestPeers) =>
                        Gen.const(testPeerToUtxos.map((k, v) => k.headPeerNumber -> v))
                    )
                )
            )
        )

        val generateHeadConfig_ = generateHeadConfig(
            genHeadConfigBootstrap = generateHeadConfigBootstrap_,
            generateInitialBlock = bootstrap =>
                generateInitialBlock(
                    genHeadConfigBootstrap =
                        ReaderT.pure[Gen, TestPeers, hydrozoa.config.head.HeadConfig.Bootstrap](bootstrap),
                    generateBlockCreationEndTime = generateHeadStartTime
                )
        )

        for
            config <- MultiNodeConfig.generateWith(testPeers)(
                generateHeadConfig = generateHeadConfig_
            )

            preinitPeerUtxosL1 = testPeerToUtxos.map((k, v) => k.headPeerNumber -> v)

            initTx = config.headConfig.initializationTx.tx
            spentInputs = initTx.body.value.inputs.toSet
            initOutputsList = initTx.body.value.outputs.toList.map(_.value).zipWithIndex

            peers = config.nodeConfigs.keys.toSeq.sortBy(p => p: Int)

            peerUtxosL1 = peers.map { pn =>
                val peerAddr = config.addressOf(pn)
                val survived: Utxos = preinitPeerUtxosL1(pn) -- spentInputs
                val newOutputs: Utxos = initOutputsList
                    .filter((out, _) => out.address.asInstanceOf[ShelleyAddress] == peerAddr)
                    .map((out, ix) => TransactionInput(initTx.id, ix) -> out)
                    .toMap
                pn -> (survived ++ newOutputs)
            }.toMap

            startTime = config.headConfig.initialBlock.endTime.convert
        yield ModelState(
            params = Params(config, absorptionSlack, peers.map(pn => pn -> meanInterArrivalTime(pn)).toMap),
            preinitPeerUtxosL1 = preinitPeerUtxosL1,
            currentModelTimes = peers.map(_ -> startTime).toMap,
            utxosL2Active = config.headConfig.initializationParameters.initialEvacuationMap.toUtxos,
            peerUtxosL1 = peerUtxosL1,
            nextRequestNumbers = peers.map(_ -> RequestNumber(0)).toMap,
            pendingDeposits = peers.map(_ -> Nil).toMap,
        )

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
        val gen = Stage4Runner.genInitialState(
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
