package hydrozoa.integration.stage4

import cats.data.ReaderT
import cats.effect.{Deferred, Fiber, IO, Ref}
import com.suprnation.actor.event.{Error as ActorError}
import cats.implicits.*
import com.suprnation.actor.ActorSystem
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.config.head.InitParamsType
import hydrozoa.config.head.initialization.{InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.generateYaciTxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.stage4.Commands.*
import hydrozoa.integration.stage4.Model.*
import hydrozoa.integration.stage4.Stage4SutCommands.given
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.lib.tracing.ProtocolTracer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendMock, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{BlockWeaver, CardanoLiaison, ConsensusActor, EventSequencer, PeerLiaison}
import hydrozoa.multisig.ledger.eutxol2.{EutxoL2Ledger, toUtxos}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.joint.JointLedger
import org.scalacheck.{Gen, Prop, YetAnotherProperties}
import org.scalacheck.commands.{ModelBasedSuite, ScenarioGen}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{CertState, TransactionInput, Utxos}
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import test.{SeedPhrase, TestPeers, given}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}

// ===================================
// Stage 4 suite
// ===================================

case class Stage4Suite(label: String = "stage4") extends ModelBasedSuite:

    override type Env = Unit
    override type State = ModelState
    override type Sut = Stage4Sut

    override def useTestControl: Boolean = true

    override def scenarioGen: ScenarioGen[ModelState, Stage4Sut] = Stage4ScenarioGen

    override def initEnv: Unit = ()

    override def genInitialState(env: Unit): Gen[ModelState] =
        Stage4Suite.genInitialState()

    override def canStartupNewSut(): Boolean = true

    override def startupSut(state: ModelState): IO[Stage4Sut] =
        val multiNodeConfig = state.params.multiNodeConfig
        val cardanoInfo = multiNodeConfig.headConfig.cardanoInfo
        val peers = multiNodeConfig.nodeConfigs.keys.toSeq.sortBy(p => p: Int)

        // Advance simulated clock to the head's start epoch BEFORE creating the ActorSystem.
        // With TestControl, IO.sleep advances the virtual clock only while no actor fibers
        // exist; once actors are started their ping loops compete with tickOne, so the sleep
        // must come first (same pattern as stage1 Suite).
        val startEpochMs = state.currentModelTimes.values.head.getEpochSecond * 1000L

        for
            _ <- IO.sleep(FiniteDuration(startEpochMs, TimeUnit.MILLISECONDS))

            system <- ActorSystem[IO](label).allocated.map(_._1)

            // All peers share one mock L1 backend, starting from the merged pre-init UTxOs.
            // The head initialization tx is submitted by the protocol through normal operation.
            genesisUtxos = state.preinitPeerUtxosL1.values.reduce(_ ++ _)
            cardanoBackend <- CardanoBackendMock.mockIO(
                initialState = MockState(genesisUtxos),
                mkContext = slot => Context(
                    env = UtxoEnv(
                        slot = slot,
                        params = cardanoInfo.protocolParams,
                        certState = CertState.empty,
                        network = cardanoInfo.network
                    ),
                    slotConfig = cardanoInfo.slotConfig
                )
            )

            // Each peer gets its own PendingConnections deferred, completed after all actors
            // are started so cross-peer liaisons can be wired.
            pendingConnsMap <- peers.traverse { peerNum =>
                Deferred[IO, MultisigRegimeManager.Connections].map(peerNum -> _)
            }.map(_.toMap)

            // Create full actor stack per peer; all actors wait on pendingConnections.
            peerStackMap <- peers.traverse { peerNum =>
                val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
                val pending = pendingConnsMap(peerNum)
                for
                    blockWeaver    <- system.actorOf(BlockWeaver(nodeConfig, pending))
                    cardanoLiaison <- system.actorOf(CardanoLiaison(nodeConfig, cardanoBackend, pending))
                    eventSequencer <- system.actorOf(EventSequencer(nodeConfig, pending))
                    l2Ledger       <- EutxoL2Ledger(nodeConfig)
                    jointLedger    <- system.actorOf(JointLedger(nodeConfig, pending, l2Ledger, ProtocolTracer.noop))
                    consensusActor <- system.actorOf(ConsensusActor(nodeConfig, pending))
                yield peerNum -> PeerStack(blockWeaver, cardanoLiaison, eventSequencer, jointLedger, consensusActor)
            }.map(_.toMap)

            // Create one PeerLiaison per (local, remote) pair.
            // peerLiaisonMap(A)(B.id) = the liaison at A directed to B.
            peerLiaisonMap <- peers.traverse { peerNum =>
                val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
                val pending = pendingConnsMap(peerNum)
                peers.filterNot(_ == peerNum).traverse { remotePeerNum =>
                    val remotePeerId = multiNodeConfig.nodeConfigs(remotePeerNum).ownHeadPeerId
                    system.actorOf(PeerLiaison(nodeConfig, remotePeerId, pending))
                        .map(remotePeerId -> _)
                }.map(liaisons => peerNum -> liaisons.toMap)
            }.map(_.toMap)

            // Complete each peer's deferred with its full wiring.
            // remotePeerLiaisons(B.id) = the liaison at B directed back at A (for A's Connections).
            _ <- peers.traverse { peerNum =>
                val stack = peerStackMap(peerNum)
                val ownPeerId = multiNodeConfig.nodeConfigs(peerNum).ownHeadPeerId
                val localLiaisons = peerLiaisonMap(peerNum).values.toList
                val remoteLiaisons = peers.filterNot(_ == peerNum).map { remotePeerNum =>
                    val remotePeerId = multiNodeConfig.nodeConfigs(remotePeerNum).ownHeadPeerId
                    remotePeerId -> peerLiaisonMap(remotePeerNum)(ownPeerId)
                }.toMap
                pendingConnsMap(peerNum).complete(
                    MultisigRegimeManager.Connections(
                        blockWeaver = stack.blockWeaver,
                        cardanoLiaison = stack.cardanoLiaison,
                        consensusActor = stack.consensusActor,
                        eventSequencer = stack.eventSequencer,
                        jointLedger = stack.jointLedger,
                        peerLiaisons = localLiaisons,
                        remotePeerLiaisons = remoteLiaisons,
                    )
                ).void
            }

            sutErrors <- Ref[IO].of(List.empty[String])
            errorDrainer <- system.eventStream.take.flatMap {
                case e: ActorError if e.cause != ActorError.NoCause =>
                    sutErrors.update(_ :+ s"[${e.logSource}] ${e.cause.getMessage}")
                case _ => IO.unit
            }.foreverM.start

        yield Stage4Sut(
            system = system,
            cardanoBackend = cardanoBackend,
            peers = peerStackMap.map { case (peerNum, stack) =>
                peerNum -> Stage4PeerHandle(eventSequencer = stack.eventSequencer)
            },
            sutErrors = sutErrors,
            errorDrainer = errorDrainer,
        )

    override def shutdownSut(lastState: ModelState, sut: Stage4Sut): IO[Prop] =
        // BlockWeaver's IO.sleep inside sleepSendWakeup keeps _isIdle=false for the full sleep
        // duration, so waitForIdle would always time out. Terminate directly after a brief settle
        // window that lets any in-flight fault-handling chains flush to eventStream.
        // TODO: oracle — collect block briefs from all JointLedgers, re-run model in SUT's
        // total order, compare predicted ValidityFlags against actual accept/reject per brief.
        for
            _ <- sut.system.terminate()
            _ <- IO.sleep(100.millis)  // settle: let the drainer consume any last-cycle errors
            _ <- sut.errorDrainer.cancel
            errors <- sut.sutErrors.get
        yield
            if errors.isEmpty then Prop.passed
            else Prop.exception(
                RuntimeException(s"SUT actor errors:\n${errors.mkString("\n")}")
            )


// ===================================
// Initial state generator (canonical location; Runner delegates here for @main)
// ===================================

object Stage4Suite:

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


// ===================================
// Test entry points
// ===================================

object Stage4Properties extends YetAnotherProperties("Integration Stage 4"):

    override def overrideParameters(p: org.scalacheck.Test.Parameters): org.scalacheck.Test.Parameters =
        p.withWorkers(1).withMinSuccessfulTests(3)

    val _ = property("Multi-peer L2 transactions and deposits") =
        Stage4Suite(label = "stage4-mock").property()
