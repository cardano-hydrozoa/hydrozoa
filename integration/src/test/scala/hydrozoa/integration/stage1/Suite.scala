package hydrozoa.integration.stage1

import cats.syntax.all.*
import cats.effect.{IO, Resource}
import com.bloxbean.cardano.client.util.HexUtil
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import com.suprnation.typelevel.actors.syntax.*
import cats.data.ReaderT
import hydrozoa.config.head.initialization.{CappedValueGen, InitializationParametersGenTopDown}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.InitParamsType
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.config.node.operation.evacuation.generateNodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.generateNodeOperationMultisigConfig
import hydrozoa.integration.stage1.Model.CurrentTime.BeforeHappyPathExpiration
import hydrozoa.integration.stage1.Model.{BlockCycle, CurrentTime}
import hydrozoa.integration.stage1.SuiteCardano.*
import hydrozoa.integration.stage1.model.Deposits
import hydrozoa.integration.yaci.DevKit
import hydrozoa.integration.yaci.DevKit.devnetInfo
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, debug, info, trace}
import hydrozoa.multisig.backend.cardano.CardanoBackendBlockfrost.URL
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat, CardanoBackendMock, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{BlockWeaver, CardanoLiaison, CardanoLiaisonEvent, CardanoLiaisonEventFormat, FastConsensusActor, FastConsensusActorEvent, FastConsensusActorEventFormat, RequestSequencer, StackComposer}
import hydrozoa.multisig.ledger.block.{Block, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.eutxol2.{EutxoL2Ledger, toUtxos}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.joint.{JointLedger, JointLedgerEvent, JointLedgerEventFormat}
import hydrozoa.multisig.persistence.{InMemoryBackendStore, Persistence, PersistenceEventFormat}
import org.scalacheck.commands.{ModelBasedSuite, ScenarioGen}
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, Prop, PropertyM}
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.{CardanoInfo, CertState, Coin, EvaluatorMode, PlutusScriptEvaluator, ProtocolParams, SlotConfig, Transaction, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{Change, TransactionBuilder}
import test.TestPeerName.Alice
import test.{GenWithTestPeers, SeedPhrase, TestPeerName, TestPeers, given}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Integration Stage 1 (the simplest).
  *   - Only three real actors are involved: [[JointLedger]], [[FastConsensusActor]], and
  *     [[CardanoLiaison]]
  *
  * Notes:
  *   - The absence of the weaver prevents automatic block creation, including timed-out major
  *     blocks.
  */

// TODO: copied from Cardano Liaison test suite, which is temporarily disabled
// used for tracing only, so it's only role is to emit FastConsensusActorEvent.LeaderStarted
class BlockWeaverMock(
    tracer: ContraTracer[IO, FastConsensusActorEvent],
    ownPeerNum: HeadPeerNumber,
    numPeers: Int
) extends Actor[IO, BlockWeaver.Request] {
    private def emitLeader(blockNum: BlockNumber): IO[Unit] =
        tracer.traceWith(FastConsensusActorEvent.LeaderStarted(blockNum, ownPeerNum))

    override def preStart: IO[Unit] =
        if 1 % numPeers == (ownPeerNum: Int) then emitLeader(BlockNumber(1))
        else IO.unit

    override def receive: Receive[IO, BlockWeaver.Request] = {
        case b: Block.SoftConfirmed =>
            val nextBlockNum = (b.blockNum: Int) + 1
            if nextBlockNum % numPeers == (ownPeerNum: Int) then
                emitLeader(BlockNumber(nextBlockNum))
            else IO.unit
        case _ => IO.unit
    }
}

enum SuiteCardano:
    case Mock(
        cardanoNetwork: CardanoNetwork
    )
    case Yaci(
        url: CardanoBackendBlockfrost.URL = DevKit.blockfrostApiBaseUri,
        protocolParams: ProtocolParams
    )
    case Public(
        seedPhrase: SeedPhrase,
        cardanoNetwork: StandardCardanoNetwork,
        blockfrostKey: String
    )

case class Suite(
    suiteCardano: SuiteCardano,
    override val scenarioGen: ScenarioGen[Model.State, Stage1Sut],
    txTimingGen: GenWithTestPeers[hydrozoa.config.head.multisig.timing.TxTiming],
    label: String = "unknown",
) extends ModelBasedSuite {

    private def pick[A](gen: Gen[A])(using pp: A => Pretty): PropertyM[IO, A] =
        PropertyM.pick[IO, A](gen)

    override type Env = Stage1Env

    case class Stage1Env(
        startTime: java.time.Instant,
        cardanoNetwork: CardanoNetwork,
        genesisUtxo: TestPeers => Map[TestPeerName, Utxos],
        testPeers: TestPeers
    )

    override type State = Model.State
    override type Sut = Stage1Sut

    override val useTestControl: Boolean = suiteCardano match {
        case Mock(_) => true
        case _       => false
    }

    override def commandGenTweaker: [A] => (g: Gen[A]) => Gen[A] =
        [A] =>
            (g: Gen[A]) =>
                suiteCardano match {
                    // When using L1 mock we do want to start with short sequences to find a failure ASAP
                    case _: SuiteCardano.Mock => g
                    // When using Yaci and devnet we don't want to generate short sequences - only long ones
                    case _: SuiteCardano.Yaci   => Gen.resize(200, g)
                    case _: SuiteCardano.Public => Gen.resize(200, g)
            }

    override def initEnv: PropertyM[IO, Env] = {
        import PropertyM.run
        val backendTracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
        suiteCardano match {

            case SuiteCardano.Mock(cardanoNetwork) =>
                run(IO {
                    val testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, 1)
                    Stage1Env(
                      startTime = java.time.Instant.ofEpochSecond(2000000000),
                      cardanoNetwork = cardanoNetwork,
                      genesisUtxo = yaciTestSauceGenesis(cardanoNetwork.network),
                      testPeers = testPeers
                    )
                })

            case SuiteCardano.Yaci(url, protocolParams) =>
                for {
                    _ <- run(log.info("Resetting Yaci..."))
                    _ <- run(IO(DevKit.reset()))
                    _ <- run(log.info("Getting devnet info..."))
                    devnetInfo <- run(IO(DevKit.devnetInfo()))
                    _ <- run(log.debug(s"devnetInfo: $devnetInfo"))

                    startTime = java.time.Instant.ofEpochSecond(devnetInfo.startTime)
                    cardanoInfo = CardanoInfo(
                      protocolParams = protocolParams,
                      network = Network.Testnet,
                      slotConfig = SlotConfig(
                        zeroTime = startTime.toEpochMilli,
                        zeroSlot = 0,
                        slotLength = devnetInfo.slotLength * 1_000L
                      )
                    )
                    cardanoNetwork: CardanoNetwork.Custom = CardanoNetwork.Custom(
                      cardanoInfo,
                      DevKit.devnetInfo().protocolMagic
                    )
                    testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, 1)
                    aliceAddress = testPeers.shelleyAddressFor(Alice)

                    _ <- run(log.info(s"Topping up Alice's address ${aliceAddress.toBech32.get}"))
                    _ <- run(IO(DevKit.topup(aliceAddress, Coin(20_000_000_000L))))

                    backend = CardanoBackendBlockfrost.apply_(
                      Right((cardanoNetwork, url)),
                      tracer = backendTracer
                    )
                    mixSplitTx <- mkMixSplitTx(cardanoNetwork, backend, aliceAddress)
                    mixSplitTxSigned = testPeers.walletFor(Alice).signTx(mixSplitTx)
                    _ <- run(
                           log.trace(
                             s"mixSplitTxSigned = ${HexUtil.encodeHexString(mixSplitTxSigned.toCbor)}"
                           )
                         )
                    ret <- run(backend.submitTx(mixSplitTxSigned))
                    _ <- run(log.trace(s"submission response: $ret"))

                    // TODO: await tx
                    _ <- run(IO.sleep(5.seconds))

                    splitUpUtxos <- run(
                                      backend
                                          .utxosAt(aliceAddress)
                                          .flatMap(_.fold(IO.raiseError, IO.pure))
                                    )
                } yield Stage1Env(
                    startTime = startTime,
                    cardanoNetwork = cardanoNetwork,
                    genesisUtxo = _ => Map(Alice -> splitUpUtxos),
                    testPeers = testPeers
                )

            case SuiteCardano.Public(seedPhrase, cardanoNetwork, blockfrostKey) =>
                val testPeers = TestPeers.apply(seedPhrase, cardanoNetwork, 1)
                val aliceAddress = testPeers.shelleyAddressFor(Alice)
                val backend = CardanoBackendBlockfrost.apply_(
                  Left(cardanoNetwork),
                  blockfrostKey,
                  tracer = backendTracer
                )
                for {
                    _ <- run(
                           log.info(
                             s"Splitting up utxos at Alice's address ${aliceAddress.toBech32.get}"
                           )
                         )
                    mixSplitTx <- mkMixSplitTx(cardanoNetwork, backend, aliceAddress)
                    splitTxSigned = testPeers.walletFor(Alice).signTx(mixSplitTx)
                    _ <- run(
                           log.trace(
                             s"splitTxSigned = ${HexUtil.encodeHexString(splitTxSigned.toCbor)}"
                           )
                         )
                    ret <- run(backend.submitTx(splitTxSigned))
                    _ <- run(log.trace(s"submission response: $ret"))

                    // TODO: await tx
                    _ <- run(IO.sleep(60.seconds))

                    startTime <- run(IO.realTimeInstant)
                    splitUpUtxos <- run(
                                      backend
                                          .utxosAt(aliceAddress)
                                          .flatMap(_.fold(IO.raiseError, IO.pure))
                                    )
                } yield Stage1Env(
                    startTime = startTime,
                    cardanoNetwork = cardanoNetwork,
                    genesisUtxo = _ => Map(Alice -> splitUpUtxos),
                    testPeers = testPeers
                )
        }
    }

    def mkMixSplitTx(
        cardanoNetwork: CardanoNetwork,
        backend: CardanoBackendBlockfrost,
        address: ShelleyAddress
    ): PropertyM[IO, Transaction] = {
        import PropertyM.run
        for {
            peerUtxos <- run(backend.utxosAt(address).flatMap(_.fold(IO.raiseError, IO.pure)))
            outputValues <- {
                                val totalValue = Value.combine(peerUtxos.map((_, o) => o.value))
                                val gen = CappedValueGen.generateCappedValue(cardanoNetwork)
                                pick(
                                  Gen.tailRecM((totalValue, List.empty: List[Value]))((rest, acc) =>
                                      gen(rest, Some(20_000_000L), Some(1000_000_000), None).map(
                                        next =>
                                            if next == rest
                                            then Right(acc :+ next)
                                            else Left((rest - next, acc :+ next))
                                      )
                                  )
                                )
                            }
            tx <- run(
                    IO.fromEither(
                      (for {
                          unbalanced <- TransactionBuilder
                              .build(
                                cardanoNetwork.cardanoInfo.network,
                                peerUtxos.map { case (utxoId, output) =>
                                    Spend(Utxo(utxoId, output))
                                }.toList ++ outputValues.map(value =>
                                    Send(
                                      TransactionOutput.Babbage(address = address, value = value)
                                    )
                                )
                              )
                          balanced <- unbalanced.balanceContext(
                            diffHandler = Change.changeOutputDiffHandler(
                              _, _,
                              protocolParams = cardanoNetwork.cardanoProtocolParams,
                              changeOutputIdx = 0
                            ),
                            protocolParams = cardanoNetwork.cardanoProtocolParams,
                            evaluator = PlutusScriptEvaluator(
                              cardanoNetwork.cardanoInfo,
                              EvaluatorMode.EvaluateAndComputeCost
                            )
                          )
                      } yield balanced.transaction).left.map(err => RuntimeException(err.toString))
                    )
                  )
        } yield tx
    }

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("Stage1.Suite"))

    // ===================================
    // Initial state handling
    // ===================================

    override def genInitialState(env: Env): PropertyM[IO, State] = {
        import PropertyM.run
        import env.testPeers
        val testPeerToUtxos = env.genesisUtxo(testPeers)

        for {
            // For real-blockchain modes: anchor all block times 1 minute in the future so the SUT
            // can sleep until that moment and be perfectly synchronized with the model clock.
            takeoffTime <- suiteCardano match {
                               case _: SuiteCardano.Mock =>
                                   pick(Gen.const(None: Option[java.time.Instant]))
                               case _ =>
                                   run(IO.realTimeInstant.map(t => Some(t.plusSeconds(60))))
                           }

            _ <- run(log.debug(s"takeoff time: $takeoffTime"))

            // Build custom HeadConfig generator anchored at the takeoff time (or env start for mock)
            generateHeadStartTime = (takeoffTime match {
                case None =>
                    ReaderT(tp =>
                        Gen.const(BlockCreationEndTime(env.startTime.quantize(tp.slotConfig)))
                    )
                case Some(t) =>
                    ReaderT(tp => Gen.const(BlockCreationEndTime(t.quantize(tp.slotConfig))))
            }): GenWithTestPeers[BlockCreationEndTime]

            generateHeadParams = hydrozoa.config.head.parameters.generateHeadParameters(
              generateTxTiming = txTimingGen
            )

            generateHeadConfigBootstrap = hydrozoa.config.head.generateHeadConfigBootstrap(
              generateHeadParams = generateHeadParams,
              generateInitializationParameters = InitParamsType.TopDown(
                InitializationParametersGenTopDown.GenWithDeps(
                  generateGenesisUtxosL1 = ReaderT((tp: TestPeers) =>
                      Gen.const(testPeerToUtxos.map((k, v) => k.headPeerNumber -> v))
                  )
                )
              )
            )

            generateHeadConfig = hydrozoa.config.head.generateHeadConfig(
              genHeadConfigBootstrap = generateHeadConfigBootstrap,
              generateInitialBlock = bootstrap =>
                  hydrozoa.config.head.initialization.generateInitialBlock(
                    genHeadConfigBootstrap = ReaderT
                        .pure[Gen, TestPeers, hydrozoa.config.head.HeadConfig.Bootstrap](bootstrap),
                    generateBlockCreationEndTime = generateHeadStartTime
                  )
            )

            config <- pick(
                        MultiNodeConfig.generateWith(testPeers)(
                          generateHeadConfig = generateHeadConfig
                        )
                      )

            _ <- run(log.debug(s"total contingency: ${config.headConfig.fallbackContingency}"))
            _ <- run(log.debug(s"l2 utxos: ${config.headConfig.initialEvacuationMap.size}"))
            _ <- run(log.debug(s"l2 total: ${config.headConfig.initialL2Value}"))

            peerL1GenesisUtxos = testPeerToUtxos.values.flatten.toMap

            _ <- run(log.debug(s"peerL1GenesisUtxos: ${peerL1GenesisUtxos}"))

            operationalMultisigConfig <- pick(
                                           generateNodeOperationMultisigConfig(
                                             config.headConfig.maxCardanoLiaisonPollingPeriod
                                           )
                                         )
            operationalEvacuationConfig <- pick(
                                             generateNodeOperationEvacuationConfig(
                                               testPeers.walletFor(Alice)
                                             )
                                           )
        } yield Model
            .State(
              multiNodeConfig = config,
              takeoffTime = takeoffTime,
              nextRequestNumber = RequestNumber(0),
              currentTime = BeforeHappyPathExpiration(
                config.headConfig.initialBlock.blockBrief.endTime.convert
              ),
              blockCycle = BlockCycle.Done(BlockNumber.zero, BlockVersion.Full.zero),
              competingFallbackStartTime = config.headConfig.txTiming
                  .newFallbackStartTime(config.headConfig.initialBlock.blockBrief.endTime),
              // TODO: see https://linear.app/gummiworm-labs/issue/GUM-104/specify-how-ledger-configuration-is-handled
              utxosL2Active =
                  config.headConfig.initializationParameters.initialEvacuationMap.toUtxos,
              peerUtxosL1 = peerL1GenesisUtxos,
              preinitPeerUtxosL1 = peerL1GenesisUtxos,
              deposits = Deposits.empty,
              utxoLocked = Set.empty,
              reservedSubmissionDuration = 10.seconds
            )
            .applyContinuingL1Tx(config.headConfig.initializationTx.tx)
    }

    // ===================================
    // SUT handling
    // ===================================

    // TODO: do we want to run multiple SUTs when using L1 mock?
    override def canStartupNewSut(): Boolean = true

    override def startupSut(state: Model.State): Resource[IO, Sut] = {
        val multiNodeConfig = state.multiNodeConfig
        val runId = java.util.UUID.randomUUID().toString.take(8)
        val headPeerNum = HeadPeerNumber.zero
        val nodeConfig = multiNodeConfig.nodeConfigs(headPeerNum)
        val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        // Run cardano L1 backend - a mock or Yaci
        val cardanoBackendConfig = suiteCardano match {
            case Mock(_) =>
                CardanoBackendConfig.Mock(
                  network = multiNodeConfig.headConfig.cardanoInfo.network,
                  slotConfig = multiNodeConfig.headConfig.cardanoInfo.slotConfig,
                  protocolParams = multiNodeConfig.headConfig.cardanoInfo.protocolParams,
                  genesisUtxos = state.preinitPeerUtxosL1
                )
            case Yaci(url, _) =>
                CardanoBackendConfig.Blockfrost(
                  network = Right(
                    (
                      CardanoNetwork.Custom(
                        multiNodeConfig.headConfig.cardanoInfo,
                        devnetInfo().protocolMagic
                      ),
                      url
                    )
                  )
                )
            case Public(_, cardanoNetwork, blockfrostKey) =>
                CardanoBackendConfig.Blockfrost(
                  network = Left(cardanoNetwork),
                  blockfrostKey = blockfrostKey
                )
        }
        for {
            _ <- Resource.eval(for {
                _ <- log.info(s"Creating new SUT [${label}/${runId}]")
                // Fast-forward to the current time if TestControl is used
                _ <- IO.whenA(useTestControl)(for {
                    _ <- log.debug("Fast-forward to the current time...")
                    // Before creating the actor system, if we are in the TestControl we need
                    // to fast-forward to the zero block creation time.
                    // Will take almost forever if is run after the actor system is spun up
                    _ <- IO.sleep(
                      FiniteDuration(
                        state.getCurrentTime.instant.instant.toEpochMilli,
                        TimeUnit.MILLISECONDS
                      )
                    )
                    now <- IO.realTimeInstant
                    _ <- log.info(s"[startupSut] Current time: ${now.toEpochMilli}")
                } yield ())
                // For real-blockchain modes: sleep until takeoff time so the model clock and the
                // Yaci clock are synchronized at command execution start. Abort if we are already late.
                _ <- state.takeoffTime match {
                    case None => IO.unit
                    case Some(t) =>
                        IO.realTimeInstant.flatMap { now =>
                            if now.isAfter(t) then
                                IO.raiseError(
                                  RuntimeException(
                                    s"Suite aborted: initialization took too long " +
                                        s"(takeoff: $t, now: $now)"
                                  )
                                )
                            else
                                val sleepMs = t.toEpochMilli - now.toEpochMilli
                                log.info(s"Sleeping ${sleepMs}ms until takeoff at $t") >>
                                    IO.sleep(FiniteDuration(sleepMs, TimeUnit.MILLISECONDS))
                        }
                }
                _ <- log.debug(s"peerKeys: ${multiNodeConfig.headConfig.headPeers.headPeerVKeys}")
            } yield ())
            // Actor system — terminates automatically when the Resource is finalized.
            system <- ActorSystem[IO]("Stage1")
            // Note: Actor exceptions are logged by the supervision strategy but don't
            // automatically fail tests. To treat them as test failures check that the
            // system was not terminated in the [[beforeFinalize]] action.
            // In-memory persistence — released when the Resource is finalized.
            persistenceBackend <- InMemoryBackendStore.open(persistenceTracer)
            sut <- Resource.eval(for {
                cardanoBackend <- mkCardanoBackend(cardanoBackendConfig)
                fcaTracer: ContraTracer[IO, FastConsensusActorEvent] =
                    Slf4jTracer.sink.contramap(FastConsensusActorEventFormat.humanFormat(headPeerNum))
                clTracer: ContraTracer[IO, CardanoLiaisonEvent] =
                    Slf4jTracer.sink.contramap(CardanoLiaisonEventFormat.humanFormat(headPeerNum))
                // Weaver stub — emits leader_started for tracing
                blockWeaver <- system.actorOf(
                  new BlockWeaverMock(
                    fcaTracer,
                    headPeerNum,
                    nodeConfig.headPeers.nHeadPeers: Int
                  )
                )
                // Cardano liaison
                cardanoLiaison <- system.actorOf(
                  CardanoLiaison(
                    nodeConfig,
                    cardanoBackend,
                    CardanoLiaison.Connections(blockWeaver),
                    clTracer
                  )
                )
                // Request sequencer stub
                requestSequencerStub <- system.actorOf(new Actor[IO, RequestSequencer.Request] {
                    override def receive: Receive[IO, RequestSequencer.Request] = _ => IO.pure(())
                })
                // Agent actor
                jointLedgerD <- IO.deferred[JointLedger.Handle]
                consensusActorD <- IO.deferred[FastConsensusActor.Handle]
                agent <- system.actorOf(AgentActor(jointLedgerD, consensusActorD, cardanoLiaison))
                // StackComposer stub — stage1 does not exercise slow consensus.
                stackComposerStub <- system.actorOf(new Actor[IO, StackComposer.Request] {
                    override def receive: Receive[IO, StackComposer.Request] = _ => IO.pure(())
                })
                jointLedgerConnections = JointLedger.Connections(
                  fastConsensusActor = agent,
                  stackComposer = stackComposerStub,
                  headPeerLiaisons = List(),
                )
                l2Ledger <- EutxoL2Ledger(nodeConfig)
                // In-memory persistence for the SUT — stage1 doesn't assert on it, but the actors
                // need a handle.
                persistence <- {
                    given CardanoNetwork.Section = nodeConfig
                    Persistence.fromBackend(persistenceBackend, persistenceTracer)
                }
                jointLedger <- system.actorOf(
                  JointLedger(
                    nodeConfig,
                    jointLedgerConnections,
                    l2Ledger,
                    Slf4jTracer.sink.contramap(JointLedgerEventFormat.humanFormat(headPeerNum)),
                    persistence
                  )
                )
                _ <- jointLedgerD.complete(jointLedger)
                // Consensus actor
                consensusConnections = FastConsensusActor.Connections(
                  blockWeaver = blockWeaver,
                  cardanoLiaison = cardanoLiaison,
                  requestSequencer = requestSequencerStub,
                  headPeerLiaisons = List.empty,
                  jointLedger = jointLedger,
                  stackComposer = stackComposerStub,
                )
                consensusActor <- system.actorOf(
                  FastConsensusActor(nodeConfig, consensusConnections, fcaTracer, persistence)
                )
                _ <- consensusActorD.complete(consensusActor)
            } yield Stage1Sut(
              headAddress = multiNodeConfig.headConfig.headMultisigAddress,
              system = system,
              cardanoBackend = cardanoBackend,
              agent = agent,
              log = Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("Stage1.Sut")),
              runId = runId,
            ))
        } yield sut
    }

    enum CardanoBackendConfig:
        case Mock(
            network: Network,
            slotConfig: SlotConfig,
            protocolParams: ProtocolParams,
            genesisUtxos: Utxos
        )
        case Blockfrost(
            network: Either[StandardCardanoNetwork, (CardanoNetwork.Custom, URL)],
            blockfrostKey: String = ""
        )

    private def mkCardanoBackend(config: CardanoBackendConfig): IO[CardanoBackend[IO]] =
        config match {
            case mock: CardanoBackendConfig.Mock =>
                for {
                    _ <- IO.pure(())
                    utxos = mock.genesisUtxos
                    mockState = MockState.apply(utxos)
                    cardanoBackend <- CardanoBackendMock.mockIO(
                      initialState = mockState,
                      mkContext = slot =>
                          Context(
                            env = UtxoEnv(
                              slot = slot,
                              params = mock.protocolParams,
                              certState = CertState.empty,
                              network = mock.network
                            ),
                            slotConfig = mock.slotConfig
                          )
                    )
                } yield cardanoBackend

            case CardanoBackendConfig.Blockfrost(network, blockfrostKey) =>
                val expectedProtocolParams = network.fold(
                  _.asInstanceOf[CardanoNetwork].cardanoProtocolParams,
                  _._1.cardanoProtocolParams
                )
                for {
                    // TODO: this is needed for Yaci only
                    _ <- log.info("Wait a bit for backend being ready...")
                    _ <- IO.sleep(1.second)
                    _ <- log.info(
                      "Creating Cardano backend and fetching the last epoch parameters to check they match ones in the head config..."
                    )
                    cardanoBackend <- CardanoBackendBlockfrost(
                      network = network,
                      apiKey = blockfrostKey,
                      tracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
                    )
                    // Here we use start-up parameters
                    response <- cardanoBackend.getStartupParams
                    check = response
                        .fold(
                          err => throw RuntimeException(s"Cannot obtain protocol parameters: $err"),
                          actualParams => (actualParams == expectedProtocolParams) -> actualParams
                        )
                    _ <- IO.raiseWhen(!check._1)(
                      RuntimeException(
                        "Protocol parameters mismatch: " +
                            s"\nexpected: ${expectedProtocolParams}" +
                            s"\nactual: ${check._2}"
                      )
                    )
                } yield cardanoBackend
        }

    override def beforeFinalize(lastState: State, sut: Sut): IO[Prop] = for {

        _ <- log.info("beforeFinalize")

        /** Drain all in-flight messages before the [[startupSut]] Resource is finalized.
          *
          * [[waitForIdle]] returns when all mailboxes are empty, the child set is stable, and
          * deadLetters are drained — it also verifies that the system was not terminated. Stage1
          * uses [[BlockWeaverMock]], so there are no wakeup timers to drain — only the mailbox
          * queues from the last submitted command.
          *
          * `maxTimeout` is `Temporal[F].sleep`-based and races the drain loop:
          *   - Under [[TestControl]] (mock backend), it is virtual time and elapses fast in real
          *     time; the value caps how much virtual clock advancement we tolerate.
          *   - Under real-clock backends (Yaci / testnet), it is wall-clock time and bounds how
          *     long this drain may stall the test before giving up.
          *
          * The same call serves both modes; the only thing the test author tunes is `maxTimeout`.
          */
        _ <- sut.system.waitForIdle(maxTimeout = 5.minutes)

        // L1 effect-presence assertion lives in stage4 ([[Stage4Suite.propEffectsLanded]]).
        // Stage1 deliberately stubs the slow side (no `SlowConsensusActor`, no `StackComposer`)
        // and won't grow one — stage4 already covers happy/fallback per-block disjunction over
        // observed `Stack.HardConfirmed`. Stage1's job is now bounded to driving the fast cycle
        // end-to-end through real Yaci/Blockfrost L1 backends to catch L1-side breakage; effect
        // semantics are tested in stage4.
    } yield Prop.passed
}
