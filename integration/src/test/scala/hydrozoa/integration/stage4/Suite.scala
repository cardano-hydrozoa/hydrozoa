package hydrozoa.integration.stage4

import cats.data.ReaderT
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.implicits.*
import com.comcast.ip4s.{Port, host}
import com.suprnation.actor.event.Error as ActorError
import com.suprnation.actor.{ActorContext, ActorSystem}
import hydrozoa.config.head.coil.{CoilPeerData, CoilPeers}
import hydrozoa.config.head.initialization.{InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.generateYaciTxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{InitParamsType, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.integration.stage4.EffectsLanded.BlockExpectation
import hydrozoa.integration.stage4.Model.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info, warn}
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendMock, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.CardanoLiaison
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber, PeerId, PeerWallet}
import hydrozoa.multisig.consensus.transport.{CoilPeerWsTransport, CoilPeerWsTransportEventFormat, CoilTransport, HubTransport, HubWsTransport, HubWsTransportEventFormat, InProcessHubCoilTransport, InProcessPeerTransport, NodeWsServer, NodeWsServerEventFormat, PeerTransport, PeerTransportEventFormat, WsPeerTransport}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.eutxol2.{EutxoL2Ledger, toUtxos}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects}
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import hydrozoa.multisig.persistence.{BackendStore, Cf, InMemoryBackendStore, Persistence, PersistenceEvent, PersistenceEventFormat}
import hydrozoa.multisig.{CoilMultisigRegimeManager, HeadMultisigRegimeManager, HeadMultisigRegimeManagerEvent, HeadMultisigRegimeManagerEventFormat}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import org.http4s.Uri
import org.http4s.client.websocket.WSClient
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.websocket.WebSocketBuilder2
import org.scalacheck.commands.{AnyCommand, ModelBasedSuite, ScenarioGen}
import org.scalacheck.{Gen, Prop, PropertyM}
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.{CertState, TransactionHash, TransactionInput, Utxos}
import test.{SeedPhrase, TestPeers, given}

// ===================================
// Stage 4 suite
// ===================================

/** Shared state assembled in `sutResource` between starting the `ActorSystem` and building the
  * per-peer actor stacks: the mock L1 backend, the per-peer `Deferred`/`Ref` plumbing, and the
  * settlement/coverage signals + targets consumed by the property runners.
  *
  *   - `cardanoBackend` — single mock L1 shared by every peer.
  *   - `pendingConnsMap(p)` — completed in `acquireSut` with peer `p`'s full `Connections`.
  *   - `blockBriefsMap(p)` / `stacksMap(p)` — capture sinks: every `BlockBrief.Intermediate` from
  *     JL and every `Stack.HardConfirmed` from SCA, per peer.
  *   - `submittedRequestIds` — every `RequestId` the model submits across the run.
  *   - `fastSettlementSignal` / `fastSettlementTarget` — fires once every targeted request id has
  *     surfaced in some peer's captured briefs; target is set by `beforeFinalize`.
  *   - `slowCoverageSignal` / `slowCoverageTarget` — fires once every targeted block number is
  *     covered by some hard-confirmed stack on *every* peer (cross-peer barrier).
  */
private case class PostSystemState(
    cardanoBackend: CardanoBackend[IO],
    pendingConnsMap: Map[HeadPeerNumber, Deferred[IO, HeadMultisigRegimeManager.Connections]],
    blockBriefsMap: Map[HeadPeerNumber, Ref[IO, Vector[BlockBrief.Intermediate]]],
    stacksMap: Map[HeadPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
    coilStacksMap: Map[CoilPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
    submittedRequestIds: Ref[IO, Vector[RequestId]],
    fastSettlementSignal: Deferred[IO, Unit],
    slowCoverageSignal: Deferred[IO, Unit],
    fastSettlementTarget: Deferred[IO, Set[RequestId]],
    slowCoverageTarget: Deferred[IO, Set[Int]],
    effectsLanded: Ref[IO, Set[TransactionHash]],
    effectsLandedSignal: Deferred[IO, Unit],
    effectsLandedTarget: Deferred[IO, List[BlockExpectation]],
    fallbackEnteredSignal: Deferred[IO, TransactionHash],
)

/** Selects the persistence backend for stage4 peers.
  *
  *   - [[InMemory]] (default) — per-peer [[InMemoryBackendStore]]. No disk I/O; test isolation is
  *     automatic.
  *   - [[RocksDb]] — per-peer RocksDB directory under `root`. Each peer gets `root/peer-N/`. The
  *     default `root` is a fresh tempdir per `RocksDb()` invocation so concurrent runs don't
  *     collide; pass an explicit path to keep the store around (e.g. for inspection). Use this when
  *     reproducing on-disk compaction / batching behavior.
  */
enum BackendMode:
    case InMemory
    case RocksDb(root: Path = Files.createTempDirectory("stage4-rocksdb-"))

case class Stage4Suite(
    label: String = "stage4",
    nPeers: Int = 2,
    nCoilPeers: Int = 0,
    nCommands: Int = 10,
    transportMode: TransportMode = TransportMode.Direct,
    backendMode: BackendMode = BackendMode.InMemory,
) extends ModelBasedSuite:

    override type Env = Unit
    override type State = ModelState
    override type Sut = Stage4Sut

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("Stage4.Suite"))

    /** TestControl is incompatible with real sockets — virtual time doesn't drive the OS scheduler
      * that owns the WS connection. Direct mode keeps virtual time; WS mode runs on the real clock.
      */
    override def useTestControl: Boolean = transportMode match {
        case TransportMode.Direct    => true
        case TransportMode.WebSocket => false
    }

    override def scenarioGen: ScenarioGen[ModelState, Stage4Sut] = Stage4ScenarioGen

    override def commandGenTweaker: [A] => Gen[A] => Gen[A] = [A] =>
        (g: Gen[A]) => Gen.resize(nCommands, g)

    override def onTestCaseGenerated(
        initialState: ModelState,
        commands: List[AnyCommand[ModelState, Stage4Sut]]
    ): IO[Unit] =
        for
            _ <- super.onTestCaseGenerated(initialState, commands)
            table <- Stage4Runner.renderTable(initialState, commands)(using log)
            _ <- log.info(table)
        yield ()

    override def initEnv: PropertyM[IO, Unit] = PropertyM.run(IO.unit)

    override def genInitialState(env: Unit): PropertyM[IO, ModelState] =
        PropertyM.pick(
          Stage4Suite.genInitialState(
            nPeers = nPeers,
            nCoilPeers = nCoilPeers,
            useTestControl = useTestControl
          )
        )

    override def canStartupNewSut(): Boolean = true

    // Resources acquired:
    //   1. `ActorSystem[IO]
    //   2. Per-head-peer `BackendStore` (RocksDB on disk or in-memory)
    //   3. `WsNetwork` via `transportSetup` — `Resource.pure` in Direct mode; in WebSocket, the web socket servers
    //   4. Per-coil-peer `InMemoryBackendStore` (always in-memory).
    //   5. `Stage4Sut` via `Resource.make` — release cancels the per-peer CardanoLiaison
    //      polling fibers and the event-stream error drainer.
    override def sutResource(state: ModelState): Resource[IO, Stage4Sut] = {
        val multiNodeConfig = state.params.multiNodeConfig
        val cardanoInfo = multiNodeConfig.headConfig.cardanoInfo
        val peers = multiNodeConfig.nodeConfigs.keys.toSeq.sortBy(p => p: Int)
        val coilConfigs = state.params.coilNodeConfigs

        // Advance simulated clock to the head's start epoch BEFORE creating the ActorSystem.
        // With TestControl, IO.sleep advances the virtual clock only while no actor fibers
        // exist; once actors are started their ping loops compete with tickOne, so the sleep
        // must come first (same pattern as stage1 Suite).
        //
        // In WS mode (real clock) we skip this: the configured start epoch is potentially
        // years in the future, sleeping would block the test. Block production almost
        // certainly won't fire under WS+real-clock until the model anchors startTime to
        // `Instant.now() + small`. v1 of WS mode validates the transport layer; full property
        // validation under WS is a follow-up.
        val startEpochMs = state.currentModelTime.getEpochSecond * 1000L

        // ------ Pre-system IO: clock alignment. ------
        val preSystem: IO[Unit] = for {
            // TestControl branch: jump the virtual clock from 0 to the head's start epoch.
            // Without TestControl this would be a literal multi-decade real sleep, so it must
            // be gated on `useTestControl`. The non-TestControl analogue is the
            // `state.takeoffTime` wait below — `genInitialState` anchors `currentModelTime`
            // at `now + 60s` for that mode, so wall-clock sleeping until the anchor is the
            // right move.
            _ <- IO.whenA(useTestControl)(
              IO.sleep(FiniteDuration(startEpochMs, TimeUnit.MILLISECONDS))
            )
            // Non-TestControl branch: wait until the wall clock reaches `takeoffTime` so the
            // model clock and the SUT wall clock coincide at command 1. Abort if setup
            // overran the budget — better a loud failure than a test that starts with
            // already-violated timing. Same shape as stage 1.
            _ <- state.takeoffTime match {
                case None => IO.unit
                case Some(t) =>
                    IO.realTimeInstant.flatMap { now =>
                        if now.isAfter(t) then
                            IO.raiseError(
                              RuntimeException(
                                "Stage4 sutResource: initialization took too long " +
                                    s"(takeoff: $t, now: $now)"
                              )
                            )
                        else
                            val sleepMs = t.toEpochMilli - now.toEpochMilli
                            val tickMs  = 5_000L
                            val ticks   = sleepMs / tickMs
                            val remMs   = sleepMs % tickMs
                            log.info(s"WS mode: sleeping ${sleepMs / 1000}s until takeoff") >>
                                (0L until ticks).toList.traverse_ { i =>
                                    IO.sleep(tickMs.millis) >>
                                        log.info(
                                          s"WS takeoff in ${(sleepMs - (i + 1) * tickMs) / 1000}s"
                                        )
                                } >>
                                IO.sleep(remMs.millis)
                    }
            }
        } yield ()

        // ------ Post-system, pre-stack IO: backend + per-peer Deferred/Ref maps. ------
        val postSystem: IO[PostSystemState] = {
            // All peers share one mock L1 backend, starting from the merged pre-init UTxOs.
            // The head initialization tx is submitted by the protocol through normal operation.
            val genesisUtxos = state.preinitPeerUtxosL1.values.reduce(_ ++ _)
            for {
                cardanoBackend <- CardanoBackendMock.mockIO(
                  initialState = MockState(genesisUtxos),
                  mkContext = slot =>
                      Context(
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
                pendingConnsMap <- peers
                    .traverse { peerNum =>
                        Deferred[IO, HeadMultisigRegimeManager.Connections].map(peerNum -> _)
                    }
                    .map(_.toMap)
                // Per-peer Ref capturing every BlockBrief.Intermediate the peer's JointLedger emits.
                // Populated via a ContraTracer sink attached to JL; consumed by propLiveness /
                // propDepositTiming / propValidRatio / propStackCoverage.
                blockBriefsMap <- peers
                    .traverse { peerNum =>
                        Ref[IO].of(Vector.empty[BlockBrief.Intermediate]).map(peerNum -> _)
                    }
                    .map(_.toMap)
                stacksMap <- peers
                    .traverse { peerNum =>
                        Ref[IO].of(Vector.empty[Stack.HardConfirmed]).map(peerNum -> _)
                    }
                    .map(_.toMap)
                coilStacksMap <- coilConfigs
                    .traverse { coilConfig =>
                        val coilNum = coilConfig.ownPeerId match {
                            case PeerId.Coil(n) => n
                            case PeerId.Head(_) =>
                                throw new IllegalStateException(
                                  "coil node config carries a head peer id"
                                )
                        }
                        Ref[IO].of(Vector.empty[Stack.HardConfirmed]).map(coilNum -> _)
                    }
                    .map(_.toMap)
                submittedRequestIds  <- Ref[IO].of(Vector.empty[RequestId])
                fastSettlementSignal <- IO.deferred[Unit]
                slowCoverageSignal   <- IO.deferred[Unit]
                fastSettlementTarget <- IO.deferred[Set[RequestId]]
                slowCoverageTarget   <- IO.deferred[Set[Int]]
                effectsLanded        <- Ref[IO].of(Set.empty[TransactionHash])
                effectsLandedSignal  <- IO.deferred[Unit]
                effectsLandedTarget  <- IO.deferred[List[BlockExpectation]]
                fallbackEnteredSignal <- IO.deferred[TransactionHash]
            } yield PostSystemState(
              cardanoBackend = cardanoBackend,
              pendingConnsMap = pendingConnsMap,
              blockBriefsMap = blockBriefsMap,
              stacksMap = stacksMap,
              coilStacksMap = coilStacksMap,
              submittedRequestIds = submittedRequestIds,
              fastSettlementSignal = fastSettlementSignal,
              slowCoverageSignal = slowCoverageSignal,
              fastSettlementTarget = fastSettlementTarget,
              slowCoverageTarget = slowCoverageTarget,
              effectsLanded = effectsLanded,
              effectsLandedSignal = effectsLandedSignal,
              effectsLandedTarget = effectsLandedTarget,
              fallbackEnteredSignal = fallbackEnteredSignal,
            )
        }

        case class PeerMrm(
            mrm: HeadMultisigRegimeManager,
            backendStore: BackendStore[IO],
        )

        def buildPeerMrm(
            peerNum: HeadPeerNumber,
            system: ActorSystem[IO],
            cardanoBackend: CardanoBackend[IO],
            peerTransport: Resource[
              IO,
              ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => PeerTransport
            ],
            hubCoilTransport: Option[Resource[
              IO,
              ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => HubTransport
            ]],
            blockBriefsMap: Map[HeadPeerNumber, Ref[IO, Vector[BlockBrief.Intermediate]]],
            stacksMap: Map[HeadPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
            fastSettlementSignal: Deferred[IO, Unit],
            slowCoverageSignal: Deferred[IO, Unit],
            fastSettlementTarget: Deferred[IO, Set[RequestId]],
            slowCoverageTarget: Deferred[IO, Set[Int]],
            effectsLanded: Ref[IO, Set[TransactionHash]],
            effectsLandedSignal: Deferred[IO, Unit],
            effectsLandedTarget: Deferred[IO, List[BlockExpectation]],
            fallbackEnteredSignal: Deferred[IO, TransactionHash],
        ): Resource[IO, PeerMrm] = {
            val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)

            // HMRM-level tracer = slf4j sink + per-peer capture observers (SCA stacks + JL briefs +
            // CL TxSubmitting). Single source of truth feeds both the logger and the test's
            // assertion refs.
            val slf4jMrm: ContraTracer[IO, HeadMultisigRegimeManagerEvent] =
                Slf4jTracer.sink.contramap(
                  HeadMultisigRegimeManagerEventFormat.humanFormat(peerNum)
                )
            val mrmTracer =
                slf4jMrm |+|
                    Observers.captureStackHardConfirmed(
                      peerNum,
                      stacksMap,
                      slowCoverageSignal,
                      slowCoverageTarget,
                    ) |+|
                    Observers.captureBriefProduced(
                      peerNum,
                      blockBriefsMap,
                      fastSettlementSignal,
                      fastSettlementTarget,
                    ) |+|
                    Observers.captureTxSubmitting(
                      effectsLanded,
                      effectsLandedSignal,
                      effectsLandedTarget,
                    ) |+|
                    Observers.captureFallbackEntered(fallbackEnteredSignal)

            val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)

            openPeerBackend(
              peerNum,
              Cf.mkAll(
                headPeers = multiNodeConfig.headConfig.headPeerNums.toList,
                coilPeers = multiNodeConfig.headConfig.coilPeers.coilPeerNumbers,
                hubs = multiNodeConfig.headConfig.coilPeers.hubHeadPeerNumbers
              ),
              persistenceTracer
            ).flatMap { backendStore =>
                for {
                    persistence <- Resource.eval {
                        given CardanoNetwork.Section = nodeConfig
                        Persistence.fromBackend(backendStore, persistenceTracer)
                    }
                    l2Ledger <- Resource.eval(EutxoL2Ledger(nodeConfig))
                    mrm <- HeadMultisigRegimeManager.resource(
                      nodeConfig,
                      cardanoBackend,
                      l2Ledger,
                      persistence,
                      mrmTracer,
                      peerTransport,
                      hubCoilTransport,
                    )
                    _ <- Resource.eval(system.actorOf(mrm, s"hmrm-$peerNum"))
                } yield PeerMrm(mrm, backendStore)
            }
        }

        def acquireSutFromMrms(
            system: ActorSystem[IO],
            cardanoBackend: CardanoBackend[IO],
            peerMrms: Map[HeadPeerNumber, PeerMrm],
            coilMrms: Map[CoilPeerNumber, CoilMrm],
            blockBriefsMap: Map[HeadPeerNumber, Ref[IO, Vector[BlockBrief.Intermediate]]],
            stacksMap: Map[HeadPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
            coilStacksMap: Map[CoilPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
            submittedRequestIds: Ref[IO, Vector[RequestId]],
            fastSettlementSignal: Deferred[IO, Unit],
            slowCoverageSignal: Deferred[IO, Unit],
            fastSettlementTarget: Deferred[IO, Set[RequestId]],
            slowCoverageTarget: Deferred[IO, Set[Int]],
            effectsLanded: Ref[IO, Set[TransactionHash]],
            effectsLandedSignal: Deferred[IO, Unit],
            effectsLandedTarget: Deferred[IO, List[BlockExpectation]],
            fallbackEnteredSignal: Deferred[IO, TransactionHash],
        ): IO[Stage4Sut] =
            for {
                peerConnections <- peerMrms.toList
                    .traverse { case (peerNum, peerMrm) =>
                        peerMrm.mrm.connectionsDeferred.get.map(peerNum -> _)
                    }
                    .map(_.toMap)
                coilConnections <- coilMrms.toList
                    .traverse { case (coilNum, coilMrm) =>
                        coilMrm.mrm.connectionsDeferred.get.map(coilNum -> _)
                    }
                    .map(_.toMap)
                sutErrors <- Ref[IO].of(List.empty[String])
                errorDrainer <- system.eventStream.take
                    .flatMap {
                        case e: ActorError if e.cause != ActorError.NoCause =>
                            sutErrors.update(_ :+ s"[${e.logSource}] ${e.cause.getMessage}")
                        case _ => IO.unit
                    }
                    .foreverM
                    .start
                headTickFibers <- peerConnections.toList.traverse { case (peerNum, conns) =>
                    val pollingPeriod = multiNodeConfig
                        .nodeConfigs(peerNum)
                        .nodeOperationMultisigConfig
                        .cardanoLiaisonPollingPeriod
                    (IO.sleep(pollingPeriod) >>
                        (conns.cardanoLiaison ! CardanoLiaison.Timeout)).foreverM.start
                }
                coilTickFibers <- coilConnections.toList.traverse { case (coilNum, conns) =>
                    val pollingPeriod = coilMrms(coilNum).config
                        .nodeOperationMultisigConfig
                        .cardanoLiaisonPollingPeriod
                    (IO.sleep(pollingPeriod) >>
                        (conns.cardanoLiaison ! CardanoLiaison.Timeout)).foreverM.start
                }
            } yield Stage4Sut(
              static = Stage4SutStatic(
                system = system,
                cardanoBackend = cardanoBackend,
                peers = peerConnections.map { case (peerNum, conns) =>
                    peerNum -> Stage4PeerHandle(
                      conns.requestSequencer.getOrElse(
                        sys.error(s"head peer $peerNum missing RequestSequencer")
                      )
                    )
                },
                errorDrainer = errorDrainer,
                liaisonTickFibers = headTickFibers ++ coilTickFibers,
                backendStores = peerMrms.map { case (peerNum, peerMrm) =>
                    peerNum -> peerMrm.backendStore
                },
                log = Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("Stage4.Sut")),
              ),
              mutable = Stage4SutMutable(
                sutErrors = sutErrors,
                blockBriefs = blockBriefsMap,
                stacks = stacksMap,
                coilStacks = coilStacksMap,
                submittedRequestIds = submittedRequestIds,
                fastSettlementSignal = fastSettlementSignal,
                slowCoverageSignal = slowCoverageSignal,
                fastSettlementTarget = fastSettlementTarget,
                slowCoverageTarget = slowCoverageTarget,
                effectsLanded = effectsLanded,
                effectsLandedSignal = effectsLandedSignal,
                effectsLandedTarget = effectsLandedTarget,
                fallbackEnteredSignal = fallbackEnteredSignal,
              ),
            )

        case class CoilMrm(
            mrm: CoilMultisigRegimeManager,
            backendStore: BackendStore[IO],
            config: NodeConfig,
        )

        /** Concrete WS handle + the OS-assigned port the NodeWsServer bound to. Carried in
          * [[HeadNetwork.ws]] so the outer for-comprehension can compute remote URIs from real
          * bound ports (post-bind) and start every peer's dialers in a second pass.
          */
        case class WsBinding(
            wsPeerTransport: WsPeerTransport,
            boundPort: Int,
        )

        case class HeadNetwork(
            peerTransport: PeerTransport,
            hubTransport: Option[HubTransport],
            ws: Option[WsBinding],
        )

        // Per-head-peer transports: head-mesh PeerTransport (always) plus, for hub head peers, the
        // hub-side HubTransport for the hub↔coil link. WS mode mounts both routes on ONE shared
        // NodeWsServer per peer (bound on port 0 — OS-assigned ephemeral — so two WS test instances
        // never collide); Direct mode wires them to the shared in-process registries instead.
        def headNetworkFor(
            peerNum: HeadPeerNumber,
            inProcessRegistry: InProcessPeerTransport.Registry,
            hubCoilRegistry: Option[InProcessHubCoilTransport.Registry],
        ): Resource[IO, HeadNetwork] = {
            val ownHeadPeerId = headPeerId(multiNodeConfig, peerNum)
            val hubbedCoils = multiNodeConfig.headConfig.hubbedCoilPeerNums(peerNum)
            transportMode match {
                case TransportMode.Direct =>
                    for {
                        peerT <- Resource.eval(
                          InProcessPeerTransport.create(ownHeadPeerId, inProcessRegistry)
                        )
                        hubT <-
                            if hubbedCoils.isEmpty then
                                Resource.pure[IO, Option[HubTransport]](None)
                            else
                                hubCoilRegistry match {
                                    case None =>
                                        Resource.eval(
                                          IO.raiseError(
                                            new IllegalStateException(
                                              s"head peer $peerNum hubs coil peers in Direct mode " +
                                                  "but no hubCoilRegistry was allocated"
                                            )
                                          )
                                        )
                                    case Some(reg) =>
                                        Resource
                                            .eval(InProcessHubCoilTransport.Hub.create(reg))
                                            .map(h => Some(h: HubTransport))
                                }
                    } yield HeadNetwork(peerT, hubT, None)
                case TransportMode.WebSocket =>
                    given CardanoNetwork.Section = multiNodeConfig.headConfig
                    val remoteIds: List[HeadPeerId] =
                        peers.filterNot(_ == peerNum).map(headPeerId(multiNodeConfig, _)).toList
                    val ptTracer =
                        Slf4jTracer.sink.contramap(PeerTransportEventFormat.humanFormat(peerNum))
                    val nwsTracer =
                        Slf4jTracer.sink.contramap(NodeWsServerEventFormat.humanFormat(peerNum))
                    val hubTracer =
                        Slf4jTracer.sink.contramap(HubWsTransportEventFormat.humanFormat(peerNum))
                    val bindHost = host"127.0.0.1"
                    for {
                        peerT <- Resource.eval(
                          WsPeerTransport.create(ownHeadPeerId, remoteIds, ptTracer)
                        )
                        hubTConcrete: Option[HubWsTransport] <-
                            if hubbedCoils.isEmpty then
                                Resource.pure[IO, Option[HubWsTransport]](None)
                            else
                                Resource
                                    .eval(HubWsTransport.create(hubbedCoils, hubTracer))
                                    .map(Some(_))
                        meshRoute = (wsb: WebSocketBuilder2[IO]) => peerT.routes(wsb)
                        hubRoutes = hubTConcrete.toList.map(h =>
                            (wsb: WebSocketBuilder2[IO]) => h.routes(wsb)
                        )
                        server <- NodeWsServer.resource(
                          bindHost,
                          Port.fromInt(0).get,
                          meshRoute :: hubRoutes,
                          nwsTracer,
                        )
                        boundPort = server.address.getPort
                    } yield HeadNetwork(
                      peerT,
                      hubTConcrete.map(h => h: HubTransport),
                      Some(WsBinding(peerT, boundPort)),
                    )
            }
        }

        // Per-coil-peer uplink transport toward its single hub. Direct mode uses the shared
        // in-process hub-coil registry; WS mode dials the hub's `/hub` route at the hub's
        // post-bind ephemeral port (passed in via `hubBoundPort`).
        def coilUplinkFor(
            coilNum: CoilPeerNumber,
            hubCoilRegistry: InProcessHubCoilTransport.Registry,
            wsClient: Option[WSClient[IO]],
            hubBoundPort: Option[Int],
        ): Resource[
          IO,
          ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => CoilTransport
        ] = transportMode match {
            case TransportMode.Direct =>
                Resource
                    .eval(InProcessHubCoilTransport.Coil.create(coilNum, hubCoilRegistry))
                    .map(t =>
                        (_: ActorContext[IO, HeadMultisigRegimeManager.Request, Any]) =>
                            t: CoilTransport
                    )
            case TransportMode.WebSocket =>
                given CardanoNetwork.Section = multiNodeConfig.headConfig
                val port = hubBoundPort.getOrElse(
                  sys.error(s"coil $coilNum's hub has no bound port — head networks not built yet?")
                )
                val hubUri = Uri.unsafeFromString(s"ws://127.0.0.1:$port/hub")
                val cpwtTracer =
                    Slf4jTracer.sink.contramap(CoilPeerWsTransportEventFormat.humanFormat(coilNum))
                val client = wsClient.getOrElse(
                  sys.error("WebSocket transport requires a shared JdkWSClient")
                )
                for {
                    t <- Resource.eval(CoilPeerWsTransport.create(coilNum, cpwtTracer))
                    _ <- t.startDialer(client, hubUri)
                } yield (_: ActorContext[IO, HeadMultisigRegimeManager.Request, Any]) =>
                    t: CoilTransport
        }

        // ------ Per-coil-peer follower stack: full CoilMultisigRegimeManager + its own in-memory
        // ------ backend + persistence + L2 ledger, with the SCA capture sink wired into the
        // ------ MRM tracer so hard-confirmed stacks land in `coilStacksRef`.
        def buildCoilMrm(
            coilConfig: NodeConfig,
            coilNum: CoilPeerNumber,
            system: ActorSystem[IO],
            cardanoBackend: CardanoBackend[IO],
            coilTransport: Resource[
              IO,
              ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => CoilTransport
            ],
            coilStacksRef: Ref[IO, Vector[Stack.HardConfirmed]],
            effectsLanded: Ref[IO, Set[TransactionHash]],
            effectsLandedSignal: Deferred[IO, Unit],
            effectsLandedTarget: Deferred[IO, List[BlockExpectation]],
            fallbackEnteredSignal: Deferred[IO, TransactionHash],
        ): Resource[IO, CoilMrm] = {
            // Reuse the head-typed event format with a synthetic peer number (matches the legacy
            // labelling); a coil-specific format is a separate follow-up.
            val labelNum = HeadPeerNumber(nPeers + coilNum.convert)
            val slf4jMrm: ContraTracer[IO, HeadMultisigRegimeManagerEvent] =
                Slf4jTracer.sink.contramap(
                  HeadMultisigRegimeManagerEventFormat.humanFormat(labelNum)
                )
            // Coil also runs a CardanoLiaison and can submit backbone txs to L1 (e.g. the init tx
            // in some races), so the TxSubmitting + FallbackToRuleBased capture observers must be
            // wired to its tracer too — otherwise the test's landed set and fallback signal miss
            // coil-side submissions.
            val mrmTracer =
                slf4jMrm |+|
                    Observers.captureCoilStackHardConfirmed(coilStacksRef) |+|
                    Observers.captureTxSubmitting(
                      effectsLanded,
                      effectsLandedSignal,
                      effectsLandedTarget,
                    ) |+|
                    Observers.captureFallbackEntered(fallbackEnteredSignal)
            val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)

            InMemoryBackendStore.open(persistenceTracer).flatMap { backendStore =>
                for {
                    persistence <- Resource.eval {
                        given CardanoNetwork.Section = coilConfig
                        Persistence.fromBackend(backendStore, persistenceTracer)
                    }
                    l2Ledger <- Resource.eval(EutxoL2Ledger(coilConfig))
                    mrm <- CoilMultisigRegimeManager.resource(
                      coilConfig,
                      cardanoBackend,
                      l2Ledger,
                      persistence,
                      mrmTracer,
                      coilTransport,
                    )
                    _ <- Resource.eval(system.actorOf(mrm, s"cmrm-${coilNum.convert}"))
                } yield CoilMrm(mrm, backendStore, coilConfig)
            }
        }

        for {
            _      <- Resource.eval(preSystem)
            system <- ActorSystem[IO](label)
            pss    <- Resource.eval(postSystem)
            inProcessRegistry <- Resource.eval(InProcessPeerTransport.emptyRegistry)
            // Allocated only when this scenario has coil peers; in Direct mode it backs the
            // in-process hub↔coil routing, and in WS mode the field stays `None` (the hub/coil
            // WS transports own their own routing). Heads with no hubbed coils pass `None`
            // either way.
            hubCoilRegistry <-
                if coilConfigs.isEmpty then
                    Resource.pure[IO, Option[InProcessHubCoilTransport.Registry]](None)
                else
                    Resource
                        .eval(InProcessHubCoilTransport.emptyRegistry)
                        .map(Some(_))
            // Shared WS client: allocated once for WebSocket mode, unused for Direct.
            wsClient <- transportMode match {
                case TransportMode.Direct =>
                    Resource.pure[IO, Option[WSClient[IO]]](None)
                case TransportMode.WebSocket =>
                    Resource.eval(JdkWSClient.simple[IO]).map(Some(_))
            }
            // Pass 1: allocate every head peer's transports + bind their NodeWsServers (WS mode on
            // port 0 ⇒ OS-assigned ephemeral). Each `HeadNetwork` carries the bound port; dialers
            // are deferred to Pass 2 so URIs can be built from the real port map.
            headNetworks <- peers.toList
                .traverse { peerNum =>
                    headNetworkFor(peerNum, inProcessRegistry, hubCoilRegistry)
                        .map(peerNum -> _)
                }
                .map(_.toMap)
            // Pass 2 (WS only): now that every peer's server is bound and we know its ephemeral
            // port, build the `HeadPeerId -> Uri` map and start each peer's dialers against the
            // ones with higher peerNum (lower dials higher; matches the production wiring).
            _ <- transportMode match {
                case TransportMode.Direct => Resource.pure[IO, Unit](())
                case TransportMode.WebSocket =>
                    val client = wsClient.getOrElse(
                      sys.error("WebSocket transport requires a shared JdkWSClient")
                    )
                    val peerHeadUris: Map[HeadPeerId, Uri] = peers.map { p =>
                        val port = headNetworks(p).ws
                            .map(_.boundPort)
                            .getOrElse(sys.error(s"peer $p has no WS binding in WS mode"))
                        headPeerId(multiNodeConfig, p) -> Uri.unsafeFromString(
                          s"ws://127.0.0.1:$port/head"
                        )
                    }.toMap
                    peers.toList.traverse_ { peerNum =>
                        val ownId = headPeerId(multiNodeConfig, peerNum)
                        val ws = headNetworks(peerNum).ws.get
                        ws.wsPeerTransport.startDialers(client, peerHeadUris - ownId)
                    }
            }
            peerMrms <- peers.toList
                .traverse { peerNum =>
                    val net = headNetworks(peerNum)
                    val peerFactory: Resource[
                      IO,
                      ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => PeerTransport
                    ] =
                        Resource.pure(
                          (_: ActorContext[IO, HeadMultisigRegimeManager.Request, Any]) =>
                              net.peerTransport
                        )
                    val hubFactory: Option[Resource[
                      IO,
                      ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => HubTransport
                    ]] = net.hubTransport.map { h =>
                        Resource.pure(
                          (_: ActorContext[IO, HeadMultisigRegimeManager.Request, Any]) => h
                        )
                    }
                    buildPeerMrm(
                      peerNum,
                      system,
                      pss.cardanoBackend,
                      peerFactory,
                      hubFactory,
                      pss.blockBriefsMap,
                      pss.stacksMap,
                      pss.fastSettlementSignal,
                      pss.slowCoverageSignal,
                      pss.fastSettlementTarget,
                      pss.slowCoverageTarget,
                      pss.effectsLanded,
                      pss.effectsLandedSignal,
                      pss.effectsLandedTarget,
                      pss.fallbackEnteredSignal,
                    ).map(peerNum -> _)
                }
                .map(_.toMap)
            // Per-coil follower stack. The MRM gets its uplink transport via `coilUplinkFor`; in
            // Direct mode this shares `hubCoilRegistry` with the hub head peer's `HubTransport`.
            coilMrms <- coilConfigs
                .traverse { coilConfig =>
                    val coilNum = coilConfig.ownPeerId match {
                        case PeerId.Coil(n) => n
                        case PeerId.Head(_) =>
                            throw new IllegalStateException(
                              "coil node config carries a head peer id"
                            )
                    }
                    val hubNum = coilConfig
                        .coilPeerHub(coilNum)
                        .getOrElse(
                          throw new IllegalStateException(
                            s"no hub configured for coil peer $coilNum"
                          )
                        )
                    val registry = hubCoilRegistry.getOrElse(
                      throw new IllegalStateException(
                        "coilConfigs is non-empty but hubCoilRegistry was not allocated"
                      )
                    )
                    val hubBoundPort = headNetworks(hubNum).ws.map(_.boundPort)
                    buildCoilMrm(
                      coilConfig,
                      coilNum,
                      system,
                      pss.cardanoBackend,
                      coilUplinkFor(coilNum, registry, wsClient, hubBoundPort),
                      pss.coilStacksMap(coilNum),
                      pss.effectsLanded,
                      pss.effectsLandedSignal,
                      pss.effectsLandedTarget,
                      pss.fallbackEnteredSignal,
                    ).map(coilNum -> _)
                }
                .map(_.toMap)
            sut <- Resource.make(
              acquireSutFromMrms(
                system,
                pss.cardanoBackend,
                peerMrms,
                coilMrms,
                pss.blockBriefsMap,
                pss.stacksMap,
                pss.coilStacksMap,
                pss.submittedRequestIds,
                pss.fastSettlementSignal,
                pss.slowCoverageSignal,
                pss.fastSettlementTarget,
                pss.slowCoverageTarget,
                pss.effectsLanded,
                pss.effectsLandedSignal,
                pss.effectsLandedTarget,
                pss.fallbackEnteredSignal,
              )
            )(sut =>
                sut.static.liaisonTickFibers.traverse_(_.cancel) >>
                    IO.sleep(100.millis) >>
                    sut.static.errorDrainer.cancel
            )
        } yield sut
    }

    /** This peer's [[HeadPeerId]] — derived from its number + the head-set size; `NodeConfig` does
      * not expose a head-specific id.
      */
    private def headPeerId(
        multiNodeConfig: MultiNodeConfig,
        peerNum: HeadPeerNumber
    ): HeadPeerId =
        HeadPeerId(peerNum, multiNodeConfig.nHeadPeers)

    override def beforeFinalize(lastState: ModelState, sut: Stage4Sut): IO[Prop] = {
        // Race the happy-path drain against the fallback-entered signal. If any peer's CL
        // successfully submits a `FallbackToRuleBased`, abandon the analysis and fail with
        // a clear message — the properties below (liveness / coverage / effects-landed) don't
        // model rule-based; waiting on them after fallback would spin the CL polling loop
        // until the outer test timeout, accumulating the InitWindowElapsed warn flood.
        val happyPathProp: IO[Prop] = for
            _ <- log.warn("beforeFinalize")
            submitted <- sut.mutable.submittedRequestIds.get
            // Arm the fast-cycle drain: publish the final submitted set so the JL capture sink
            // knows the target. The sink fires fastSettlementSignal only after this is set,
            // preventing mid-run firing against a partial submittedRequestIds snapshot.
            _ <- sut.mutable.fastSettlementTarget.complete(submitted.toSet)
            // One-time coverage check: if all IDs already landed before we armed the target,
            // fire the signal ourselves (no new brief will arrive to trigger the sink).
            allBriefs <- sut.mutable.blockBriefs.values.toList.traverse(_.get).map(_.flatten)
            seen       = allBriefs
                             .flatMap(br =>
                                 br.events.map(_._1) ++ br.depositsAbsorbed ++ br.depositsRefunded
                             )
                             .toSet
            _ <- IO.whenA(submitted.forall(seen.contains))(
                     sut.mutable.fastSettlementSignal.complete(()).void
                 )
            _ <- IO.whenA(submitted.nonEmpty)(sut.mutable.fastSettlementSignal.get)
            // Arm the slow-cycle drain: freeze the block nums that must be covered. Done after
            // the fast drain so any blocks produced during that wait are included in the target.
            blockNums <- sut.mutable.blockBriefs.values.toList
                             .traverse(_.get)
                             .map(_.flatten.map(b => (b.blockNum: Int)).toSet)
            _ <- sut.mutable.slowCoverageTarget.complete(blockNums)
            // One-time coverage check across ALL peers — matching the sink condition so a spurious
            // signal fire can't race ahead of any peer's stacksMap update.
            allPeersStacks <- sut.mutable.stacks.values.toList.traverse(_.get)
            allCovered      = blockNums.isEmpty ||
                                  allPeersStacks.forall { peerStacks =>
                                      blockNums.forall { bn =>
                                          peerStacks.exists { s =>
                                              (s.brief.firstBlockNum: Int) <= bn &&
                                              bn <= (s.brief.lastBlockNum: Int)
                                          }
                                      }
                                  }
            _ <- IO.whenA(allCovered)(sut.mutable.slowCoverageSignal.complete(()).void)
            _ <- IO.whenA(blockNums.nonEmpty)(sut.mutable.slowCoverageSignal.get)
            // Arm the effects-landed drain: now that the slow cycle has reached agreement on
            // every block, derive the backbone expectations from the canonical peer's stacks and
            // wait for the TxSubmitting sink to observe enough hashes to satisfy them. Gap
            // between slow signal and this one = the StackComposer rate-limit delay.
            canonicalStacksForTarget <- sut.mutable.stacks.toList
                                            .traverse { case (p, ref) => ref.get.map(p -> _) }
                                            .map { byPeer =>
                                                val sorted = byPeer.toMap.toSeq.sortBy(_._1: Int)
                                                sorted.headOption.map(_._2).getOrElse(Vector.empty)
                                            }
            expectations = EffectsLanded.expectations(canonicalStacksForTarget)
            _ <- sut.mutable.effectsLandedTarget.complete(expectations)
            // One-time check: if every relevant expectation is already satisfied by the hashes
            // we've observed so far, fire the signal ourselves (no new TxSubmitting will arrive).
            landedNow <- sut.mutable.effectsLanded.get
            _ <- IO.whenA(EffectsLanded.isComplete(landedNow, expectations))(
                     sut.mutable.effectsLandedSignal.complete(()).void
                 )
            _ <- IO.whenA(expectations.nonEmpty)(sut.mutable.effectsLandedSignal.get)
            errors <- sut.mutable.sutErrors.get
            // Snapshot every observer-written Ref ONCE here, after all drain signals have fired,
            // and reuse this single frozen view for the whole analysis. Re-reading these Refs
            // separately (here and again inside analyzeBlockBriefs) races the observer fibers
            // under WS (real clock, no TestControl tick-drain), comparing quantities derived from
            // inconsistent snapshots — the source of the intermittent WS failure.
            briefsByPeer <- sut.mutable.blockBriefs.toList
                                .traverse { case (p, ref) => ref.get.map(p -> _) }
                                .map(_.toMap)
            stacksByPeer <- sut.mutable.stacks.toList
                                .traverse { case (p, ref) => ref.get.map(p -> _) }
                                .map(_.toMap)
            coilStacksByCoil <- sut.mutable.coilStacks.toList
                                    .traverse { case (c, ref) => ref.get.map(c -> _) }
                                    .map(_.toMap)
            submittedIds <- sut.mutable.submittedRequestIds.get
            sortedPeers = stacksByPeer.keys.toSeq.sortBy(p => p: Int)
            // propEffectsLanded must check exactly what effectsLandedSignal confirmed landed, so it
            // uses the stacks frozen when effectsLandedTarget was armed (above), not the post-signal
            // snapshot — which could include a trailing stack whose txs have not landed yet.
            analysisProp <- analyzeBlockBriefs(
                              lastState,
                              sut,
                              briefsByPeer,
                              stacksByPeer,
                              coilStacksByCoil,
                              submittedIds,
                              canonicalStacksForTarget,
                            )
            persistenceProp <- analyzePersistence(sut, stacksByPeer, sortedPeers)
            props = analysisProp && persistenceProp
        yield
            if errors.nonEmpty then
                Prop.exception(RuntimeException(s"SUT actor errors:\n${errors.mkString("\n")}"))
            else props

        IO.race(sut.mutable.fallbackEnteredSignal.get, happyPathProp).map {
            case Left(txId) =>
                Prop.exception(
                  RuntimeException(
                    s"scenario entered rule-based fallback (txId=$txId) — outside the modeled" +
                        " happy-path regime; widen stage4 timings or cap the inter-arrival tail"
                  )
                )
            case Right(prop) => prop
        }
    }

    private def analyzeBlockBriefs(
        lastState: ModelState,
        sut: Stage4Sut,
        briefsByPeer: Map[HeadPeerNumber, Vector[BlockBrief.Intermediate]],
        stacksByPeer: Map[HeadPeerNumber, Vector[Stack.HardConfirmed]],
        coilStacksByCoil: Map[CoilPeerNumber, Vector[Stack.HardConfirmed]],
        submittedIds: Vector[RequestId],
        canonicalStacksForEffects: Vector[Stack.HardConfirmed],
    ): IO[Prop] = {
        val sortedPeers = briefsByPeer.keys.toSeq.sortBy(p => p: Int)
        val nPeers = sortedPeers.length
        val canonicalBriefs = briefsByPeer(sortedPeers.head)
        val canonicalStacks = stacksByPeer(sortedPeers.head)
        for
            _ <- log.info(
              "hard-confirmed stacks per peer: " +
                  sortedPeers
                      .map(p => s"peer${p: Int}=${stacksByPeer.getOrElse(p, Vector.empty).size}")
                      .mkString(", ")
            )

            _ <- IO.whenA(coilStacksByCoil.nonEmpty)(
              log.info(
                "coil hard-confirmed stacks: " +
                    coilStacksByCoil.toList
                        .sortBy((c, _) => c.convert)
                        .map((c, ss) => s"coil${c.convert}=${ss.size}")
                        .mkString(", ")
              )
            )

            _ <- traceBlockTable(
              canonicalBriefs,
              sortedPeers,
              briefsByPeer,
              nPeers,
              submittedIds,
              lastState,
            )

            _ <- traceStackTable(canonicalStacks, sortedPeers, stacksByPeer, nPeers)

            // Mock backend resolves instantly; one attempt is enough. Kept the (attempts, sleep)
            // knob so a Yaci / Blockfrost-backed stage4 future swap just bumps these. Checks the
            // stacks frozen when the effects-landed target was armed — consistent with the signal.
            effectsLandedProp <- EffectsLanded.propEffectsLanded(
              canonicalStacksForEffects,
              sut.static.cardanoBackend,
              log,
              attempts = 1,
              sleep = 0.seconds,
            )

            targetBlockNums <- sut.mutable.slowCoverageTarget.get

        yield propLiveness(submittedIds, canonicalBriefs) &&
            propDepositTiming(lastState.registeredDeposits, canonicalBriefs) &&
            propValidRatio(lastState, canonicalBriefs) &&
            propStackCoverage(targetBlockNums, canonicalStacks) &&
            propCoilParticipation(coilStacksByCoil, canonicalStacks) &&
            effectsLandedProp
    }

    /** Post-scenario verification of the §6 producer-side persistence writes:
      *   - **SC** (`StackComposer`) writes `Treasury` once and one `EvacuationMap` per
      *     **committed** block (each major + each last-of-partition SEC minor) at every own
      *     hard-ack stack-close (#21).
      *   - **SCA** (`SlowConsensusActor`) writes `HardConfirmation` at every hard-confirmation
      *     (#22).
      *   - **JL** (`JointLedger`) writes `BlockResult` + the `DepositMap` snapshot + its own
      *     `Block` (leader) / `SoftAck` lane entries at each own soft ack; **FCA**
      *     (`FastConsensusActor`) writes `SoftConfirmation` at each soft-confirmation; **SC** also
      *     writes its own `Stack` (leader) / `HardAck` lane entries at stack-close.
      *   - **RequestSequencer** writes the assigned request to the `Request` lane (CR1);
      *     **PeerLiaison** writes each inbound *remote* lane entry it receives (CR8).
      *
      * For each peer that observed at least one `Stack.HardConfirmed` during the scenario, this
      * property asserts:
      *   1. `Cf.HardConfirmation` holds an entry per hard-confirmed stack (= the captured stacks
      *      count, since both the observer and the persistence write happen on hard-confirmation).
      *   2. `Cf.Treasury` holds its single snapshot blob (always exactly 1 — overwritten per
      *      close).
      *   3. `Cf.EvacuationMap` holds one entry per committed block (major / SEC minor) of every
      *      Regular stack the peer hard-confirmed — the only maps that back an on-chain commitment,
      *      counted from each stack's partitions (mirroring `StackComposer.committedBlockNums`).
      *      `Initial` (stack 0) contributes nothing since `bootstrapInitialStack` doesn't go
      *      through the close paths.
      *   4. The fast side wrote: `Cf.BlockResult` / `Cf.SoftConfirmation` non-empty and
      *      `Cf.DepositMap` a singleton — a peer that hard-confirmed necessarily produced and
      *      soft-confirmed blocks first (sanity lower bounds, not exact counts).
      *   5. The satellite lanes are non-empty: `Cf.SoftAck` (every block) and `Cf.HardAck` (every
      *      confirmed stack), and `Cf.Request` (own assignments + inbound). The `Block` / `Stack`
      *      spine lanes get both own (leader) and inbound (follower) writes but are per-peer
      *      variable, so they are logged but not asserted.
      *
      * Skip a peer entirely if it never reached a hard-confirmation (the typical `nPeers < 3`
      * cold-start scenarios). The property only fires once at least one hard-confirmation actually
      * happened, which is exactly where the writes should have landed.
      */
    private def analyzePersistence(
        sut: Stage4Sut,
        stacksByPeer: Map[HeadPeerNumber, Vector[Stack.HardConfirmed]],
        sortedPeers: Seq[HeadPeerNumber]
    ): IO[Prop] = {
        sortedPeers
            .traverse { peerNum =>
                val backend = sut.static.backendStores(peerNum)
                val captured = stacksByPeer.getOrElse(peerNum, Vector.empty)
                val expectedStacks = captured.size
                // Only the blocks that back an on-chain KZG commitment get an `EvacuationMap`
                // entry — each major (the settlement's `nextKzg`) and each last-of-partition SEC
                // minor — mirroring `StackComposer.committedBlockNums` /
                // `StackEffectsBuilder.mkEffectsRegular`. So per Regular stack the count is, over
                // its partitions: Major → 1 (the major) + 1 iff it carries a trailing-minor SEC;
                // Minor → 1 (its mandatory SEC minor); Final → 0 (drains the map, commits nothing).
                // `Initial` (stack 0) goes through `bootstrapInitialStack`, never the close paths,
                // so it contributes nothing.
                val expectedEvac = captured.map { s =>
                    s.effects match {
                        case _: StackEffects.HardConfirmed.Initial => 0
                        case r: StackEffects.HardConfirmed.Regular =>
                            r.partitions.toList.map {
                                case m: PartitionEffects.Major[?] =>
                                    if m.sec.isDefined then 2 else 1
                                case _: PartitionEffects.Minor[?] => 1
                                case _: PartitionEffects.Final    => 0
                            }.sum
                    }
                }.sum
                for {
                    hardConfirmations <- countEntries(backend, Cf.HardConfirmation)
                    treasuries <- countEntries(backend, Cf.Treasury)
                    evacuationMaps <- countEntries(backend, Cf.EvacuationMap)
                    blockResults <- countEntries(backend, Cf.BlockResult)
                    softConfirmations <- countEntries(backend, Cf.SoftConfirmation)
                    depositMaps <- countEntries(backend, Cf.DepositMap)
                    // Satellites are split one CF per author (§7.1); count across every head peer's
                    // own-author CF (each peer holds all peers' entries — own + inbound).
                    softAcks <- countAcross(backend, sortedPeers.toList.map(Cf.SoftAck(_)))
                    hardAcks <- countAcross(
                      backend,
                      sortedPeers.toList.map(p => Cf.HardAck(PeerId.Head(p)))
                    )
                    requests <- countAcross(backend, sortedPeers.toList.map(Cf.Request(_)))
                    _ <- log.info(
                      s"peer${peerNum: Int} persistence: expectedHardConf=$expectedStacks " +
                          s"hardConfirmations=$hardConfirmations treasuries=$treasuries " +
                          s"evacuationMaps=$evacuationMaps (expected=$expectedEvac) " +
                          s"blockResults=$blockResults softConfirmations=$softConfirmations " +
                          s"depositMaps=$depositMaps softAcks=$softAcks hardAcks=$hardAcks " +
                          s"requests=$requests"
                    )
                } yield {
                    if expectedStacks == 0 then Prop.passed
                    else {
                        val hardOk = hardConfirmations == expectedStacks
                        val treasuryOk = treasuries == 1
                        val evacOk = evacuationMaps == expectedEvac
                        // Fast-side producer writes: a peer that hard-confirmed has necessarily
                        // produced blocks (JL's `BlockResult`) and soft-confirmed them (FCA's
                        // `SoftConfirmation`), and the deposits snapshot is a singleton.
                        val fastOk =
                            blockResults >= 1 && softConfirmations >= 1 && depositMaps == 1
                        // Lane writes (own + inbound): every peer soft-acks every block (JL) and
                        // hard-acks each stack it confirmed (SC); requests flow into the Request
                        // lane (RequestSequencer own + PeerLiaison inbound, CR1/CR8). All non-empty.
                        val laneOk = softAcks >= 1 && hardAcks >= 1 && requests >= 1
                        Prop(hardOk && treasuryOk && evacOk && fastOk && laneOk).label(
                          s"peer${peerNum: Int}: " +
                              s"hardConfirmations=$hardConfirmations expected=$expectedStacks, " +
                              s"treasuries=$treasuries (expected 1), " +
                              s"evacuationMaps=$evacuationMaps (expected $expectedEvac), " +
                              s"blockResults=$blockResults softConfirmations=$softConfirmations " +
                              s"depositMaps=$depositMaps softAcks=$softAcks hardAcks=$hardAcks " +
                              s"requests=$requests (fast >=1/>=1/==1, lanes >=1/>=1/>=1)"
                        )
                    }
                }
            }
            .map(_.foldLeft(Prop.passed)(_ && _))
    }

    /** Per-peer backend allocator chosen by [[backendMode]].
      *
      * `BackendMode.InMemory` returns a fresh `InMemoryBackendStore`. `BackendMode.RocksDb(root)`
      * opens `root/peer-N/`, creating parent directories on demand. The returned `Resource` is
      * allocated immediately in `sutResource`; cleanup currently leaks RocksDB handles until a
      * stage4-level shutdown hook lands (parallel to `errorDrainer.cancel`).
      */
    private def openPeerBackend(
        peerNum: HeadPeerNumber,
        cfs: List[Cf],
        tracer: ContraTracer[IO, PersistenceEvent]
    ): Resource[IO, BackendStore[IO]] =
        backendMode match
            case BackendMode.InMemory => InMemoryBackendStore.open(tracer)
            case BackendMode.RocksDb(root) =>
                val dir = root.resolve(s"peer-${peerNum: Int}")
                Resource.eval(IO.blocking(Files.createDirectories(dir))) >>
                    RocksDbBackendStore.open(dir, cfs, tracer)

    private def countEntries(
        backend: BackendStore[IO],
        cf: Cf
    ): IO[Int] =
        backend.cursor(cf, Array.emptyByteArray).use { c =>
            def loop(n: Int): IO[Int] =
                c.next.flatMap {
                    case None    => IO.pure(n)
                    case Some(_) => loop(n + 1)
                }
            loop(0)
        }

    /** Sum entry counts across a set of (per-author) CFs — e.g. every head peer's `SoftAck` CF, now
      * that satellites are split one CF per author (§7.1).
      */
    private def countAcross(backend: BackendStore[IO], cfs: List[Cf]): IO[Int] =
        cfs.traverse(countEntries(backend, _)).map(_.sum)

    /** With `coilQuorum` ≥ 1, no stack hard-confirms without the coil peers' acks, so
      * [[propStackCoverage]] already proves coil participation on the head side. This additionally
      * checks the relay back: each coil peer follower itself hard-confirms stacks (it received the
      * head acks + its own echo), and never hard-confirms a stack the canonical head didn't. No-op
      * for a pure-head run.
      */
    private def propCoilParticipation(
        coilStacksByCoil: Map[CoilPeerNumber, Vector[Stack.HardConfirmed]],
        canonicalStacks: Vector[Stack.HardConfirmed]
    ): Prop =
        if coilStacksByCoil.isEmpty then Prop.proved
        else
            val headStackNums = canonicalStacks.map(_.brief.stackNum).toSet
            coilStacksByCoil.toList
                .map { (coilNum, coilStacks) =>
                    val coilStackNums = coilStacks.map(_.brief.stackNum).toSet
                    (Prop(coilStacks.nonEmpty) :|
                        s"coil ${coilNum.convert} hard-confirmed no stacks") &&
                    (Prop(coilStackNums.subsetOf(headStackNums)) :|
                        s"coil ${coilNum.convert} hard-confirmed stacks absent from the head: " +
                        s"${(coilStackNums -- headStackNums).map(_.convert)}")
                }
                .foldLeft(Prop.proved)(_ && _)

    // TODO: side-channel validity-error tracking + propNoStaleRejections
    //
    // `JointLedger.rejectEvent` records every rejection as `(reqId, ValidityFlag.Invalid)` in
    // the in-progress block, so propLiveness sees the request landed in a brief — it cannot
    // distinguish:
    //   1. Reordering-induced ledger errors (e.g. `BadAllInputsUTxOException`) — legitimate
    //      effect of stage4's leader scheduling, expected at stress.
    //   2. Validity-window expirations (`UserRequestError.BlockOutOfRequestValidityInterval`)
    //      — symptom of the request reaching the leader after `requestValidityEnd`, currently
    //      caused by the per-peer model clock vs SUT virtual clock skew (see comment near
    //      `Generator` validity computation).
    //   3. SUT bugs producing wrong Invalid verdicts — what we actually want to catch.
    //
    // Plan:
    //   - Add `Stage4Sut.rejections: Ref[IO, Vector[(RequestId, UserRequestError | L1/L2 err)]]`
    //     populated from a tracer hook in `JointLedger.rejectEvent` (one entry per rejection,
    //     across all peers).
    //   - Add `propNoStaleRejections`: assert no `BlockOutOfRequestValidityInterval` rejections
    //     occurred. Other rejection types are informational only (printed in the analysis
    //     table) — they are the legitimate reordering signal stage4 wants to exercise.
    //   - When `propNoStaleRejections` fails, the message must include the rejection's
    //     timestamps (`blockCreationStartTime`, `requestValidityStart`, `requestValidityEnd`)
    //     and the reqId, so the diagnostic is self-contained without grepping the log.

    /** Property: every submitted request id eventually appears in some block — either as an event
      * (Valid or Invalid) or as a deposit (absorbed or refunded). Catches silent message loss (e.g.
      * the historical `Mempool.extractRequestsWhile` bug).
      */
    private def propLiveness(
        submittedIds: Vector[RequestId],
        canonicalBriefs: Vector[BlockBrief.Intermediate]
    ): Prop = {
        val processedIds: Set[RequestId] =
            canonicalBriefs
                .flatMap(b => b.events.map(_._1) ++ b.depositsAbsorbed ++ b.depositsRefunded)
                .toSet
        val missing = submittedIds.toSet -- processedIds
        Prop(missing.isEmpty) :|
            s"liveness: ${missing.size} submitted reqId(s) never appeared in any block: " +
            s"${missing.toSeq.sortBy(r => (r.peerNum.convert, r.requestNum.convert)).mkString(", ")}"
    }

    /** Property: every absorbed deposit was mature by the time the absorbing block ended, i.e.
      * `brief.endTime >= deposit.absorptionStartTime`. Refund-window check is a TODO — needs
      * `depositAbsorptionEndTime` and the `notInPollResults` legitimate case which we don't track.
      */
    private def propDepositTiming(
        registeredDeposits: Map[RequestId, PendingDeposit],
        canonicalBriefs: Vector[BlockBrief.Intermediate]
    ): Prop = {
        val violations: Vector[String] =
            for
                brief <- canonicalBriefs
                reqId <- brief.depositsAbsorbed.toVector
                deposit <- registeredDeposits.get(reqId).toVector
                if brief.endTime.convert < deposit.absorptionStartTime
            yield s"reqId=$reqId absorbed at brief.endTime=${brief.endTime.convert} but " +
                s"absorptionStartTime=${deposit.absorptionStartTime}"
        Prop(violations.isEmpty) :|
            s"deposit timing: ${violations.size} absorbed-too-early violation(s):\n" +
            violations.mkString("\n")
    }

    /** Property: SUT's valid/total ratio is no greater than the model's, i.e. the SUT is not more
      * permissive than the model. Compared as exact rationals via cross-multiplication.
      *
      * Both sides are restricted to L2-tx reqIds (excluding any deposit reqId — deposits go into
      * `depositsAbsorbed` / `depositsRefunded` on the SUT side, but a rejected deposit registration
      * ends up in `events` via `JointLedger.rejectEvent` and would otherwise inflate the SUT total
      * relative to the model.
      */
    private def propValidRatio(
        lastState: ModelState,
        canonicalBriefs: Vector[BlockBrief.Intermediate]
    ): Prop = {
        val depositIds = lastState.registeredDeposits.keySet
        val l2TxReqIds = lastState.modelFlags.keySet -- depositIds
        val modelValid = l2TxReqIds.count(lastState.modelFlags(_) == ValidityFlag.Valid).toLong
        val modelTotal = l2TxReqIds.size.toLong
        val sutL2Events =
            canonicalBriefs.flatMap(_.events).filterNot { case (r, _) => depositIds.contains(r) }
        val sutValid = sutL2Events.count(_._2 == ValidityFlag.Valid).toLong
        val sutTotal = sutL2Events.size.toLong

        // sutValid/sutTotal <= modelValid/modelTotal  iff  sutValid*modelTotal <= modelValid*sutTotal
        // Trivially holds when either total is 0 (vacuous).
        val holds =
            modelTotal == 0L || sutTotal == 0L ||
                sutValid * modelTotal <= modelValid * sutTotal
        Prop(holds) :|
            s"valid ratio: SUT $sutValid/$sutTotal exceeds model $modelValid/$modelTotal " +
            "(SUT is more permissive than the model)"
    }

    /** Property: the slow cycle hard-confirmed at least one stack, and every block in the frozen
      * `slowCoverageTarget` lies in some hard-confirmed stack on the canonical peer. Using the
      * frozen target — the contract the shutdown waited on — rather than a fresh brief re-read
      * avoids drift if the leader produces another block between the signal and the snapshot.
      */
    private def propStackCoverage(
        targetBlockNums: Set[Int],
        canonicalStacks: Vector[Stack.HardConfirmed]
    ): Prop = {
        // A stack covers the inclusive block range [firstBlockNum, lastBlockNum] from its
        // brief. The Initial stack carries a synthetic zero-range brief ([0..0]), so it
        // contributes no real block coverage.
        val coveredRanges: Vector[(Int, Int)] =
            canonicalStacks.map { s =>
                val b = s.brief
                ((b.firstBlockNum: Int), (b.lastBlockNum: Int))
            }
        def covered(n: Int): Boolean =
            coveredRanges.exists { case (lo, hi) => lo <= n && n <= hi }
        val uncovered = targetBlockNums.filterNot(covered)
        (Prop(canonicalStacks.nonEmpty) :|
            "stack coverage: slow cycle hard-confirmed no stacks") &&
        (Prop(uncovered.isEmpty) :|
            s"stack coverage: ${uncovered.size} observed block(s) never in any " +
            s"hard-confirmed stack: ${uncovered.toSeq.sorted.mkString(", ")}")
    }

    private def traceBlockTable(
        canonicalBriefs: Vector[BlockBrief.Intermediate],
        sortedPeers: Seq[HeadPeerNumber],
        briefsByPeer: Map[HeadPeerNumber, Vector[BlockBrief.Intermediate]],
        nPeers: Int,
        submittedIds: Vector[RequestId],
        lastState: ModelState,
    ): IO[Unit] = {
        val colWidth = 72
        val divider = s"+${"-" * (colWidth + 2)}+"
        val header = s"| ${"Block".padTo(colWidth, ' ')} |"

        val rows = canonicalBriefs.map { brief =>
            val blockType = brief match {
                case _: BlockBrief.Minor => "Min"; case _: BlockBrief.Major => "Maj"
            }
            val vMaj = brief.blockVersion.major.convert
            val vMin = brief.blockVersion.minor.convert
            val leader = (brief.blockNum: Int) % nPeers
            val evs = brief.events.map { case (reqId, flag) =>
                val f = if flag == ValidityFlag.Valid then "V" else "I"
                s"p${reqId.peerNum.convert}:r${reqId.requestNum.convert}=$f"
            }
            val abs = brief.depositsAbsorbed.map(r =>
                s"abs:p${r.peerNum.convert}:r${r.requestNum.convert}"
            )
            val ref = brief.depositsRefunded.map(r =>
                s"ref:p${r.peerNum.convert}:r${r.requestNum.convert}"
            )
            val events = (evs ++ abs ++ ref).mkString(" ")
            val label =
                s"#${brief.blockNum: Int} $blockType v$vMaj.$vMin lead=p$leader | $events"
            s"| ${label.take(colWidth).padTo(colWidth, ' ')} |"
        }

        // SUT processing order — each block contributes its absorbed deposits, then its events.
        val sutOrder: Vector[RequestId] =
            canonicalBriefs.flatMap(b => b.depositsAbsorbed ++ b.events.map(_._1)).toVector
        val commonPrefixLen =
            submittedIds.zip(sutOrder).takeWhile { case (a, b) => a == b }.length

        val depositIds = lastState.registeredDeposits.keySet
        val l2TxReqIds = lastState.modelFlags.keySet -- depositIds
        val modelValid = l2TxReqIds.count(lastState.modelFlags(_) == ValidityFlag.Valid)
        val modelTotal = l2TxReqIds.size
        val sutL2Events =
            canonicalBriefs.flatMap(_.events).filterNot { case (r, _) => depositIds.contains(r) }
        val sutValid = sutL2Events.count(_._2 == ValidityFlag.Valid)
        val sutTotal = sutL2Events.size

        val peersLine =
            s"Peers: ${sortedPeers.map(p => s"p${p: Int}=${briefsByPeer(p).length}blks").mkString("  ")}"
        val prefixLine =
            s"Common prefix: $commonPrefixLen / ${submittedIds.length} (submission order vs SUT block order)"
        val ratioLine =
            s"Valid/total (L2 txs) — model: $modelValid/$modelTotal  SUT: $sutValid/$sutTotal"
        val legend =
            "Legend: Min=Minor Maj=Major v=version lead=leader p=peer r=requestNum V=valid I=invalid abs=deposit-absorbed ref=refunded"

        val text = (divider :: header :: divider :: rows.toList ++
            (divider :: peersLine :: prefixLine :: ratioLine :: legend :: Nil))
            .mkString("\n", "\n", "")
        log.info(text)
    }

    /** Mirror of [[traceBlockTable]] for the slow cycle: one row per hard-confirmed stack on the
      * canonical peer, showing the stack number, covered block range, partition spine (Min/Maj/Fin
      * kinds, in stack order), and the round-2 unlock selection (settlement-at-i,
      * finalization-at-i, or sole-acknowledgment / no unlock). Stack-0 renders as `Init`. Followed
      * by a per-peer stack-count line for cross-peer convergence at a glance.
      */
    private def traceStackTable(
        canonicalStacks: Vector[Stack.HardConfirmed],
        sortedPeers: Seq[HeadPeerNumber],
        stacksByPeer: Map[HeadPeerNumber, Vector[Stack.HardConfirmed]],
        nPeers: Int,
    ): IO[Unit] = {
        val colWidth = 72
        val divider = s"+${"-" * (colWidth + 2)}+"
        val header = s"| ${"Stack".padTo(colWidth, ' ')} |"

        val rows = canonicalStacks.map { stack =>
            val brief = stack.brief
            val sNum = brief.stackNum: Int
            val first = brief.firstBlockNum: Int
            val last = brief.lastBlockNum: Int
            val nBlks = last - first + 1
            // Slow-consensus leadership schedule is round-robin by stack number, mirroring
            // fast-consensus block-number round-robin.
            val leader = sNum % nPeers
            val label = stack.effects match {
                case _: StackEffects.HardConfirmed.Initial =>
                    s"#$sNum Init lead=p$leader | init+fallback"
                case r: StackEffects.HardConfirmed.Regular =>
                    val blkLabel = if nBlks == 1 then "blk" else "blks"
                    val parts = r.partitions.toList.map {
                        case _: PartitionEffects.Minor[?] => "Min"
                        case _: PartitionEffects.Major[?] => "Maj"
                        case _: PartitionEffects.Final    => "Fin"
                    }
                    val unlockStr = PartitionEffects.unlock(r.partitions) match {
                        case Some(PartitionEffects.Unlock.Settlement(i))   => s"sttlmnt@$i"
                        case Some(PartitionEffects.Unlock.Finalization(i)) => s"fin@$i"
                        case None                                          => "sole"
                    }
                    s"#$sNum [$first..$last] ($nBlks $blkLabel) Reg lead=p$leader | " +
                        s"[${parts.mkString(",")}] u=$unlockStr"
            }
            s"| ${label.take(colWidth).padTo(colWidth, ' ')} |"
        }

        val peersLine =
            s"Peers: ${sortedPeers.map(p => s"p${p: Int}=${stacksByPeer(p).length}stk").mkString("  ")}"
        val legend =
            "Legend: Init=initial stack Reg=regular Min/Maj/Fin=partition kinds " +
                "u=unlock (set=settlement, fin=finalization, sole=no unlock) @i=partition index"

        val text = (divider :: header :: divider :: rows.toList ++
            (divider :: peersLine :: legend :: Nil))
            .mkString("\n", "\n", "")
        log.info(text)
    }

// ===================================
// Initial state generator (canonical location; Runner delegates here for @main)
// ===================================

object Stage4Suite:

    def genInitialState(
        nPeers: Int = 2,
        nCoilPeers: Int = 0,
        absorptionSlack: FiniteDuration = 60.seconds,
        meanInterArrivalTime: HeadPeerNumber => FiniteDuration = _ => 12.seconds,
        useTestControl: Boolean = true,
    ): Gen[ModelState] =
        val cardanoNetwork = CardanoNetwork.Preprod
        val testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nPeers)
        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)

        // Coil wallets are extra keys from the same seed, beyond the head set; each coil peer is hubbed
        // by head 0. Empty for a pure-head run. Their vkeys go into the head bootstrap so the
        // threshold script requires `coilQuorum` of them, and `mkCoilConfig` (below) derives each
        // coil's own node config from the shared head config.
        val coilWallets: List[PeerWallet] =
            if nCoilPeers == 0 then Nil
            else {
                val withCoils =
                    TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nPeers + nCoilPeers)
                (0 until nCoilPeers).toList.map(i =>
                    withCoils.walletFor(HeadPeerNumber(nPeers + i))
                )
            }
        val coilPeers: CoilPeers =
            CoilPeers.indexed(
              coilWallets.map(w => CoilPeerData(w.exportVerificationKey, HeadPeerNumber(0)))
            )

        // For non-TestControl runs we need the head's initial block end-time anchored at a
        // small wall-clock offset in the future, so `sutResource` can sleep until that anchor
        // and have the model clock and the wall clock coincide at command 1. 60s matches
        // stage 1's budget; if 20-peer setup overruns it the test aborts (see sutResource).
        // Under TestControl we keep the deterministic Jan-1-2026 + 100-day random distribution
        // — `Instant.now()` would defeat seed-based reproducibility.
        val takeoffTime: Option[java.time.Instant] =
            if useTestControl then None
            else Some(java.time.Instant.now().plusSeconds(60))

        val generateHeadStartTime = ReaderT((tp: TestPeers) =>
            takeoffTime match {
                case Some(t) =>
                    Gen.const(BlockCreationEndTime(t.quantize(tp.slotConfig)))
                case None =>
                    // Date and time (GMT): Thursday, January 1, 2026 at 12:00:00 AM, POSIX seconds
                    val anchorTime = 1767225600L
                    // 100 day range, seconds
                    val range = 86_400 * 100L
                    for offset <- Gen.choose(0L, range)
                    yield BlockCreationEndTime(
                      java.time.Instant.ofEpochSecond(anchorTime + offset).quantize(tp.slotConfig)
                    )
            }
        )

        val generateHeadConfigBootstrap_ = generateHeadConfigBootstrap(
          generateHeadParams = generateHeadParameters(generateTxTiming = generateYaciTxTiming)
              .map(_.copy(coilQuorum = nCoilPeers)),
          generateInitializationParameters = InitParamsType.TopDown(
            InitializationParametersGenTopDown.GenWithDeps(
              generateGenesisUtxosL1 = ReaderT((tp: TestPeers) =>
                  Gen.const(testPeerToUtxos.map((k, v) => k.headPeerNumber -> v))
              )
            )
          ),
          coilPeers = coilPeers
        )

        val generateHeadConfig_ = generateHeadConfig(
          genHeadConfigBootstrap = generateHeadConfigBootstrap_,
          generateInitialBlock = bootstrap =>
              generateInitialBlock(
                genHeadConfigBootstrap = ReaderT
                    .pure[Gen, TestPeers, hydrozoa.config.head.HeadConfig.Bootstrap](bootstrap),
                generateBlockCreationEndTime = generateHeadStartTime
              )
        )

        for
            config <- MultiNodeConfig.generateWith(testPeers)(
              generateHeadConfig = generateHeadConfig_,
              // Halve the maximum allowed polling period (the default is
              // `headConfig.maxCardanoLiaisonPollingPeriod`). With a lower upper bound,
              // every peer's CardanoLiaison polls L1 more frequently — peers see new
              // deposits closer in time, which should reduce inter-peer skew that may
              // currently produce mismatched block briefs at major-block consensus.
              generateNodeOperationMultisigConfig = hc =>
                  hydrozoa.config.node.operation.multisig.generateNodeOperationMultisigConfig(
                    maxPollingPeriod = hc.maxCardanoLiaisonPollingPeriod / 2,
                    // Keep `softBlockMinPeriod` at the production default so block-production
                    // cadence (and the model-command-batching that depends on it) is unchanged.
                    // Only narrow `hardStackMinPeriod`: the 3-minute production value is what
                    // starves CardanoLiaison of PushResults inside the WS test wall-clock budget
                    // (see docs/rate-limiter.md). 2s preserves the throttle's batching intent
                    // across blocks while fitting the WS budget.
                    rateLimits = hydrozoa.config.node.operation.multisig.RateLimits(
                      softBlockMinPeriod =
                          hydrozoa.config.node.operation.multisig.RateLimits.default.softBlockMinPeriod,
                      hardStackMinPeriod = 2.seconds
                    )
                  )
            )

            preinitPeerUtxosL1 = testPeerToUtxos.map((k, v) => k.headPeerNumber -> v)

            // Each coil's own node config: the shared head config plus the coil identity seam. The
            // coil is a read-only follower, so it reuses head 0's operational sub-configs (polling
            // period etc.); none of head 0's wallet-derived fields are exercised on the coil path.
            head0Private = config.nodePrivateConfigs(HeadPeerNumber(0))
            coilNodeConfigs = coilWallets.map { w =>
                NodeConfig
                    .mkCoilConfig(
                      headConfig = config.headConfig,
                      ownCoilWallet = w,
                      nodeOperationEvacuationConfig = head0Private.nodeOperationEvacuationConfig,
                      nodeOperationMultisigConfig = head0Private.nodeOperationMultisigConfig,
                      blockfrostApiKey = "not-a-real-key",
                      sugarRushUri = "ws://localhost:3001/ws",
                      adminUsername = "admin",
                      adminPassword = "welcome",
                      httpHost = "0.0.0.0",
                      httpPort = "8080",
                    )
                    .get
            }

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

            startTime = config.headConfig.initialBlock.blockBrief.endTime.convert
        yield ModelState(
          params = Params(
            config,
            absorptionSlack,
            peers.map(pn => pn -> meanInterArrivalTime(pn)).toMap,
            coilNodeConfigs = coilNodeConfigs
          ),
          preinitPeerUtxosL1 = preinitPeerUtxosL1,
          currentModelTime = startTime,
          takeoffTime = takeoffTime,
          utxosL2Active = config.headConfig.initializationParameters.initialEvacuationMap.toUtxos,
          peerUtxosL1 = peerUtxosL1,
          nextRequestNumbers = peers.map(_ -> RequestNumber(0)).toMap,
          pendingDeposits = peers.map(_ -> Nil).toMap,
          modelFlags = Map.empty,
          registeredDeposits = Map.empty,
        )
