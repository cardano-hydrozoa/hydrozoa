package hydrozoa.integration.stage4

import cats.data.ReaderT
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.implicits.*
import com.suprnation.actor.ActorSystem
import com.suprnation.actor.event.Error as ActorError
import hydrozoa.config.head.initialization.{InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.generateYaciTxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.coil.{CoilPeerData, CoilPeers}
import hydrozoa.config.head.{InitParamsType, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.integration.stage4.Model.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.lib.logging.{Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info, warn}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.HeadMultisigRegimeManager
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendMock, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber, PeerId, PeerWallet}
import hydrozoa.multisig.consensus.transport.{HubWsTransport, CoilPeerWsTransport, NodeWsServer, PeerTransport, NodeWsServerEventFormat, RemoteCoilProxy, RemoteHubProxy, RemotePeerProxy, PeerTransportEventFormat}
import hydrozoa.multisig.consensus.limiter.{Limiter, LimiterEvent, LimiterEventFormat}
import hydrozoa.multisig.consensus.{BlockWeaver, BlockWeaverEvent, BlockWeaverEventFormat, CardanoLiaison, CardanoLiaisonEvent, CardanoLiaisonEventFormat, CoilAckSequencer, CoilRelay, EventSequencerEvent, EventSequencerEventFormat, FastConsensusActor, FastConsensusActorEvent, FastConsensusActorEventFormat, RequestSequencer, SlowConsensusActor, SlowConsensusActorEvent, SlowConsensusActorEventFormat, StackComposer, StackComposerEvent, StackComposerEventFormat}
import hydrozoa.multisig.consensus.CoilAckSequencerEventFormat
import hydrozoa.multisig.consensus.transport.{CoilPeerWsTransportEventFormat, HubWsTransportEventFormat}
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonCoilToHub, PeerLiaisonEvent, PeerLiaisonEventFormat, PeerLiaisonHeadToHead, PeerLiaisonHubToCoil}
import org.http4s.Uri
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.websocket.WebSocketBuilder2
import com.comcast.ip4s.{Host, Port, host}
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.eutxol2.{EutxoL2Ledger, toUtxos}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.ledger.joint.{JointLedger, JointLedgerEvent, JointLedgerEventFormat}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects}
import hydrozoa.multisig.persistence.{BackendStore, Cf, InMemoryBackendStore, Persistence, PersistenceEvent, PersistenceEventFormat}
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import org.scalacheck.commands.{AnyCommand, ModelBasedSuite, ScenarioGen}
import org.scalacheck.{Gen, Prop, PropertyM}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.{CertState, TransactionInput, Utxos}
import test.{SeedPhrase, TestPeers, given}

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}

// ===================================
// Stage 4 suite
// ===================================

/** All actors + observation refs for one coil peer follower, assembled in `sutResource` (gated on
  * `nCoilPeers > 0`). `headLiaison` is the hub-side `PeerLiaisonHubToCoil`; `coilLiaison` is the
  * coil-side `PeerLiaisonCoilToHub`. `stacksRef` collects the hard-confirmed stacks captured by the
  * follower's SlowConsensusActor tracer sink.
  */
private case class CoilWiring(
    coilNum: CoilPeerNumber,
    config: NodeConfig,
    pending: Deferred[IO, HeadMultisigRegimeManager.Connections],
    stack: PeerStack,
    stacksRef: Ref[IO, Vector[Stack.HardConfirmed]],
    coilLiaison: PeerLiaisonCoilToHub.Handle,
    headLiaison: PeerLiaisonHubToCoil.Handle,
)

/** The WS network for one SUT: the head-mesh remote handles per peer, plus (when coils run over WS)
  * the coil transports and the proxy handles that stand in for the remote hub/coil liaisons. In
  * Direct mode the coil fields are empty and the coil links are wired in-process.
  *
  *   - `remoteHeadByPeer(A)(B)` is the handle A's mesh liaison toward B sends through.
  *   - `remoteCoilProxies(c)` is the hub's handle toward coil `c` (a `RemoteCoilProxy`).
  *   - `remoteHubProxies(c)` is coil `c`'s handle toward the hub (a `RemoteHubProxy`).
  */
private case class WsNetwork(
    remoteHeadByPeer: Map[HeadPeerNumber, Map[HeadPeerNumber, PeerLiaisonHeadToHead.Handle]],
    coilHubTransport: Option[HubWsTransport],
    coilUplinks: Map[CoilPeerNumber, CoilPeerWsTransport],
    remoteCoilProxies: Map[CoilPeerNumber, LiaisonProtocol.CoilToHubHandle],
    remoteHubProxies: Map[CoilPeerNumber, LiaisonProtocol.HubToCoilHandle],
)

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
    submittedRequestIds: Ref[IO, Vector[RequestId]],
    fastSettlementSignal: Deferred[IO, Unit],
    slowCoverageSignal: Deferred[IO, Unit],
    fastSettlementTarget: Deferred[IO, Set[RequestId]],
    slowCoverageTarget: Deferred[IO, Set[Int]],
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
        case TransportMode.Direct       => true
        case _: TransportMode.WebSocket => false
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

        // Every coil is hubbed by head 0 in stage4.
        val hubNum = HeadPeerNumber(0)
        val wsMode = transportMode match {
            case _: TransportMode.WebSocket => true
            case TransportMode.Direct       => false
        }

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
                                s"Stage4 sutResource: initialization took too long " +
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
                submittedRequestIds   <- Ref[IO].of(Vector.empty[RequestId])
                fastSettlementSignal <- IO.deferred[Unit]
                slowCoverageSignal   <- IO.deferred[Unit]
                fastSettlementTarget <- IO.deferred[Set[RequestId]]
                slowCoverageTarget   <- IO.deferred[Set[Int]]
            } yield PostSystemState(
              cardanoBackend = cardanoBackend,
              pendingConnsMap = pendingConnsMap,
              blockBriefsMap = blockBriefsMap,
              stacksMap = stacksMap,
              submittedRequestIds = submittedRequestIds,
              fastSettlementSignal = fastSettlementSignal,
              slowCoverageSignal = slowCoverageSignal,
              fastSettlementTarget = fastSettlementTarget,
              slowCoverageTarget = slowCoverageTarget,
            )
        }

        // ------ Per-peer actor stack. Brackets `openPeerBackend` (RocksDB on disk); the rest
        // ------ is plain IO around it.
        def buildPeerStack(
            peerNum: HeadPeerNumber,
            system: ActorSystem[IO],
            cardanoBackend: CardanoBackend[IO],
            pendingConnsMap: Map[HeadPeerNumber, Deferred[IO, HeadMultisigRegimeManager.Connections]],
            blockBriefsMap: Map[HeadPeerNumber, Ref[IO, Vector[BlockBrief.Intermediate]]],
            stacksMap: Map[HeadPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
            submittedRequestIds: Ref[IO, Vector[RequestId]],
            fastSettlementSignal: Deferred[IO, Unit],
            slowCoverageSignal: Deferred[IO, Unit],
            fastSettlementTarget: Deferred[IO, Set[RequestId]],
            slowCoverageTarget: Deferred[IO, Set[Int]],
        ): Resource[IO, PeerStack] = {
            val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
            val pending = pendingConnsMap(peerNum)
            val bwTracer: ContraTracer[IO, BlockWeaverEvent] =
                Slf4jTracer.sink.contramap(BlockWeaverEventFormat.humanFormat(peerNum))
            val fcaTracer: ContraTracer[IO, FastConsensusActorEvent] =
                Slf4jTracer.sink.contramap(FastConsensusActorEventFormat.humanFormat(peerNum))
            val clTracer: ContraTracer[IO, CardanoLiaisonEvent] =
                Slf4jTracer.sink.contramap(CardanoLiaisonEventFormat.humanFormat(peerNum))
            val scTracer: ContraTracer[IO, StackComposerEvent] =
                Slf4jTracer.sink.contramap(StackComposerEventFormat.humanFormat(peerNum))


            val captureScaSink: ContraTracer[IO, SlowConsensusActorEvent] =
                ContraTracer.emit[IO, SlowConsensusActorEvent] {
                    case SlowConsensusActorEvent.StackHardConfirmed(stack) =>
                        for
                            _           <- stacksMap(peerNum).update(_ :+ stack)
                            maybeTarget <- slowCoverageTarget.tryGet
                            _ <- maybeTarget match
                                     case None => IO.unit
                                     case Some(targetNums) =>
                                         for
                                             // Check ALL peers so the signal only fires once every
                                             // peer's stacksMap is current — ensuring analyzePersistence
                                             // sees consistent captured stacks across all peers.
                                             allPeersStacks <- stacksMap.values.toList.traverse(_.get)
                                             allCovered = targetNums.isEmpty ||
                                                 allPeersStacks.forall { peerStacks =>
                                                     targetNums.forall { bn =>
                                                         peerStacks.exists { s =>
                                                             (s.brief.firstBlockNum: Int) <= bn &&
                                                             bn <= (s.brief.lastBlockNum: Int)
                                                         }
                                                     }
                                                 }
                                             _ <- if allCovered then slowCoverageSignal.complete(()).void
                                                  else IO.unit
                                         yield ()
                        yield ()
                    case _ => IO.unit
                }
            val scaTracer: ContraTracer[IO, SlowConsensusActorEvent] =
                captureScaSink
                    |+| Slf4jTracer.sink.contramap(
                      SlowConsensusActorEventFormat.humanFormat(peerNum)
                    )
            // Capture sink: accumulates briefs and fires fastSettlementSignal once all IDs in
            // the target set (populated by beforeFinalize) appear across the collected briefs.
            // The target guard prevents mid-run firing against a partial submittedRequestIds.
            val captureSink: ContraTracer[IO, JointLedgerEvent] =
                ContraTracer.emit[IO, JointLedgerEvent] {
                    case JointLedgerEvent.BriefProduced(b) =>
                        for
                            _           <- blockBriefsMap(peerNum).update(_ :+ b)
                            maybeTarget <- fastSettlementTarget.tryGet
                            _ <- maybeTarget match
                                     case None => IO.unit
                                     case Some(submitted) =>
                                         for
                                             briefs <- blockBriefsMap(peerNum).get
                                             seen    = briefs
                                                           .flatMap(br =>
                                                               br.events.map(_._1) ++
                                                               br.depositsAbsorbed ++
                                                               br.depositsRefunded
                                                           )
                                                           .toSet
                                             _ <- if submitted.forall(seen.contains)
                                                  then fastSettlementSignal.complete(()).void
                                                  else IO.unit
                                         yield ()
                        yield ()
                    case _ => IO.unit
                }
            // SLF4J sink: all JL events get a human-readable line so test runs are debuggable.
            val textSink: ContraTracer[IO, JointLedgerEvent] =
                Slf4jTracer.sink.contramap(JointLedgerEventFormat.humanFormat(peerNum))
            val jlTracer: ContraTracer[IO, JointLedgerEvent] = captureSink |+| textSink
            val esTracer: ContraTracer[IO, EventSequencerEvent] =
                Slf4jTracer.sink.contramap(EventSequencerEventFormat.humanFormat(peerNum))
            // Per-peer persistence backend — InMemory by default; RocksDb when the
            // suite is constructed with `BackendMode.RocksDb(root)`. Built first so
            // every producer (RequestSequencer/JL/FCA/SC/SCA/PeerLiaison) shares the
            // one per-peer store; `analyzePersistence` reads it back.
            val persistenceTracer: ContraTracer[IO, PersistenceEvent] =
                Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
            openPeerBackend(
              peerNum,
              Cf.mkAll(
                headPeers = multiNodeConfig.headConfig.headPeerNums.toList,
                coilPeers = multiNodeConfig.headConfig.coilPeers.coilPeerNumbers,
                hubs = multiNodeConfig.headConfig.coilPeers.hubHeadPeerNumbers
              ),
              persistenceTracer
            ).evalMap { backendStore =>
                for
                    persistence <- {
                        given CardanoNetwork.Section = nodeConfig
                        Persistence.fromBackend(backendStore, persistenceTracer)
                    }
                    blockWeaver <- system.actorOf(BlockWeaver(nodeConfig, pending, bwTracer))
                    cardanoLiaison <- system.actorOf(
                      CardanoLiaison(nodeConfig, cardanoBackend, pending, clTracer, persistence)
                    )
                    requestSequencer <- system.actorOf(
                      RequestSequencer(nodeConfig, pending, esTracer, persistence)
                    )
                    l2Ledger <- EutxoL2Ledger(nodeConfig)
                    jointLedger <- system.actorOf(
                      JointLedger(nodeConfig, pending, l2Ledger, jlTracer, persistence)
                    )
                    consensusActor <- system.actorOf(
                      FastConsensusActor(nodeConfig, pending, fcaTracer, persistence)
                    )
                    stackComposer <- system.actorOf(
                      StackComposer(nodeConfig, pending, scTracer, persistence)
                    )
                    slowConsensusActor <- system.actorOf(
                      SlowConsensusActor(nodeConfig, pending, scaTracer, persistence)
                    )
                yield PeerStack(
                  blockWeaver,
                  cardanoLiaison,
                  Some(requestSequencer),
                  jointLedger,
                  consensusActor,
                  stackComposer,
                  slowConsensusActor,
                  backendStore,
                  persistence
                )
            }
        }

        // ------ Post-stack, pre-transport IO: local peer liaisons. ------
        def postStack(
            system: ActorSystem[IO],
            peerStackMap: Map[HeadPeerNumber, PeerStack],
            pendingConnsMap: Map[HeadPeerNumber, Deferred[IO, HeadMultisigRegimeManager.Connections]],
        ): IO[Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaisonHeadToHead.Handle]]] = {
            for {
                // Create one local PeerLiaisonHeadToHead per (local, remote) pair.
                // peerLiaisonMap(A)(B.id) = the liaison at A directed to B.
                peerLiaisonMap <- peers
                    .traverse { peerNum =>
                        val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
                        val pending = pendingConnsMap(peerNum)
                        // Reuse this peer's persistence (same store + arrival-stamp generation) for
                        // its PeerLiaisons' CR8 inbound writes — do NOT build a second one (that
                        // would double-bump the generation).
                        val persistence = peerStackMap(peerNum).persistence
                        peers
                            .filterNot(_ == peerNum)
                            .traverse { remotePeerNum =>
                                val remotePeerId = headPeerId(multiNodeConfig, remotePeerNum)
                                val plTracer: ContraTracer[IO, PeerLiaisonEvent] =
                                    Slf4jTracer.sink.contramap(
                                      PeerLiaisonEventFormat
                                          .humanFormat(
                                            PeerId.Head(peerNum),
                                            PeerId.Head(remotePeerNum)
                                          )
                                    )
                                system
                                    .actorOf(
                                      PeerLiaisonHeadToHead(
                                        nodeConfig,
                                        remotePeerId,
                                        pending,
                                        plTracer,
                                        persistence
                                      )
                                    )
                                    .map(remotePeerId -> _)
                            }
                            .map(liaisons => peerNum -> liaisons.toMap)
                    }
                    .map(_.toMap)
            } yield peerLiaisonMap
        }

        // ------ Build the per-peer remote-liaison map. In Direct mode this is the actual
        // ------ PeerLiaisonHeadToHead handle from the other peer's actor stack. In WebSocket
        // ------ mode it's a RemotePeerProxy that forwards messages over the local
        // ------ PeerTransport. ws.remoteHeadByPeer(A)(B) = the handle that A's
        // ------ PeerLiaisonHeadToHead(A->B) uses to reach B's PeerLiaisonHeadToHead(B->A).
        // ------ WS transports (head mesh and coil links) are released when the Resource is
        // ------ finalized.
        def transportSetup(
            system: ActorSystem[IO],
            peerLiaisonMap: Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaisonHeadToHead.Handle]],
        ): Resource[IO, WsNetwork] =
            transportMode match {
                case TransportMode.Direct =>
                    val remoteHeadByPeer
                        : Map[HeadPeerNumber, Map[HeadPeerNumber, PeerLiaisonHeadToHead.Handle]] =
                        peers.map { peerNum =>
                            val ownPeerId = headPeerId(multiNodeConfig, peerNum)
                            peerNum -> peers
                                .filterNot(_ == peerNum)
                                .map { remotePeerNum =>
                                    remotePeerNum -> peerLiaisonMap(remotePeerNum)(ownPeerId)
                                }
                                .toMap
                        }.toMap
                    Resource.pure(
                      WsNetwork(remoteHeadByPeer, None, Map.empty, Map.empty, Map.empty)
                    )
                case TransportMode.WebSocket(basePort) =>
                    given CardanoNetwork.Section = multiNodeConfig.headConfig
                    setupWebSocketNetwork(
                      multiNodeConfig,
                      peers,
                      basePort,
                      system,
                      peerLiaisonMap,
                      coilConfigs,
                      hubNum,
                    )
            }

        // ------ Coil peer followers (gated; empty for a pure-head run). Each coil peer is
        // ------ hubbed by head 0: build its full follower actor stack + its single
        // ------ PeerLiaisonCoilToHub, plus the hub-side PeerLiaisonHubToCoil and (once) the
        // ------ CoilAckSequencer + CoilRelay. In WS mode the coil links run over the shared
        // ------ per-peer server (registered here); in Direct mode they are wired in-process.
        def buildCoilWirings(
            system: ActorSystem[IO],
            cardanoBackend: CardanoBackend[IO],
            peerStackMap: Map[HeadPeerNumber, PeerStack],
            pendingConnsMap: Map[HeadPeerNumber, Deferred[IO, HeadMultisigRegimeManager.Connections]],
            ws: WsNetwork,
        ): Resource[IO, (Option[CoilAckSequencer.Handle], Option[CoilRelay.Handle], List[CoilWiring])] = {
            val hubConfig = multiNodeConfig.nodeConfigs(hubNum)
            val hubPending = pendingConnsMap(hubNum)
            for {
                coilHandles <- Resource.eval(for {
                    coilAckSequencer <-
                        if coilConfigs.isEmpty then IO.none[CoilAckSequencer.Handle]
                        else
                            val casTracer = Slf4jTracer.sink
                                .contramap(CoilAckSequencerEventFormat.humanFormat(hubNum))
                            system
                                .actorOf(
                                  CoilAckSequencer(
                                    hubConfig,
                                    peerStackMap(hubNum).persistence,
                                    hubPending,
                                    casTracer
                                  )
                                )
                                .map(Some(_))
                    coilRelay <-
                        if coilConfigs.isEmpty then IO.none[CoilRelay.Handle]
                        else system.actorOf(CoilRelay(hubPending)).map(Some(_))
                } yield (coilAckSequencer, coilRelay))
                (coilAckSequencer, coilRelay) = coilHandles

                coilWirings <- coilConfigs.traverse { coilConfig =>
                    val coilNum = coilConfig.ownPeerId match {
                        case PeerId.Coil(n) => n
                        case PeerId.Head(_) =>
                            throw new IllegalStateException(
                              "coil node config carries a head peer id"
                            )
                    }
                    // The format renderers label events by peer number; coil peer i logs as
                    // nPeers + i, matching its test-wallet index in `genInitialState`.
                    val labelNum = HeadPeerNumber(nPeers + coilNum.convert)
                    val bwTracer: ContraTracer[IO, BlockWeaverEvent] =
                        Slf4jTracer.sink.contramap(BlockWeaverEventFormat.humanFormat(labelNum))
                    val clTracer: ContraTracer[IO, CardanoLiaisonEvent] =
                        Slf4jTracer.sink.contramap(CardanoLiaisonEventFormat.humanFormat(labelNum))
                    val jlTracer: ContraTracer[IO, JointLedgerEvent] =
                        Slf4jTracer.sink.contramap(JointLedgerEventFormat.humanFormat(labelNum))
                    val fcaTracer: ContraTracer[IO, FastConsensusActorEvent] =
                        Slf4jTracer.sink.contramap(
                          FastConsensusActorEventFormat.humanFormat(labelNum)
                        )
                    val scTracer: ContraTracer[IO, StackComposerEvent] =
                        Slf4jTracer.sink.contramap(StackComposerEventFormat.humanFormat(labelNum))
                    val coilPlTracer: ContraTracer[IO, PeerLiaisonEvent] =
                        Slf4jTracer.sink.contramap(
                          PeerLiaisonEventFormat
                              .humanFormat(PeerId.Coil(coilNum), PeerId.Head(hubNum))
                        )
                    val hubPlTracer: ContraTracer[IO, PeerLiaisonEvent] =
                        Slf4jTracer.sink.contramap(
                          PeerLiaisonEventFormat
                              .humanFormat(PeerId.Head(hubNum), PeerId.Coil(coilNum))
                        )
                    val coilPersistenceTracer = Slf4jTracer.sink
                        .contramap(PersistenceEventFormat.humanFormat)
                    // The coil peer gets its own per-peer store (in-memory for the test).
                    InMemoryBackendStore.open(coilPersistenceTracer).flatMap { coilBackendStore =>
                        Resource.eval(for {
                            coilPending <- Deferred[IO, HeadMultisigRegimeManager.Connections]
                            coilPersistence <- {
                                given CardanoNetwork.Section = coilConfig
                                Persistence.fromBackend(coilBackendStore, coilPersistenceTracer)
                            }
                            // Capture sink mirroring `buildPeerStack`'s SCA sink: the follower's
                            // hard-confirmed stacks land in the per-coil Ref consumed by
                            // `propCoilParticipation`.
                            stacksRef <- Ref[IO].of(Vector.empty[Stack.HardConfirmed])
                            captureScaSink = ContraTracer.emit[IO, SlowConsensusActorEvent] {
                                case SlowConsensusActorEvent.StackHardConfirmed(stack) =>
                                    stacksRef.update(_ :+ stack)
                                case _ => IO.unit
                            }
                            scaTracer = captureScaSink |+| Slf4jTracer.sink.contramap(
                              SlowConsensusActorEventFormat.humanFormat(labelNum)
                            )
                            blockWeaver <- system.actorOf(
                              BlockWeaver(coilConfig, coilPending, bwTracer)
                            )
                            cardanoLiaison <- system.actorOf(
                              CardanoLiaison(
                                coilConfig,
                                cardanoBackend,
                                coilPending,
                                clTracer,
                                coilPersistence
                              )
                            )
                            l2Ledger <- EutxoL2Ledger(coilConfig)
                            jointLedger <- system.actorOf(
                              JointLedger(
                                coilConfig,
                                coilPending,
                                l2Ledger,
                                jlTracer,
                                coilPersistence
                              )
                            )
                            consensusActor <- system.actorOf(
                              FastConsensusActor(
                                coilConfig,
                                coilPending,
                                fcaTracer,
                                coilPersistence
                              )
                            )
                            stackComposer <- system.actorOf(
                              StackComposer(coilConfig, coilPending, scTracer, coilPersistence)
                            )
                            slowConsensusActor <- system.actorOf(
                              SlowConsensusActor(
                                coilConfig,
                                coilPending,
                                scaTracer,
                                coilPersistence
                              )
                            )
                            coilLiaison <- system.actorOf(
                              PeerLiaisonCoilToHub(
                                coilConfig,
                                coilPending,
                                coilPlTracer,
                                coilPersistence
                              )
                            )
                            headLiaison <- system.actorOf(
                              PeerLiaisonHubToCoil(
                                hubConfig,
                                coilNum,
                                hubPending,
                                hubPlTracer,
                                peerStackMap(hubNum).persistence
                              )
                            )
                        } yield CoilWiring(
                          coilNum = coilNum,
                          config = coilConfig,
                          pending = coilPending,
                          stack = PeerStack(
                            blockWeaver,
                            cardanoLiaison,
                            None,
                            jointLedger,
                            consensusActor,
                            stackComposer,
                            slowConsensusActor,
                            coilBackendStore,
                            coilPersistence
                          ),
                          stacksRef = stacksRef,
                          coilLiaison = coilLiaison,
                          headLiaison = headLiaison
                        ))
                    }
                }

                // In WS mode, register each coil peer's now-spawned liaisons as the inbound
                // dispatch targets on the coil transports (the hub→coil server and the coil's
                // uplink dialer).
                _ <- Resource.eval(IO.whenA(wsMode)(coilWirings.traverse_ { c =>
                    ws.coilHubTransport.traverse_(_.register(c.coilNum, c.headLiaison)) >>
                        ws.coilUplinks.get(c.coilNum).traverse_(_.register(c.coilLiaison))
                }))
            } yield (coilAckSequencer, coilRelay, coilWirings)
        }

        // ------ Post-transport IO: wire each peer's Connections (head and coil), start the
        // ------ error drainer and per-peer CardanoLiaison-tick fibers, then assemble the
        // ------ Stage4Sut.
        def acquireSut(
            system: ActorSystem[IO],
            cardanoBackend: CardanoBackend[IO],
            peerStackMap: Map[HeadPeerNumber, PeerStack],
            pendingConnsMap: Map[HeadPeerNumber, Deferred[IO, HeadMultisigRegimeManager.Connections]],
            blockBriefsMap: Map[HeadPeerNumber, Ref[IO, Vector[BlockBrief.Intermediate]]],
            stacksMap: Map[HeadPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
            peerLiaisonMap: Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaisonHeadToHead.Handle]],
            ws: WsNetwork,
            coilAckSequencer: Option[CoilAckSequencer.Handle],
            coilRelay: Option[CoilRelay.Handle],
            coilWirings: List[CoilWiring],
            submittedRequestIds: Ref[IO, Vector[RequestId]],
            fastSettlementSignal: Deferred[IO, Unit],
            slowCoverageSignal: Deferred[IO, Unit],
            fastSettlementTarget: Deferred[IO, Set[RequestId]],
            slowCoverageTarget: Deferred[IO, Set[Int]],
        ): IO[Stage4Sut] = {
            // Coil-ward additions merged into the hub head peer's Connections (below). The hub's
            // view of each coil peer's liaison is the in-process handle in Direct mode, or a
            // `RemoteCoilProxy` (over the shared server) in WS mode.
            val hubExtraLiaisons = coilWirings.map(_.headLiaison)
            val hubRemoteCoil =
                if wsMode then ws.remoteCoilProxies
                else coilWirings.map(c => c.coilNum -> c.coilLiaison).toMap
            for {
                // Complete each peer's deferred with its full wiring.
                _ <- peers.traverse { peerNum =>
                    val stack = peerStackMap(peerNum)
                    val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
                    val localLiaisons = peerLiaisonMap(peerNum).values.toList
                    // The hub head peer additionally fans the population stream to its coil-ward
                    // liaisons (kept separate from the head mesh) and owns the relay sequencers.
                    val isHub = peerNum == hubNum
                    val hubCoilLiaisons = if isHub then hubExtraLiaisons else Nil
                    // Per-peer rate limiters wrapping BlockWeaver and StackComposer. With the
                    // default RateLimits config (zero periods) these are no-ops; non-zero
                    // periods enable throttling for the corresponding lane.
                    val bwlTracer: ContraTracer[IO, LimiterEvent] =
                        Slf4jTracer.sink.contramap(LimiterEventFormat.humanFormat("BlockWeaver"))
                    val sclTracer: ContraTracer[IO, LimiterEvent] =
                        Slf4jTracer.sink.contramap(LimiterEventFormat.humanFormat("StackComposer"))
                    for {
                        blockWeaverLimiter <- system.actorOf(
                          Limiter[BlockWeaver.Request](stack.blockWeaver, nodeConfig, bwlTracer)
                        )
                        stackComposerLimiter <- system.actorOf(
                          Limiter[StackComposer.Request](stack.stackComposer, nodeConfig, sclTracer)
                        )
                        _ <- pendingConnsMap(peerNum)
                            .complete(
                              HeadMultisigRegimeManager.Connections(
                                blockWeaver = stack.blockWeaver,
                                blockWeaverLimiter = blockWeaverLimiter,
                                cardanoLiaison = stack.cardanoLiaison,
                                consensusActor = stack.consensusActor,
                                requestSequencer = stack.requestSequencer,
                                jointLedger = stack.jointLedger,
                                stackComposer = stack.stackComposer,
                                stackComposerLimiter = stackComposerLimiter,
                                slowConsensusActor = stack.slowConsensusActor,
                                headPeerLiaisons = localLiaisons,
                                remoteHeadLiaisons = ws.remoteHeadByPeer(peerNum),
                                remoteCoilLiaisons = if isHub then hubRemoteCoil else Map.empty,
                                coilPeerLiaisons = hubCoilLiaisons,
                                coilAckSequencer = if isHub then coilAckSequencer else None,
                                coilRelay = if isHub then coilRelay else None,
                              )
                            )
                            .void
                    } yield ()
                }

                // Complete each coil peer's deferred with its follower wiring: its single liaison
                // toward the hub and the hub-side liaison as its only remote. Hard-confirmed stacks
                // are captured by the SlowConsensusActor tracer sink wired in `buildCoilWirings`.
                _ <- coilWirings.traverse_ { c =>
                    val bwlTracer: ContraTracer[IO, LimiterEvent] =
                        Slf4jTracer.sink.contramap(LimiterEventFormat.humanFormat("BlockWeaver"))
                    val sclTracer: ContraTracer[IO, LimiterEvent] =
                        Slf4jTracer.sink.contramap(LimiterEventFormat.humanFormat("StackComposer"))
                    for {
                        blockWeaverLimiter <- system.actorOf(
                          Limiter[BlockWeaver.Request](c.stack.blockWeaver, c.config, bwlTracer)
                        )
                        stackComposerLimiter <- system.actorOf(
                          Limiter[StackComposer.Request](c.stack.stackComposer, c.config, sclTracer)
                        )
                        _ <- c.pending
                            .complete(
                              HeadMultisigRegimeManager.Connections(
                                blockWeaver = c.stack.blockWeaver,
                                blockWeaverLimiter = blockWeaverLimiter,
                                cardanoLiaison = c.stack.cardanoLiaison,
                                consensusActor = c.stack.consensusActor,
                                jointLedger = c.stack.jointLedger,
                                stackComposer = c.stack.stackComposer,
                                stackComposerLimiter = stackComposerLimiter,
                                slowConsensusActor = c.stack.slowConsensusActor,
                                coilUplink = Some(c.coilLiaison),
                                // The coil's view of its hub liaison: in-process in Direct mode, a
                                // `RemoteHubProxy` (over the coil's uplink) in WS mode.
                                remoteHubLiaison = Some(
                                  if wsMode then ws.remoteHubProxies(c.coilNum) else c.headLiaison
                                ),
                              )
                            )
                            .void
                    } yield ()
                }
                sutErrors <- Ref[IO].of(List.empty[String])
                errorDrainer <- system.eventStream.take
                    .flatMap {
                        case e: ActorError if e.cause != ActorError.NoCause =>
                            sutErrors.update(_ :+ s"[${e.logSource}] ${e.cause.getMessage}")
                        case _ => IO.unit
                    }
                    .foreverM
                    .start
                // Side-fiber per peer that periodically pokes its CardanoLiaison with
                // `Timeout`. cats-actors' `setReceiveTimeout` is unusable under TestControl
                // (1s-virtual-ping + wall-clock check), so we drive polling externally to
                // honor `cardanoLiaisonPollingPeriod` in virtual time. `Temporal.sleep` is
                // virtual-clock-aware, so each tick fires after `period` of virtual time and
                // every peer's CardanoLiaison.runEffects runs at the configured cadence.
                liaisonTickFibers <- peers.traverse { peerNum =>
                    val pollingPeriod = multiNodeConfig
                        .nodeConfigs(peerNum)
                        .nodeOperationMultisigConfig
                        .cardanoLiaisonPollingPeriod
                    val liaison = peerStackMap(peerNum).cardanoLiaison
                    (IO.sleep(pollingPeriod) >> (liaison ! CardanoLiaison.Timeout)).foreverM.start
                }
                // Same polling tick for each coil peer's CardanoLiaison.
                coilTickFibers <- coilWirings.traverse { c =>
                    val pollingPeriod =
                        c.config.nodeOperationMultisigConfig.cardanoLiaisonPollingPeriod
                    (IO.sleep(
                      pollingPeriod
                    ) >> (c.stack.cardanoLiaison ! CardanoLiaison.Timeout)).foreverM.start
                }
            } yield Stage4Sut(
              system = system,
              cardanoBackend = cardanoBackend,
              peers = peerStackMap.map { case (peerNum, stack) =>
                  peerNum -> Stage4PeerHandle(
                    requestSequencer = stack.requestSequencer.getOrElse(
                      sys.error(s"head peer $peerNum missing RequestSequencer")
                    )
                  )
              },
              sutErrors = sutErrors,
              errorDrainer = errorDrainer,
              liaisonTickFibers = liaisonTickFibers.toList ++ coilTickFibers,
              blockBriefs = blockBriefsMap,
              stacks = stacksMap,
              coilStacks = coilWirings.map(c => c.coilNum -> c.stacksRef).toMap,
              backendStores = peerStackMap.map { case (peerNum, stack) =>
                  peerNum -> stack.backendStore
              },
              submittedRequestIds = submittedRequestIds,
              fastSettlementSignal = fastSettlementSignal,
              slowCoverageSignal = slowCoverageSignal,
              fastSettlementTarget = fastSettlementTarget,
              slowCoverageTarget = slowCoverageTarget,
              log = Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("Stage4.Sut")),
            )
        }

        for {
            _ <- Resource.eval(preSystem)
            system <- ActorSystem[IO](label)
            pss <- Resource.eval(postSystem)
            peerStackMap <- peers.toList
                .traverse { peerNum =>
                    buildPeerStack(
                      peerNum,
                      system,
                      pss.cardanoBackend,
                      pss.pendingConnsMap,
                      pss.blockBriefsMap,
                      pss.stacksMap,
                      pss.submittedRequestIds,
                      pss.fastSettlementSignal,
                      pss.slowCoverageSignal,
                      pss.fastSettlementTarget,
                      pss.slowCoverageTarget,
                    ).map(peerNum -> _)
                }
                .map(_.toMap)
            peerLiaisonMap <- Resource.eval(postStack(system, peerStackMap, pss.pendingConnsMap))
            ws <- transportSetup(system, peerLiaisonMap)
            coilParts <- buildCoilWirings(
              system,
              pss.cardanoBackend,
              peerStackMap,
              pss.pendingConnsMap,
              ws,
            )
            (coilAckSequencer, coilRelay, coilWirings) = coilParts
            sut <- Resource.make(
              acquireSut(
                system,
                pss.cardanoBackend,
                peerStackMap,
                pss.pendingConnsMap,
                pss.blockBriefsMap,
                pss.stacksMap,
                peerLiaisonMap,
                ws,
                coilAckSequencer,
                coilRelay,
                coilWirings,
                pss.submittedRequestIds,
                pss.fastSettlementSignal,
                pss.slowCoverageSignal,
                pss.fastSettlementTarget,
                pss.slowCoverageTarget,
              )
            )(sut =>
                sut.liaisonTickFibers.traverse_(_.cancel) >>
                    IO.sleep(100.millis) >>
                    sut.errorDrainer.cancel
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

    /** WS-mode helper. Builds, per head peer, the head-mesh [[PeerTransport]] and (on the hub)
      * the [[HubWsTransport]], mounts both on **one** shared [[NodeWsServer]] per peer (routes
      * `/head` and `/hub`), and starts the mesh dialers. For each coil peer it builds a
      * [[CoilPeerWsTransport]] dialing the hub's `/hub`. Returns the head-mesh remote-proxy map
      * plus the coil transports and the [[RemoteCoilProxy]] / [[RemoteHubProxy]] handles, and a
      * cleanup IO that releases every server/dialer/client.
      *
      * The real coil liaisons are spawned later (in `buildCoilWirings`) and registered onto these
      * transports there.
      */
    private def setupWebSocketNetwork(
        multiNodeConfig: MultiNodeConfig,
        peers: Seq[HeadPeerNumber],
        basePort: Int,
        system: ActorSystem[IO],
        peerLiaisonMap: Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaisonHeadToHead.Handle]],
        coilConfigs: List[NodeConfig],
        hubNum: HeadPeerNumber,
    )(using CardanoNetwork.Section): Resource[IO, WsNetwork] = {
        // Map each peer to a localhost address. Bind on 127.0.0.1 (avoids firewall prompts on
        // dev machines); peers dial each other at the same address+port.
        def addrFor(peerNum: HeadPeerNumber): (Host, Port) =
            (host"127.0.0.1", Port.fromInt(basePort + (peerNum: Int)).get)

        def uriFor(peerNum: HeadPeerNumber): Uri = {
            val (h, p) = addrFor(peerNum)
            Uri.unsafeFromString(s"ws://$h:$p/head")
        }

        val coilNums: List[CoilPeerNumber] = coilConfigs.map(_.ownPeerId match {
            case PeerId.Coil(n) => n
            case PeerId.Head(_) => throw new IllegalStateException("coil config carries a head id")
        })
        val (hubHost, hubPort) = addrFor(hubNum)
        val hubCoilUri = Uri.unsafeFromString(s"ws://$hubHost:$hubPort/hub")

        for {
            // Per-head mesh transports, hub coil transport, and shared WS client (all IO).
            wsParts <- Resource.eval(for {
                meshTransports <- peers.toList
                    .traverse { ownPeerNum =>
                        val ownPeerId = headPeerId(multiNodeConfig, ownPeerNum)
                        val remotes: Map[HeadPeerId, Uri] = peers
                            .filterNot(_ == ownPeerNum)
                            .map(rpn => headPeerId(multiNodeConfig, rpn) -> uriFor(rpn))
                            .toMap
                        val pwsTracer = Slf4jTracer.sink.contramap(
                          PeerTransportEventFormat.humanFormat(ownPeerNum)
                        )
                        PeerTransport.create(ownPeerId, remotes, pwsTracer).map(ownPeerNum -> _)
                    }
                    .map(_.toMap)
                coilHubTransportOpt <-
                    if coilNums.isEmpty then IO.none[HubWsTransport]
                    else
                        val hwtTracer = Slf4jTracer.sink
                            .contramap(HubWsTransportEventFormat.humanFormat(hubNum))
                        HubWsTransport.create(coilNums, hwtTracer).map(Some(_))
                client <- JdkWSClient.simple[IO]
            } yield (meshTransports, coilHubTransportOpt, client))
            (meshTransports, coilHubTransportOpt, client) = wsParts

            // One shared server per peer: mesh `/head` for every head, plus the hub's `/hub`.
            _ <- peers.toList.traverse_ { ownPeerNum =>
                val (bindH, bindP) = addrFor(ownPeerNum)
                val meshRoute =
                    (wsb: WebSocketBuilder2[IO]) => meshTransports(ownPeerNum).routes(wsb)
                val coilRoute =
                    if ownPeerNum == hubNum then
                        coilHubTransportOpt.toList.map(t =>
                            (wsb: WebSocketBuilder2[IO]) => t.routes(wsb)
                        )
                    else Nil
                val nwsTracer =
                    Slf4jTracer.sink.contramap(NodeWsServerEventFormat.humanFormat(ownPeerNum))
                NodeWsServer.resource(bindH, bindP, meshRoute :: coilRoute, nwsTracer)
            }

            // Register liaisons, start mesh dialers, build remote proxies (IO between Resource steps).
            remoteParts <- Resource.eval(for {
                _ <- peers.toList.traverse_ { ownPeerNum =>
                    peerLiaisonMap(ownPeerNum).toList.traverse_ { case (remotePeerId, localLiaison) =>
                        meshTransports(ownPeerNum).register(remotePeerId, localLiaison)
                    }
                }
                remoteHeadByPeer <- peers.toList
                    .traverse { ownPeerNum =>
                        val transport = meshTransports(ownPeerNum)
                        peers
                            .filterNot(_ == ownPeerNum)
                            .toList
                            .traverse { remotePeerNum =>
                                val remotePeerId = headPeerId(multiNodeConfig, remotePeerNum)
                                RemotePeerProxy(remotePeerId, transport)
                                    .flatMap(system.actorOf)
                                    .map(remotePeerNum -> _)
                            }
                            .map(ownPeerNum -> _.toMap)
                    }
                    .map(_.toMap)
                remoteCoilProxies <- coilHubTransportOpt match {
                    case None => IO.pure(Map.empty[CoilPeerNumber, LiaisonProtocol.CoilToHubHandle])
                    case Some(t) =>
                        coilNums
                            .traverse(coilNum =>
                                RemoteCoilProxy(coilNum, t)
                                    .flatMap(system.actorOf)
                                    .map(coilNum -> _)
                            )
                            .map(_.toMap)
                }
            } yield (remoteHeadByPeer, remoteCoilProxies))
            (remoteHeadByPeer, remoteCoilProxies) = remoteParts

            // Start the mesh dialers (lower dials higher).
            _ <- peers.toList.traverse_ { ownPeerNum =>
                meshTransports(ownPeerNum).startDialers(client)
            }

            // Coil uplinks: create each transport, start its dialer, then build the hub proxy.
            coilParts <- coilNums
                .traverse { coilNum =>
                    val cpwtTracer = Slf4jTracer.sink
                        .contramap(CoilPeerWsTransportEventFormat.humanFormat(coilNum))
                    for
                        up <- Resource.eval(
                          CoilPeerWsTransport.create(coilNum, hubCoilUri, cpwtTracer)
                        )
                        _ <- up.startDialer(client)
                        proxy <- Resource.eval(
                          RemoteHubProxy(up).flatMap(system.actorOf)
                        )
                    yield (coilNum -> up, coilNum -> proxy)
                }
                .map(entries => (entries.map(_._1).toMap, entries.map(_._2).toMap))
            (coilUplinks, remoteHubProxies) = coilParts
        } yield WsNetwork(
          remoteHeadByPeer = remoteHeadByPeer,
          coilHubTransport = coilHubTransportOpt,
          coilUplinks = coilUplinks,
          remoteCoilProxies = remoteCoilProxies,
          remoteHubProxies = remoteHubProxies,
        )
    }

    override def beforeFinalize(lastState: ModelState, sut: Stage4Sut): IO[Prop] =
        for
            _ <- log.warn("beforeFinalize")
            submitted <- sut.submittedRequestIds.get
            // Arm the fast-cycle drain: publish the final submitted set so the JL capture sink
            // knows the target. The sink fires fastSettlementSignal only after this is set,
            // preventing mid-run firing against a partial submittedRequestIds snapshot.
            _ <- sut.fastSettlementTarget.complete(submitted.toSet)
            // One-time coverage check: if all IDs already landed before we armed the target,
            // fire the signal ourselves (no new brief will arrive to trigger the sink).
            allBriefs <- sut.blockBriefs.values.toList.traverse(_.get).map(_.flatten)
            seen       = allBriefs
                             .flatMap(br =>
                                 br.events.map(_._1) ++ br.depositsAbsorbed ++ br.depositsRefunded
                             )
                             .toSet
            _ <- IO.whenA(submitted.forall(seen.contains))(
                     sut.fastSettlementSignal.complete(()).void
                 )
            _ <- IO.whenA(submitted.nonEmpty)(sut.fastSettlementSignal.get)
            // Arm the slow-cycle drain: freeze the block nums that must be covered. Done after
            // the fast drain so any blocks produced during that wait are included in the target.
            blockNums <- sut.blockBriefs.values.toList
                             .traverse(_.get)
                             .map(_.flatten.map(b => (b.blockNum: Int)).toSet)
            _ <- sut.slowCoverageTarget.complete(blockNums)
            // One-time coverage check across ALL peers — matching the sink condition so a spurious
            // signal fire can't race ahead of any peer's stacksMap update.
            allPeersStacks <- sut.stacks.values.toList.traverse(_.get)
            allCovered      = blockNums.isEmpty ||
                                  allPeersStacks.forall { peerStacks =>
                                      blockNums.forall { bn =>
                                          peerStacks.exists { s =>
                                              (s.brief.firstBlockNum: Int) <= bn &&
                                              bn <= (s.brief.lastBlockNum: Int)
                                          }
                                      }
                                  }
            _ <- IO.whenA(allCovered)(sut.slowCoverageSignal.complete(()).void)
            _ <- IO.whenA(blockNums.nonEmpty)(sut.slowCoverageSignal.get)
            errors <- sut.sutErrors.get
            analysisProp <- analyzeBlockBriefs(lastState, sut)
            sortedPeers = sut.stacks.keys.toSeq.sortBy(p => p: Int)
            stacksByPeer <- sut.stacks.toList
                                .traverse { case (p, ref) => ref.get.map(p -> _) }
                                .map(_.toMap)
            persistenceProp <- analyzePersistence(sut, stacksByPeer, sortedPeers)
            props = analysisProp && persistenceProp
        yield
            if errors.nonEmpty then
                Prop.exception(RuntimeException(s"SUT actor errors:\n${errors.mkString("\n")}"))
            else props

    private def analyzeBlockBriefs(lastState: ModelState, sut: Stage4Sut): IO[Prop] = {
        for
            briefsByPeer <- sut.blockBriefs.toList
                .traverse { case (p, ref) => ref.get.map(p -> _) }
                .map(_.toMap)

            sortedPeers = briefsByPeer.keys.toSeq.sortBy(p => p: Int)
            nPeers = sortedPeers.length
            canonicalBriefs = briefsByPeer(sortedPeers.head)
            submittedIds <- sut.submittedRequestIds.get

            stacksByPeer <- sut.stacks.toList
                .traverse { case (p, ref) => ref.get.map(p -> _) }
                .map(_.toMap)
            canonicalStacks = stacksByPeer(sortedPeers.head)
            _ <- log.info(
              "hard-confirmed stacks per peer: " +
                  sortedPeers
                      .map(p => s"peer${p: Int}=${stacksByPeer.getOrElse(p, Vector.empty).size}")
                      .mkString(", ")
            )

            coilStacksByCoil <- sut.coilStacks.toList
                .traverse { case (c, ref) => ref.get.map(c -> _) }
                .map(_.toMap)
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
            // knob so a Yaci / Blockfrost-backed stage4 future swap just bumps these.
            effectsLandedProp <- EffectsLanded.propEffectsLanded(
              canonicalStacks,
              sut.cardanoBackend,
              log,
              attempts = 1,
              sleep = 0.seconds,
            )

        yield propLiveness(submittedIds, canonicalBriefs) &&
            propDepositTiming(lastState.registeredDeposits, canonicalBriefs) &&
            propValidRatio(lastState, canonicalBriefs) &&
            propStackCoverage(canonicalBriefs, canonicalStacks) &&
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
                val backend = sut.backendStores(peerNum)
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
            s"(SUT is more permissive than the model)"
    }

    /** Property: the slow cycle made progress and kept up — it hard-confirmed at least one stack
      * and, by shutdown idle, every block the fast cycle produced (captured via the brief
      * ContraTracer in `buildPeerStack`) is contained in some hard-confirmed stack (captured via
      * the SCA ContraTracer sink). Catches a slow side that stalls, never closes a stack, or fails
      * to aggregate hard-acks into [[Stack.HardConfirmed]]. Both observers read the canonical peer.
      */
    private def propStackCoverage(
        canonicalBriefs: Vector[BlockBrief.Intermediate],
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
        def covered(n: BlockNumber): Boolean =
            coveredRanges.exists { case (lo, hi) => lo <= (n: Int) && (n: Int) <= hi }
        val observedBlocks: Set[BlockNumber] = canonicalBriefs.map(_.blockNum).toSet
        val uncovered = observedBlocks.filterNot(covered)
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
                    hc.maxCardanoLiaisonPollingPeriod / 2
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
                      hydrozoaHost = "localhost",
                      hydrozoaPort = "4973",
                      blockfrostApiKey = "not-a-real-key"
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
