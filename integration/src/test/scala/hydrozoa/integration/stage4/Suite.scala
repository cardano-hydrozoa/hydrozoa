package hydrozoa.integration.stage4

import cats.data.ReaderT
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.implicits.*
import com.suprnation.actor.ActorSystem
import com.suprnation.actor.event.Error as ActorError
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.config.head.initialization.{InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.generateYaciTxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{InitParamsType, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.stage4.Model.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import cats.effect.IOLocal
import hydrozoa.lib.logging.{Logging, Tracer}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendMock, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.consensus.transport.{PeerWsTransport, RemotePeerProxy}
import hydrozoa.multisig.consensus.limiter.Limiter
import hydrozoa.multisig.consensus.{BlockWeaver, CardanoLiaison, CardanoLiaisonEvent, CardanoLiaisonEventFormat, EventSequencer, FastConsensusActor, FastConsensusActorEvent, FastConsensusActorEventFormat, PeerLiaison, SlowConsensusActor, SlowConsensusActorEvent, SlowConsensusActorEventFormat, StackComposer, StackComposerEvent, StackComposerEventFormat}
import org.http4s.Uri
import com.comcast.ip4s.{Host, Port, host}
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.eutxol2.{EutxoL2Ledger, toUtxos}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.ledger.joint.{JointLedger, JointLedgerEvent, JointLedgerEventFormat}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects}
import hydrozoa.multisig.persistence.{BackendStore, Cf, InMemoryBackendStore, Persistence}
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import org.scalacheck.commands.{AnyCommand, ModelBasedSuite, ScenarioGen}
import org.scalacheck.{Gen, Prop}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.{CertState, TransactionInput, Utxos}
import test.{SeedPhrase, TestPeers, given}

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}

// ===================================
// Stage 4 suite
// ===================================

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
    nCommands: Int = 500,
    transportMode: TransportMode = TransportMode.Direct,
    backendMode: BackendMode = BackendMode.InMemory,
) extends ModelBasedSuite:

    override type Env = Unit
    override type State = ModelState
    override type Sut = Stage4Sut

    private val logger = Logging.loggerIO("Stage4.Suite")

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
        super.onTestCaseGenerated(initialState, commands) >>
            logger.info(Stage4Runner.renderTable(initialState, commands))

    override def initEnv: Unit = ()

    override def genInitialState(env: Unit): Gen[ModelState] =
        Stage4Suite.genInitialState(nPeers = nPeers, useTestControl = useTestControl)

    override def canStartupNewSut(): Boolean = true

    override def startupSut(state: ModelState): Resource[IO, Stage4Sut] = {
        val multiNodeConfig = state.params.multiNodeConfig
        val cardanoInfo = multiNodeConfig.headConfig.cardanoInfo
        val peers = multiNodeConfig.nodeConfigs.keys.toSeq.sortBy(p => p: Int)

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

        // ------ Pre-system IO: clock alignment + tracer local. ------
        val preSystem: IO[IOLocal[Tracer]] = for {
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
                                s"Stage4 startupSut: initialization took too long " +
                                    s"(takeoff: $t, now: $now)"
                              )
                            )
                        else
                            val sleepMs = t.toEpochMilli - now.toEpochMilli
                            IO.sleep(FiniteDuration(sleepMs, TimeUnit.MILLISECONDS))
                    }
            }
            tracerLocal <- Tracer.makeLocal
        } yield tracerLocal

        // ------ Post-system, pre-stack IO: backend + per-peer Deferred/Ref maps. ------
        val postSystem: IO[
          (
              CardanoBackend[IO],
              Map[HeadPeerNumber, Deferred[IO, MultisigRegimeManager.Connections]],
              Map[HeadPeerNumber, Ref[IO, Vector[BlockBrief.Intermediate]]]
          )
        ] = {
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
                    Deferred[IO, MultisigRegimeManager.Connections].map(peerNum -> _)
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
            } yield (cardanoBackend, pendingConnsMap, blockBriefsMap)
        }

        // ------ Per-peer actor stack. Brackets `openPeerBackend` (RocksDB on disk); the rest
        // ------ is plain IO around it.
        def buildPeerStack(
            peerNum: HeadPeerNumber,
            system: ActorSystem[IO],
            cardanoBackend: CardanoBackend[IO],
            pendingConnsMap: Map[HeadPeerNumber, Deferred[IO, MultisigRegimeManager.Connections]],
            blockBriefsMap: Map[HeadPeerNumber, Ref[IO, Vector[BlockBrief.Intermediate]]],
        ): Resource[IO, PeerStack] = {
            val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
            val pending = pendingConnsMap(peerNum)
            val fcaTracer: ContraTracer[IO, FastConsensusActorEvent] =
                Tracer.sink.contramap(FastConsensusActorEventFormat.humanFormat(peerNum))
                    |+| Tracer.sink.traceMaybe(FastConsensusActorEventFormat.jsonlFormat(peerNum))
            val clTracer: ContraTracer[IO, CardanoLiaisonEvent] =
                Tracer.sink.contramap(CardanoLiaisonEventFormat.humanFormat(peerNum))
                    |+| Tracer.sink.traceMaybe(CardanoLiaisonEventFormat.jsonlFormat(peerNum))
            val scTracer: ContraTracer[IO, StackComposerEvent] =
                Tracer.sink.contramap(StackComposerEventFormat.humanFormat(peerNum))
                    |+| Tracer.sink.traceMaybe(StackComposerEventFormat.jsonlFormat(peerNum))
            val scaTracer: ContraTracer[IO, SlowConsensusActorEvent] =
                Tracer.sink.contramap(SlowConsensusActorEventFormat.humanFormat(peerNum))
                    |+| Tracer.sink.traceMaybe(SlowConsensusActorEventFormat.jsonlFormat(peerNum))
            // Capture sink: only briefs go into the per-peer Ref.
            val captureSink: ContraTracer[IO, JointLedgerEvent] =
                ContraTracer.emit[IO, JointLedgerEvent] {
                    case JointLedgerEvent.BriefProduced(b) =>
                        blockBriefsMap(peerNum).update(_ :+ b)
                    case _ => IO.unit
                }
            // SLF4J sink: all JL events get a human-readable line so test runs are debuggable.
            val textSink: ContraTracer[IO, JointLedgerEvent] =
                Tracer.sink.contramap(JointLedgerEventFormat.humanFormat(peerNum))
            val jlTracer: ContraTracer[IO, JointLedgerEvent] = captureSink |+| textSink
            // Per-peer persistence backend — InMemory by default; RocksDb when the
            // suite is constructed with `BackendMode.RocksDb(root)`. Built first so
            // every producer (EventSequencer/JL/FCA/SC/SCA/PeerLiaison) shares the
            // one per-peer store; `analyzePersistence` reads it back.
            openPeerBackend(peerNum).evalMap { backendStore =>
                for
                    persistence <- {
                        given CardanoNetwork.Section = nodeConfig
                        Persistence.fromBackend(backendStore)
                    }
                    blockWeaver <- system.actorOf(BlockWeaver(nodeConfig, pending))
                    cardanoLiaison <- system.actorOf(
                      CardanoLiaison(nodeConfig, cardanoBackend, pending, clTracer)
                    )
                    eventSequencer <- system.actorOf(
                      EventSequencer(nodeConfig, pending, persistence)
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
                  eventSequencer,
                  jointLedger,
                  consensusActor,
                  stackComposer,
                  slowConsensusActor,
                  backendStore,
                  persistence
                )
            }
        }

        // ------ Post-stack, pre-transport IO: observers + local peer liaisons. ------
        def postStack(
            system: ActorSystem[IO],
            peerStackMap: Map[HeadPeerNumber, PeerStack],
            pendingConnsMap: Map[HeadPeerNumber, Deferred[IO, MultisigRegimeManager.Connections]],
        ): IO[
          (
              Map[HeadPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
              Map[HeadPeerNumber, CardanoLiaison.Handle],
              Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaison.Handle]]
          )
        ] = for {
            // Create stack-collecting observers wrapping each peer's CardanoLiaison.
            // Injected via Connections so SlowConsensusActor is unaware; captures every
            // hard-confirmed stack the slow cycle emits (parallel to the brief-capture
            // ContraTracer on the fast side).
            stacksMap <- peers
                .traverse { peerNum =>
                    Ref[IO].of(Vector.empty[Stack.HardConfirmed]).map(peerNum -> _)
                }
                .map(_.toMap)
            stackObserverMap <- peers
                .traverse { peerNum =>
                    system
                        .actorOf(
                          StackObserver(
                            peerNum,
                            peerStackMap(peerNum).cardanoLiaison,
                            stacksMap(peerNum)
                          )
                        )
                        .map(peerNum -> _)
                }
                .map(_.toMap)
            // Create one local PeerLiaison per (local, remote) pair.
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
                            val remotePeerId =
                                multiNodeConfig.nodeConfigs(remotePeerNum).ownHeadPeerId
                            system
                                .actorOf(
                                  PeerLiaison(nodeConfig, remotePeerId, pending, persistence)
                                )
                                .map(remotePeerId -> _)
                        }
                        .map(liaisons => peerNum -> liaisons.toMap)
                }
                .map(_.toMap)
        } yield (stacksMap, stackObserverMap, peerLiaisonMap)

        // ------ Build the per-peer remote-liaison map. In Direct mode this is the actual
        // ------ PeerLiaison handle from the other peer's actor stack. In WebSocket mode it's
        // ------ a RemotePeerProxy that forwards messages over the local PeerWsTransport.
        // ------ remoteLiaisonsByPeer(A)(B.id) = the handle that A's PeerLiaison(A->B) uses to
        // ------ reach B's PeerLiaison(B->A).
        def transportSetup(
            system: ActorSystem[IO],
            peerLiaisonMap: Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaison.Handle]],
        ): Resource[IO, Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaison.Handle]]] =
            transportMode match {
                case TransportMode.Direct =>
                    val remoteLiaisonsByPeer
                        : Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaison.Handle]] =
                        peers.map { peerNum =>
                            val ownPeerId = multiNodeConfig.nodeConfigs(peerNum).ownHeadPeerId
                            peerNum -> peers
                                .filterNot(_ == peerNum)
                                .map { remotePeerNum =>
                                    val remotePeerId =
                                        multiNodeConfig.nodeConfigs(remotePeerNum).ownHeadPeerId
                                    remotePeerId -> peerLiaisonMap(remotePeerNum)(ownPeerId)
                                }
                                .toMap
                        }.toMap
                    Resource.pure(remoteLiaisonsByPeer)
                case TransportMode.WebSocket(basePort) =>
                    given CardanoNetwork.Section = multiNodeConfig.headConfig
                    setupWebSocketTransports(
                      multiNodeConfig,
                      peers,
                      basePort,
                      system,
                      peerLiaisonMap,
                    )
            }

        // ------ Post-transport IO: wire each peer's Connections, start the error drainer and
        // ------ per-peer CardanoLiaison-tick fibers, then assemble the Stage4Sut.
        def finalizeSut(
            tracerLocal: IOLocal[Tracer],
            system: ActorSystem[IO],
            cardanoBackend: CardanoBackend[IO],
            peerStackMap: Map[HeadPeerNumber, PeerStack],
            pendingConnsMap: Map[HeadPeerNumber, Deferred[IO, MultisigRegimeManager.Connections]],
            blockBriefsMap: Map[HeadPeerNumber, Ref[IO, Vector[BlockBrief.Intermediate]]],
            stacksMap: Map[HeadPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
            stackObserverMap: Map[HeadPeerNumber, CardanoLiaison.Handle],
            peerLiaisonMap: Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaison.Handle]],
            remoteLiaisonsByPeer: Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaison.Handle]],
        ): IO[Stage4Sut] = for {
            // Complete each peer's deferred with its full wiring.
            _ <- peers.traverse { peerNum =>
                val stack = peerStackMap(peerNum)
                val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
                val localLiaisons = peerLiaisonMap(peerNum).values.toList
                for {
                    // Per-peer rate limiters wrapping BlockWeaver and StackComposer. With the
                    // default RateLimits config (zero periods) these are no-ops; non-zero
                    // periods enable throttling for the corresponding lane.
                    blockWeaverLimiter <- system.actorOf(
                      Limiter[BlockWeaver.Request](stack.blockWeaver, nodeConfig)
                    )
                    stackComposerLimiter <- system.actorOf(
                      Limiter[StackComposer.Request](stack.stackComposer, nodeConfig)
                    )
                    _ <- pendingConnsMap(peerNum)
                        .complete(
                          MultisigRegimeManager.Connections(
                            blockWeaver = stack.blockWeaver,
                            blockWeaverLimiter = blockWeaverLimiter,
                            // Route the slow cycle's Stack.HardConfirmed through StackObserver
                            // (forwards to the real CardanoLiaison) so the test can assert
                            // coverage.
                            cardanoLiaison = stackObserverMap(peerNum),
                            consensusActor = stack.consensusActor,
                            eventSequencer = stack.eventSequencer,
                            jointLedger = stack.jointLedger,
                            stackComposer = stack.stackComposer,
                            stackComposerLimiter = stackComposerLimiter,
                            slowConsensusActor = stack.slowConsensusActor,
                            peerLiaisons = localLiaisons,
                            remotePeerLiaisons = remoteLiaisonsByPeer(peerNum),
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
            submittedRequestIds <- Ref[IO].of(Vector.empty[RequestId])
        } yield Stage4Sut(
          system = system,
          cardanoBackend = cardanoBackend,
          peers = peerStackMap.map { case (peerNum, stack) =>
              peerNum -> Stage4PeerHandle(eventSequencer = stack.eventSequencer)
          },
          sutErrors = sutErrors,
          errorDrainer = errorDrainer,
          liaisonTickFibers = liaisonTickFibers.toList,
          blockBriefs = blockBriefsMap,
          stacks = stacksMap,
          backendStores = peerStackMap.map { case (peerNum, stack) =>
              peerNum -> stack.backendStore
          },
          submittedRequestIds = submittedRequestIds,
          tracerLocal = tracerLocal,
        )

        for {
            tracerLocal <- Resource.eval(preSystem)
            system <- ActorSystem[IO](label)
            postSystemState <- Resource.eval(postSystem)
            (cardanoBackend, pendingConnsMap, blockBriefsMap) = postSystemState
            peerStackMap <- peers.toList
                .traverse { peerNum =>
                    buildPeerStack(
                      peerNum,
                      system,
                      cardanoBackend,
                      pendingConnsMap,
                      blockBriefsMap,
                    ).map(peerNum -> _)
                }
                .map(_.toMap)
            postStackState <- Resource.eval(
              postStack(system, peerStackMap, pendingConnsMap)
            )
            (stacksMap, stackObserverMap, peerLiaisonMap) = postStackState
            remoteLiaisonsByPeer <- transportSetup(system, peerLiaisonMap)
            sut <- Resource.eval(
              finalizeSut(
                tracerLocal,
                system,
                cardanoBackend,
                peerStackMap,
                pendingConnsMap,
                blockBriefsMap,
                stacksMap,
                stackObserverMap,
                peerLiaisonMap,
                remoteLiaisonsByPeer,
              )
            )
        } yield sut
    }

    /** WS-mode helper: builds one [[PeerWsTransport]] per peer (each bound to a distinct localhost
      * port), spawns one [[RemotePeerProxy]] per (local, remote) pair, registers each local
      * [[PeerLiaison]] with its peer's transport, and returns the remote-handle map (proxy actors
      * keyed by remote peer id), per peer. Transport sockets are released when the `Resource` is
      * finalized.
      */
    private def setupWebSocketTransports(
        multiNodeConfig: MultiNodeConfig,
        peers: Seq[HeadPeerNumber],
        basePort: Int,
        system: ActorSystem[IO],
        peerLiaisonMap: Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaison.Handle]],
    )(using
        CardanoNetwork.Section
    ): Resource[IO, Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaison.Handle]]] = {
        // Map each peer to a localhost address. Bind on 127.0.0.1 (avoids firewall prompts on
        // dev machines); peers dial each other at the same address+port.
        def addrFor(peerNum: HeadPeerNumber): (Host, Port) =
            (host"127.0.0.1", Port.fromInt(basePort + (peerNum: Int)).get)

        def uriFor(peerNum: HeadPeerNumber): Uri = {
            val (h, p) = addrFor(peerNum)
            Uri.unsafeFromString(s"ws://$h:$p/peer")
        }

        for {
            transports <- peers.toList.traverse { ownPeerNum =>
                val ownPeerId = multiNodeConfig.nodeConfigs(ownPeerNum).ownHeadPeerId
                val (bindH, bindP) = addrFor(ownPeerNum)
                val remotes: Map[HeadPeerId, Uri] = peers
                    .filterNot(_ == ownPeerNum)
                    .map { rpn =>
                        multiNodeConfig.nodeConfigs(rpn).ownHeadPeerId -> uriFor(rpn)
                    }
                    .toMap
                PeerWsTransport
                    .resource(ownPeerId, bindH, bindP, remotes)
                    .map(ownPeerNum -> _)
            }.map(_.toMap)

            // Register each local liaison so inbound msgs from remote get dispatched correctly.
            // peerLiaisonMap(A)(B.id) is A's local liaison for talking to B. When A's transport
            // receives a msg from B, it dispatches to that liaison.
            _ <- Resource.eval(peers.toList.traverse_ { ownPeerNum =>
                val transport = transports(ownPeerNum)
                peerLiaisonMap(ownPeerNum).toList.traverse_ { case (remotePeerId, localLiaison) =>
                    transport.register(remotePeerId, localLiaison)
                }
            })

            // Build the proxy actors: one per (local peer, remote peer) pair.
            remoteHandlesByPeer <- Resource.eval(
              peers.toList
                  .traverse { ownPeerNum =>
                      val transport = transports(ownPeerNum)
                      peers
                          .filterNot(_ == ownPeerNum)
                          .toList
                          .traverse { remotePeerNum =>
                              val remotePeerId =
                                  multiNodeConfig.nodeConfigs(remotePeerNum).ownHeadPeerId
                              for {
                                  proxy <- RemotePeerProxy(remotePeerId, transport)
                                  ref <- system.actorOf(proxy)
                              } yield remotePeerId -> ref
                          }
                          .map(ownPeerNum -> _.toMap)
                  }
                  .map(_.toMap)
            )
        } yield remoteHandlesByPeer
    }

    override def shutdownSut(lastState: ModelState, sut: Stage4Sut): IO[Prop] =
        for
            _ <- logger.warn("shutdownSut")
            // Drain before terminating: the leader's JointLedger is in `Producing` when the
            // last command arrives, and its `userRequestState.requests` only become a
            // `BlockBrief` when the block is sealed. Sealing happens when `BlockWeaver` decides
            // it's time — driven by either the dead-man fallback push-out timer or a deposit
            // decision wakeup, whichever fires first. Without `waitForIdle` we'd terminate
            // before any of that completes and miss the in-progress block's events in the
            // analysis, causing spurious liveness failures.
            //
            // `waitForIdle` returns when all mailboxes are empty, the child set is stable, and
            // deadLetters are drained — it does NOT require "no scheduled future sleeps".
            // BlockWeaver's `sleepSendWakeup` runs in a separate fiber, so between wakeups the
            // actor's mailbox is empty and `isIdle` is true.
            //
            // `maxTimeout` is virtual under TestControl and elapses fast in real time. The
            // rate limiter throttles the slow cycle, so the post-command tail can take several
            // hard-stack periods to drain — under WS real-clock that's wall-clock seconds, far
            // beyond the framework default (30s). Size the timeout to the throttle: each stack
            // close sweeps the longest ready prefix, so the remaining blocks fold into the next
            // stack(s); two hard-stack periods cover the close + cross-peer hard-confirm
            // round-trip. Floored at 30s so zero rate limits don't yield a 0 (instant) timeout.
            //
            // Order matters: `waitForIdle` BEFORE cancelling liaison tick fibers. Cancelling
            // ticks first stops the leader's CardanoLiaison from observing L1 settlement,
            // which kills the path that drives the in-progress block to confirmation — the
            // leader keeps applying its mempool tail but never gets `BlockConfirmed` because
            // followers (with ticks cancelled too) stop emitting acks. The result was visible
            // in WS real-clock 10-peer runs: ~15% of submitted reqIds never reached any
            // brief because the leader's WIP block at shutdown was discarded by terminate.
            // Letting ticks run during `waitForIdle` keeps the L1-polling-driven seal path
            // alive long enough for the tail to land in confirmed blocks. `waitForIdle` can
            // still return because the periods between ticks are mailbox-empty.
            stackDrainTimeout =
                (lastState.params.multiNodeConfig.nodeConfigs.values.head.hardStackMinPeriod * 3)
                    .max(30.seconds)
            _ <- sut.system.waitForIdle(maxTimeout = stackDrainTimeout)

            // Slow-cycle tail drain. `waitForIdle` returns when mailboxes are empty + child
            // set stable + deadLetters drained — but it does NOT wait for scheduled future
            // sleeps. The `PeerLiaison` resend timer (`startResendTimer`) ticks in its own
            // fiber via `IO.sleep(peerLiaisonResendInterval) >> ResendCurrent`; between
            // ticks every actor's mailbox is empty and the system reports idle. Under
            // `TransportMode.WebSocket` that interval is real wall-clock, so when the very
            // last stack is closed only milliseconds before `waitForIdle` is called, its
            // hard-acks have not yet completed the cross-peer round-trip needed to
            // saturate every peer's `SlowConsensusActor`. `terminate()` immediately after
            // `waitForIdle` drops the last stack from `StackObserver`'s record, causing a
            // spurious `propStackCoverage` failure on whatever block(s) sit in that final
            // stack. Two effects must be covered: (a) ≥ 2 × `peerLiaisonResendInterval` so a
            // full resend cycle fires across every link and the tail acks land; (b) ≥
            // `hardStackMinPeriod`, because the rate limiter on the SlowConsensusActor →
            // StackComposer lane HOLDS the prior stack's `Stack.HardConfirmed` for that long
            // before it arms StackComposer to close the final stack over the last block(s).
            // Without (b) the throttled final-stack trigger never arrives before `terminate()`
            // and the last block is left uncovered. The fast cycle has its own analogous
            // tail-drain (the L1-polling tick path described just above); this is its
            // slow-cycle counterpart.
            nodeConfig = lastState.params.multiNodeConfig.nodeConfigs.values.head
            slowTailDrain =
                (nodeConfig.peerLiaisonResendInterval * 2) + nodeConfig.hardStackMinPeriod
            _ <- IO.sleep(slowTailDrain)

            _ <- sut.liaisonTickFibers.traverse_(_.cancel)
            _ <- IO.sleep(100.millis) // settle: let the drainer consume any last-cycle errors
            _ <- sut.errorDrainer.cancel
            errors <- sut.sutErrors.get
            analysisProp <- analyzeBlockBriefs(lastState, sut)
        yield
            if errors.nonEmpty then
                Prop.exception(RuntimeException(s"SUT actor errors:\n${errors.mkString("\n")}"))
            else analysisProp

    private def analyzeBlockBriefs(lastState: ModelState, sut: Stage4Sut): IO[Prop] = {
        given IOLocal[Tracer] = sut.tracerLocal
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
            _ <- logger.info(
              "hard-confirmed stacks per peer: " +
                  sortedPeers
                      .map(p => s"peer${p: Int}=${stacksByPeer.getOrElse(p, Vector.empty).size}")
                      .mkString(", ")
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
              attempts = 1,
              sleep = 0.seconds,
            )
            persistenceProp <- analyzePersistence(sut, stacksByPeer, sortedPeers)
        yield propLiveness(submittedIds, canonicalBriefs) &&
            propDepositTiming(lastState.registeredDeposits, canonicalBriefs) &&
            propValidRatio(lastState, canonicalBriefs) &&
            propStackCoverage(canonicalBriefs, canonicalStacks) &&
            effectsLandedProp &&
            persistenceProp
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
      *   - **EventSequencer** writes the assigned request to the `Request` lane (CR1);
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
                    softAcks <- countEntries(backend, Cf.SoftAck)
                    hardAcks <- countEntries(backend, Cf.HardAck)
                    requests <- countEntries(backend, Cf.Request)
                    _ <- logger.info(
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
                        // lane (EventSequencer own + PeerLiaison inbound, CR1/CR8). All non-empty.
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
      * allocated immediately in `startupSut`; cleanup currently leaks RocksDB handles until a
      * stage4-level shutdown hook lands (parallel to `errorDrainer.cancel`).
      */
    private def openPeerBackend(peerNum: HeadPeerNumber): Resource[IO, BackendStore[IO]] =
        backendMode match
            case BackendMode.InMemory => InMemoryBackendStore.open
            case BackendMode.RocksDb(root) =>
                val dir = root.resolve(s"peer-${peerNum: Int}")
                Resource.eval(IO.blocking(Files.createDirectories(dir))) >>
                    RocksDbBackendStore.open(dir)

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
      * ContraTracer in `buildPeerStack`) is contained in some hard-confirmed stack (observed via
      * [[StackObserver]]). Catches a slow side that stalls, never closes a stack, or fails to
      * aggregate hard-acks into [[Stack.HardConfirmed]]. Both observers read the canonical peer.
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
    )(using IOLocal[Tracer]): IO[Unit] = {
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
        Tracer.info(text)
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
    )(using IOLocal[Tracer]): IO[Unit] = {
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
        Tracer.info(text)
    }

// ===================================
// Initial state generator (canonical location; Runner delegates here for @main)
// ===================================

object Stage4Suite:

    def genInitialState(
        nPeers: Int = 2,
        absorptionSlack: FiniteDuration = 60.seconds,
        meanInterArrivalTime: HeadPeerNumber => FiniteDuration = _ => 12.seconds,
        useTestControl: Boolean = true,
    ): Gen[ModelState] =
        val cardanoNetwork = CardanoNetwork.Preprod
        val testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nPeers)
        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)

        // For non-TestControl runs we need the head's initial block end-time anchored at a
        // small wall-clock offset in the future, so `startupSut` can sleep until that anchor
        // and have the model clock and the wall clock coincide at command 1. 60s matches
        // stage 1's budget; if 20-peer setup overruns it the test aborts (see startupSut).
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
            peers.map(pn => pn -> meanInterArrivalTime(pn)).toMap
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
