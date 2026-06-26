package hydrozoa.integration.harness

import cats.effect.{IO, Ref, Resource}
import cats.implicits.*
import com.comcast.ip4s.{Port, host}
import com.suprnation.actor.event.Error as ActorError
import com.suprnation.actor.{ActorContext, ActorSystem}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.CardanoLiaison
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.*
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import hydrozoa.multisig.persistence.{BackendStore, Cf, InMemoryBackendStore, Persistence, PersistenceEvent, PersistenceEventFormat}
import hydrozoa.multisig.{CoilMultisigRegimeManager, HeadMultisigRegimeManager, HeadMultisigRegimeManagerEvent, HeadMultisigRegimeManagerEventFormat}
import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.concurrent.TimeUnit
import org.http4s.Uri
import org.http4s.client.websocket.WSClient
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.websocket.WebSocketBuilder2
import scala.concurrent.duration.{DurationLong, FiniteDuration}
import scalus.cardano.ledger.CertState
import scalus.cardano.ledger.Utxos
import scalus.cardano.ledger.rules.{Context, UtxoEnv}

/** Reusable bring-up scaffold for a multi-peer head (+ optional coil followers) backed by a shared
  * mock L1. Owns the `ActorSystem`, mock `CardanoBackend`, per-peer transports (in-process or WS),
  * per-peer HMRM with `BackendStore` + `Persistence`, per-coil CMRM, the connections drain, an
  * error drainer, and per-peer `CardanoLiaison` tick fibers.
  *
  * Test-side concerns (capture observers, signal `Deferred`s, custom per-peer handle types like
  * stage4's `Stage4PeerHandle` or an RBR test's `CardanoLiaison.Handle` snapshot) are injected via
  * [[Hooks]] — the harness threads test-provided tracers into each MRM and runs a test-provided
  * `(peerNum, connections) => IO[H]` finalizer once each peer's connections are available.
  *
  * Allocation phases (mirrors the historic `Suite.sutResource` flow):
  *   1. `preSystem` — clock alignment: `useTestControl` jump to `startEpochMs`, or wall-clock sleep
  *      until `takeoffTime`. Must complete before `ActorSystem` exists.
  *   2. `ActorSystem` + shared `CardanoBackendMock`.
  *   3. In-process registries (always allocated) + optional hub-coil registry + optional shared WS
  *      client.
  *   4. Per-peer transports + `NodeWsServer` bind (WS mode binds port 0). Pass 2 (WS only): build
  *      the post-bind URI map and start each peer's dialers.
  *   5. Per-peer HMRM (backend store, persistence, L2 ledger).
  *   6. Per-coil CMRM (always-in-memory backend, persistence, L2 ledger, coil uplink).
  *   7. Connections drain + error drainer + CL tick fibers.
  */
object MultiPeerHeadHarness:

    /** Persistence backend selector. In-memory is fast and isolated; RocksDB exercises on-disk
      * compaction/batching.
      */
    enum BackendMode:
        case InMemory
        case RocksDb(root: Path = Files.createTempDirectory("hydrozoa-harness-rocksdb-"))

    /** Per-peer mesh transport selector. `useTestControl` is exposed here so callers can wire it
      * into their `ModelBasedSuite.useTestControl` override without re-deriving the rule.
      */
    enum TransportMode:
        case Direct
        case WebSocket

    extension (mode: TransportMode)
        def useTestControl: Boolean = mode match
            case TransportMode.Direct    => true
            case TransportMode.WebSocket => false

    /** Coarse-grained run knobs. `label` names the `ActorSystem`. */
    case class Config(
        label: String,
        backendMode: BackendMode,
        transportMode: TransportMode,
    )

    /** Per-run configuration assembled by the caller (typically derived from a `ModelState`). */
    case class Inputs(
        config: Config,
        multiNodeConfig: MultiNodeConfig,
        coilNodeConfigs: List[NodeConfig],
        preinitPeerUtxosL1: Map[HeadPeerNumber, Utxos],
        takeoffTime: Option[Instant],
        startEpochMs: Long,
    )

    /** Test-side wiring injected into each MRM. The harness combines `peerTracer(p)` /
      * `coilTracer(c)` with its own slf4j sink via the `ContraTracer` monoid; `peerHandle` /
      * `coilHandle` run once each MRM's `connectionsDeferred` resolves.
      */
    case class Hooks[H, C](
        peerTracer: HeadPeerNumber => ContraTracer[IO, HeadMultisigRegimeManagerEvent],
        // TODO: introduce a distinct `CoilMultisigRegimeManagerEvent` upstream and switch this
        //       type to it. Today `CoilMultisigRegimeManager` reuses `HeadMultisigRegimeManagerEvent`
        //       (see `CoilMultisigRegimeManager.scala:33`), so the harness has no choice.
        coilTracer: CoilPeerNumber => ContraTracer[IO, HeadMultisigRegimeManagerEvent],
        peerHandle: (HeadPeerNumber, HeadMultisigRegimeManager.Connections) => IO[H],
        coilHandle: (CoilPeerNumber, HeadMultisigRegimeManager.Connections) => IO[C],
    )

    /** Per-peer artifacts exposed to callers: resolved connections, persistence backend, and the
      * caller-derived handle.
      */
    case class Peer[H](
        connections: HeadMultisigRegimeManager.Connections,
        backendStore: BackendStore[IO],
        handle: H,
    )

    case class Coil[C](
        connections: HeadMultisigRegimeManager.Connections,
        backendStore: BackendStore[IO],
        handle: C,
    )

    /** Everything the harness yields. `sutErrors` is appended to by the error drainer (one entry
      * per uncaught actor exception); callers read it post-run.
      */
    case class Harness[H, C](
        system: ActorSystem[IO],
        cardanoBackend: CardanoBackend[IO],
        peers: Map[HeadPeerNumber, Peer[H]],
        coils: Map[CoilPeerNumber, Coil[C]],
        sutErrors: Ref[IO, List[String]],
    )

    /** Build a fully-wired multi-peer head + coil followers. The returned resource owns everything;
      * release cancels the CL tick fibers and the error drainer.
      */
    def resource[H, C](
        inputs: Inputs,
        hooks: Hooks[H, C],
    ): Resource[IO, Harness[H, C]] =
        import inputs.*
        import inputs.config.*
        val cardanoInfo = multiNodeConfig.headConfig.cardanoInfo
        val peers       = multiNodeConfig.nodeConfigs.keys.toSeq.sortBy(p => p: Int)
        val useTC       = transportMode.useTestControl
        val log: ContraTracer[IO, Slf4jMsg] =
            Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("Harness"))

        val preSystem: IO[Unit] = for
            _ <- IO.whenA(useTC)(
                     IO.sleep(FiniteDuration(startEpochMs, TimeUnit.MILLISECONDS))
                 )
            _ <- takeoffTime match
                case None => IO.unit
                case Some(t) =>
                    IO.realTimeInstant.flatMap { now =>
                        if now.isAfter(t) then
                            IO.raiseError(
                              RuntimeException(
                                "Harness preSystem: initialization took too long " +
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
        yield ()

        val mkCardanoBackend: IO[CardanoBackend[IO]] =
            val genesisUtxos: Utxos = preinitPeerUtxosL1.values.reduce(_ ++ _)
            CardanoBackendMock.mockIO(
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

        for
            _              <- Resource.eval(preSystem)
            system         <- ActorSystem[IO](label)
            cardanoBackend <- Resource.eval(mkCardanoBackend)
            inProcessRegistry <- Resource.eval(InProcessPeerTransport.emptyRegistry)
            hubCoilRegistry <-
                if coilNodeConfigs.isEmpty then
                    Resource.pure[IO, Option[InProcessHubCoilTransport.Registry]](None)
                else
                    Resource
                        .eval(InProcessHubCoilTransport.emptyRegistry)
                        .map(Some(_))
            wsClient <- transportMode match
                case TransportMode.Direct => Resource.pure[IO, Option[WSClient[IO]]](None)
                case TransportMode.WebSocket =>
                    Resource.eval(JdkWSClient.simple[IO]).map(Some(_))
            headNetworks <- peers.toList
                .traverse { peerNum =>
                    headNetworkFor(
                      peerNum,
                      multiNodeConfig,
                      peers,
                      transportMode,
                      inProcessRegistry,
                      hubCoilRegistry,
                    ).map(peerNum -> _)
                }
                .map(_.toMap)
            _ <- transportMode match
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
                        val ws    = headNetworks(peerNum).ws.get
                        ws.wsPeerTransport.startDialers(client, peerHeadUris - ownId)
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
                      cardanoBackend,
                      multiNodeConfig,
                      backendMode,
                      peerFactory,
                      hubFactory,
                      hooks.peerTracer(peerNum),
                    ).map(peerNum -> _)
                }
                .map(_.toMap)
            coilMrms <- coilNodeConfigs
                .traverse { coilConfig =>
                    val coilNum = coilConfig.ownPeerId match
                        case PeerId.Coil(n) => n
                        case PeerId.Head(_) =>
                            throw new IllegalStateException(
                              "coil node config carries a head peer id"
                            )
                    val hubNum = coilConfig
                        .coilPeerHub(coilNum)
                        .getOrElse(
                          throw new IllegalStateException(
                            s"no hub configured for coil peer $coilNum"
                          )
                        )
                    val registry = hubCoilRegistry.getOrElse(
                      throw new IllegalStateException(
                        "coilNodeConfigs is non-empty but hubCoilRegistry was not allocated"
                      )
                    )
                    val hubBoundPort = headNetworks(hubNum).ws.map(_.boundPort)
                    buildCoilMrm(
                      coilConfig,
                      coilNum,
                      system,
                      cardanoBackend,
                      multiNodeConfig,
                      coilUplinkFor(
                        coilNum,
                        multiNodeConfig,
                        transportMode,
                        registry,
                        wsClient,
                        hubBoundPort,
                      ),
                      hooks.coilTracer(coilNum),
                    ).map(coilNum -> _)
                }
                .map(_.toMap)
            peerConnections <- Resource.eval(peerMrms.toList
                                   .traverse { case (peerNum, peerMrm) =>
                                       peerMrm.mrm.connectionsDeferred.get.map(peerNum -> _)
                                   }
                                   .map(_.toMap))
            coilConnections <- Resource.eval(coilMrms.toList
                                   .traverse { case (coilNum, coilMrm) =>
                                       coilMrm.mrm.connectionsDeferred.get.map(coilNum -> _)
                                   }
                                   .map(_.toMap))
            sutErrors <- Resource.eval(Ref[IO].of(List.empty[String]))
            _ <- startedFiber(
                     system.eventStream.take
                         .flatMap {
                             case e: ActorError if e.cause != ActorError.NoCause =>
                                 sutErrors.update(_ :+ s"[${e.logSource}] ${e.cause.getMessage}")
                             case _ => IO.unit
                         }
                         .foreverM
                 )
            _ <- peerConnections.toList.traverse_ { case (peerNum, conns) =>
                     val pollingPeriod = multiNodeConfig
                         .nodeConfigs(peerNum)
                         .nodeOperationMultisigConfig
                         .cardanoLiaisonPollingPeriod
                     startedFiber(
                       (IO.sleep(pollingPeriod) >>
                           (conns.cardanoLiaison ! CardanoLiaison.Timeout)).foreverM
                     )
                 }
            _ <- coilConnections.toList.traverse_ { case (coilNum, conns) =>
                     val pollingPeriod = coilMrms(coilNum).config
                         .nodeOperationMultisigConfig
                         .cardanoLiaisonPollingPeriod
                     startedFiber(
                       (IO.sleep(pollingPeriod) >>
                           (conns.cardanoLiaison ! CardanoLiaison.Timeout)).foreverM
                     )
                 }
            peerEntries <- Resource.eval(peerConnections.toList.traverse { case (peerNum, conns) =>
                               hooks.peerHandle(peerNum, conns).map { h =>
                                   peerNum -> Peer(
                                     connections = conns,
                                     backendStore = peerMrms(peerNum).backendStore,
                                     handle = h,
                                   )
                               }
                           })
            coilEntries <- Resource.eval(coilConnections.toList.traverse { case (coilNum, conns) =>
                               hooks.coilHandle(coilNum, conns).map { h =>
                                   coilNum -> Coil(
                                     connections = conns,
                                     backendStore = coilMrms(coilNum).backendStore,
                                     handle = h,
                                   )
                               }
                           })
        yield Harness(
          system = system,
          cardanoBackend = cardanoBackend,
          peers = peerEntries.toMap,
          coils = coilEntries.toMap,
          sutErrors = sutErrors,
        )

    // ---- private internals ----

    /** Built per-peer in Pass 1 of WS bring-up. */
    private case class WsBinding(
        wsPeerTransport: WsPeerTransport,
        boundPort: Int,
    )

    /** Per-peer transport bundle: the head-mesh transport, an optional hub-side hub-coil transport
      * (present on hubs), and (WS mode) the post-bind port carrier used by Pass 2 dialers.
      */
    private case class HeadNetwork(
        peerTransport: PeerTransport,
        hubTransport: Option[HubTransport],
        ws: Option[WsBinding],
    )

    private case class PeerMrm(
        mrm: HeadMultisigRegimeManager,
        backendStore: BackendStore[IO],
    )

    private case class CoilMrm(
        mrm: CoilMultisigRegimeManager,
        backendStore: BackendStore[IO],
        config: NodeConfig,
    )

    /** This peer's [[HeadPeerId]] — derived from its number + head-set size; `NodeConfig` does not
      * expose a head-specific id.
      */
    private def headPeerId(
        multiNodeConfig: MultiNodeConfig,
        peerNum: HeadPeerNumber,
    ): HeadPeerId =
        HeadPeerId(peerNum, multiNodeConfig.nHeadPeers)

    private def headNetworkFor(
        peerNum: HeadPeerNumber,
        multiNodeConfig: MultiNodeConfig,
        peers: Seq[HeadPeerNumber],
        transportMode: TransportMode,
        inProcessRegistry: InProcessPeerTransport.Registry,
        hubCoilRegistry: Option[InProcessHubCoilTransport.Registry],
    ): Resource[IO, HeadNetwork] =
        val ownHeadPeerId = headPeerId(multiNodeConfig, peerNum)
        val hubbedCoils   = multiNodeConfig.headConfig.hubbedCoilPeerNums(peerNum)
        transportMode match
            case TransportMode.Direct =>
                for
                    peerT <- Resource.eval(
                                 InProcessPeerTransport.create(ownHeadPeerId, inProcessRegistry)
                             )
                    hubT <-
                        if hubbedCoils.isEmpty then
                            Resource.pure[IO, Option[HubTransport]](None)
                        else
                            hubCoilRegistry match
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
                yield HeadNetwork(peerT, hubT, None)
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
                for
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
                yield HeadNetwork(
                  peerT,
                  hubTConcrete.map(h => h: HubTransport),
                  Some(WsBinding(peerT, boundPort)),
                )

    private def coilUplinkFor(
        coilNum: CoilPeerNumber,
        multiNodeConfig: MultiNodeConfig,
        transportMode: TransportMode,
        hubCoilRegistry: InProcessHubCoilTransport.Registry,
        wsClient: Option[WSClient[IO]],
        hubBoundPort: Option[Int],
    ): Resource[
      IO,
      ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => CoilTransport,
    ] = transportMode match
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
            for
                t <- Resource.eval(CoilPeerWsTransport.create(coilNum, cpwtTracer))
                _ <- t.startDialer(client, hubUri)
            yield (_: ActorContext[IO, HeadMultisigRegimeManager.Request, Any]) =>
                t: CoilTransport

    private def buildPeerMrm(
        peerNum: HeadPeerNumber,
        system: ActorSystem[IO],
        cardanoBackend: CardanoBackend[IO],
        multiNodeConfig: MultiNodeConfig,
        backendMode: BackendMode,
        peerTransport: Resource[
          IO,
          ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => PeerTransport,
        ],
        hubCoilTransport: Option[Resource[
          IO,
          ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => HubTransport,
        ]],
        callerTracer: ContraTracer[IO, HeadMultisigRegimeManagerEvent],
    ): Resource[IO, PeerMrm] =
        val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
        val slf4jMrm: ContraTracer[IO, HeadMultisigRegimeManagerEvent] =
            Slf4jTracer.sink.contramap(
              HeadMultisigRegimeManagerEventFormat.humanFormat(peerNum)
            )
        val mrmTracer = slf4jMrm |+| callerTracer
        val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        openPeerBackend(
          peerNum,
          backendMode,
          Cf.mkAll(
            headPeers = multiNodeConfig.headConfig.headPeerNums.toList,
            coilPeers = multiNodeConfig.headConfig.coilPeers.coilPeerNumbers,
            hubs = multiNodeConfig.headConfig.coilPeers.hubHeadPeerNumbers,
          ),
          persistenceTracer,
        ).flatMap { backendStore =>
            for
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
            yield PeerMrm(mrm, backendStore)
        }

    private def buildCoilMrm(
        coilConfig: NodeConfig,
        coilNum: CoilPeerNumber,
        system: ActorSystem[IO],
        cardanoBackend: CardanoBackend[IO],
        multiNodeConfig: MultiNodeConfig,
        coilTransport: Resource[
          IO,
          ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => CoilTransport,
        ],
        callerTracer: ContraTracer[IO, HeadMultisigRegimeManagerEvent],
    ): Resource[IO, CoilMrm] =
        val nHeadPeers = multiNodeConfig.nHeadPeers
        // TODO: drop this synthetic `HeadPeerNumber` offset once a coil-specific event format
        //       exists (paired with a distinct `CoilMultisigRegimeManagerEvent`). For now it's
        //       the only way to keep coil log lines distinguishable from head ones in the same
        //       run.
        val labelNum = HeadPeerNumber(nHeadPeers + coilNum.convert)
        val slf4jMrm: ContraTracer[IO, HeadMultisigRegimeManagerEvent] =
            Slf4jTracer.sink.contramap(
              HeadMultisigRegimeManagerEventFormat.humanFormat(labelNum)
            )
        val mrmTracer = slf4jMrm |+| callerTracer
        val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore.open(persistenceTracer).flatMap { backendStore =>
            for
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
            yield CoilMrm(mrm, backendStore, coilConfig)
        }

    private def openPeerBackend(
        peerNum: HeadPeerNumber,
        backendMode: BackendMode,
        cfs: List[Cf],
        tracer: ContraTracer[IO, PersistenceEvent],
    ): Resource[IO, BackendStore[IO]] =
        backendMode match
            case BackendMode.InMemory => InMemoryBackendStore.open(tracer)
            case BackendMode.RocksDb(root) =>
                val dir = root.resolve(s"peer-${peerNum: Int}")
                Resource.eval(IO.blocking(Files.createDirectories(dir))) >>
                    RocksDbBackendStore.open(dir, cfs, tracer)

    /** Long-running fiber managed as a resource: started on acquire, cancelled on release. Used for
      * the actor-system error drainer and per-peer / per-coil CardanoLiaison tick loops.
      */
    private def startedFiber(action: IO[Nothing]): Resource[IO, Unit] =
        Resource.make(action.start)(_.cancel).void
