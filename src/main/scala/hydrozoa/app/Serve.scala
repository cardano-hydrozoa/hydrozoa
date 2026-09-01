package hydrozoa.app

import cats.Monoid
import cats.effect.{ExitCode, IO, Resource}
import cats.syntax.apply.*
import cats.syntax.contravariant.*
import cats.syntax.semigroup.*
import com.bloxbean.cardano.client.util.HexUtil.encodeHexString
import com.comcast.ip4s.{Host, Port}
import com.monovore.decline.{Command, Opts}
import com.suprnation.actor.{ActorContext, ActorSystem}
import hydrozoa.BuildInfo
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.L2LedgerKind
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.{CoilPeerWsTransport, CoilPeerWsTransportEventFormat, CoilTransport, HubTransport, HubWsTransport, NodeWsServer, WsPeerTransport}
import hydrozoa.multisig.ledger.eutxol2.store.RocksDbL2Store
import hydrozoa.multisig.ledger.eutxol2.{EutxoL2Ledger, EutxoL2Screener}
import hydrozoa.multisig.ledger.l2.{EutxoL2LedgerReader, L2Ledger, L2Screener}
import hydrozoa.multisig.ledger.remote.{RemoteL2Ledger, RemoteL2LedgerEventFormat, RemoteL2Screener, RemoteL2ScreenerEventFormat}
import hydrozoa.multisig.metrics.PeerMetrics
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import hydrozoa.multisig.persistence.{Cf, ConsensusStoreReader, Persistence, PersistenceEventFormat}
import hydrozoa.multisig.server.{HydrozoaHttpEvent, HydrozoaHttpEventFormat, HydrozoaServer}
import hydrozoa.multisig.{CoilMultisigRegimeManager, CoilMultisigRegimeManagerEventFormat, CoilRegimeManagerEvent, HeadMultisigRegimeManager, HeadMultisigRegimeManagerEventFormat, HeadRegimeManagerEvent, MrmTracers}
import java.nio.file.Path
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.websocket.WebSocketBuilder2
import scala.concurrent.duration.*

/** The head-node server: the `serve` subcommand of the `hydrozoa` CLI.
  *
  * Usage:
  * {{{
  *   hydrozoa serve <head-config.json> <peer-private.json>
  * }}}
  *
  * Both files are produced by the bootstrap tools (`hydrozoa build-head-config` et al). All
  * settings the node needs at runtime — Blockfrost API key, wallet keys, bind host/port,
  * remote-ledger WS URI, HTTP admin credentials — live in those files; the process reads no
  * environment variables.
  */
object Serve {

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa.app.Serve"))

    private val headConfigPathArg: Opts[Path] =
        Opts.argument[String]("head-config.json").map(Path.of(_))

    private val privateConfigPathArg: Opts[Path] =
        Opts.argument[String]("peer-private.json").map(Path.of(_))

    /** The `serve` subcommand: run a head node from a generated config. */
    lazy val command: Command[IO[ExitCode]] =
        Command(
          name = "serve",
          header = "Run a Hydrozoa head node from a generated config"
        )((headConfigPathArg, privateConfigPathArg).mapN((h, p) => runNode(h, p)))

    /** Run a Hydrozoa head node from a loaded config.
      *
      * @param dataDir
      *   filesystem location for the per-peer RocksDB persistence store; the actual store path is
      *   `${dataDir}/peer-${ownPeerLabel}/rocksdb`. Defaults to `.hydrozoa-data` (relative to cwd)
      *   for the CLI; tests should pass a temp dir.
      * @param httpExtraTracer
      *   additional observer fanned out alongside the slf4j HTTP server tracer (combined via the
      *   `ContraTracer` monoid). Tests use this to observe `ServerStarted` (the bind milestone)
      *   without disturbing logging.
      * @param backendOverride
      *   if `Some`, used in place of the Blockfrost backend the decoder would otherwise build. Lets
      *   tests inject a mock so script-reference UTxO resolution doesn't hit the network.
      */
    def runNode(
        headConfigPath: Path,
        privateConfigPath: Path,
        dataDir: Path = Path.of(".hydrozoa-data"),
        httpExtraTracer: ContraTracer[IO, HydrozoaHttpEvent] =
            Monoid[ContraTracer[IO, HydrozoaHttpEvent]].empty,
        backendOverride: Option[CardanoBackend[IO]] = None,
    ): IO[ExitCode] = {
        val setupIO = for {
            _ <- log.info(
              s"Hydrozoa ${BuildInfo.version} " +
                  s"(git ${BuildInfo.gitCommit}, built ${BuildInfo.builtAtString})"
            )
            _ <- log.info("Starting Hydrozoa node...")
            _ <- log.info(s"Loading head config from $headConfigPath")
            _ <- log.info(s"Loading peer private config from $privateConfigPath")
            loaded <- NodeConfig.load(
              headConfigPath,
              privateConfigPath,
              backendOverride
            )
            (nodeConfig, backend) = loaded
            _ <- log.info(s"headAddress: ${nodeConfig.headMultisigAddress.toBech32.get}")
            _ <- log.info(s"initTx hash: ${nodeConfig.initializationTx.tx.id}")
            _ <- log.info(
              s"initTx: ${encodeHexString(nodeConfig.initializationTx.tx.toCbor)}"
            )
        } yield (backend, nodeConfig)

        val resource = for {
            result <- Resource.eval(setupIO)
            (backend, nodeConfig) = result

            // Peer stats registry (docs/spec/peer-stats-endpoint.md): created once, threaded into the
            // instrumented actors and the HTTP server, with its 1 Hz sampler running for the node's
            // lifetime. In-memory only — counters reset on restart.
            metrics <- Resource.eval(
              IO.realTime.map(t =>
                  // Track only remote head peers: this peer never sends requests to itself, so
                  // its own `peerRequests` entry would be a constant zero (see PeerMetrics.create).
                  val ownHeadNum = nodeConfig.ownPeerId match {
                      case PeerId.Head(n) => Some(n.convert)
                      case PeerId.Coil(_) => None
                  }
                  val remotePeerNums = nodeConfig.headConfig.headPeerNums.toList
                      .map(_.convert)
                      .filterNot(ownHeadNum.contains)
                      .toVector
                  PeerMetrics.create(t.toMillis, remotePeerNums)
              )
            )
            _ <- metrics.sampler().background

            // Select the L2 ledger the peers agreed on (HeadParameters.l2Ledger): the built-in
            // in-process EUTXO ledger, or a remote black box reached over this node's
            // `remoteLedgerUri`. Only the EUTXO ledger is also an EutxoL2LedgerReader, so only it
            // yields a reader for the server's L2-query endpoints (a remote node hands it None).
            l2 <- mkL2Ledger(nodeConfig, dataDir)
            (l2Ledger, l2Screener, l2QueryReader) = l2

            // Per-peer persistence store. Default path; later milestones will surface this
            // through NodeConfig (P1 skeleton; see design §7). Open the RocksDB-backed
            // BackendStore (byte-level primitive), then wrap it in the typed Persistence the
            // actor topology consumes.
            persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
            backendStore <- RocksDbBackendStore.open(
              dataDir.resolve(s"peer-${nodeConfig.ownPeerLabel}/rocksdb"),
              Cf.mkAll(
                headPeers = nodeConfig.headConfig.headPeerNums.toList,
                coilPeers = nodeConfig.headConfig.coilPeers.coilPeerNumbers,
                hubs = nodeConfig.headConfig.coilPeers.hubHeadPeerNumbers
              ),
              persistenceTracer,
            )
            persistence <- Resource.eval {
                given CardanoNetwork.Section = nodeConfig
                Persistence.fromBackend(backendStore, persistenceTracer)
            }

            system <- ActorSystem[IO]("Hydrozoa Demo")

            wsClient <- Resource.eval(JdkWSClient.simple[IO])

            nodeRun <- nodeConfig.ownPeerId match {
                case PeerId.Head(ownHeadNum) =>
                    val headTracer = Slf4jTracer.sink.contramap(
                      HeadMultisigRegimeManagerEventFormat.humanFormat(ownHeadNum)
                    )
                    buildHeadNode(
                      nodeConfig,
                      backend,
                      l2Ledger,
                      l2Screener,
                      l2QueryReader,
                      persistence,
                      metrics,
                      headTracer,
                      wsClient,
                      ownHeadNum,
                    )
                case PeerId.Coil(ownCoilNum) =>
                    // Synthetic label so coil log lines stay distinguishable from head ones.
                    val labelNum = HeadPeerNumber(
                      nodeConfig.headConfig.headPeerNums.size + ownCoilNum.convert
                    )
                    val coilTracer = Slf4jTracer.sink.contramap(
                      CoilMultisigRegimeManagerEventFormat.humanFormat(labelNum, ownCoilNum)
                    )
                    buildCoilNode(
                      nodeConfig,
                      backend,
                      l2Ledger,
                      persistence,
                      metrics,
                      coilTracer,
                      wsClient,
                      ownCoilNum,
                    )
            }
            // Read-only consensus-store view behind the /head/blocks queries.
            consensusReader = ConsensusStoreReader.fromPersistence(persistence)(using
              nodeConfig.headConfig
            )
        } yield (nodeConfig, system, nodeRun, consensusReader, metrics)

        resource.use { case (nodeConfig, system, nodeRun, consensusReader, metrics) =>
            nodeRun match {
                case NodeRun.HeadNode(mrm, l2QueryReader) =>
                    runHeadNode(
                      nodeConfig,
                      system,
                      mrm,
                      consensusReader,
                      l2QueryReader,
                      metrics,
                      httpExtraTracer
                    )
                case NodeRun.CoilNode(mrm) =>
                    runCoilNode(
                      nodeConfig,
                      system,
                      mrm,
                      consensusReader,
                      metrics,
                      httpExtraTracer
                    )
            }
        }
    }

    /** Instantiate the head-agreed L2 ledger and, when it is the built-in EUTXO ledger, the
      * read-only view the user-facing server exposes over the `GET /l2/cardano-eutxo/...`
      * endpoints. A node wired to a remote ledger has no such reader, so the server is handed
      * `None` and mounts no L2-query endpoints. The EUTXO ledger owns its own RocksDB persistence,
      * separate from the consensus store.
      */
    /** Where this node's peer websocket server listens.
      *
      * Normally that is the address the shared head config advertises for this peer, so there is
      * one source of truth and no way for the two to drift. The override exists for the case where
      * they genuinely differ: with a TLS-terminating proxy in front, peers dial a public name that
      * resolves to the proxy, and no host can bind that name. The node then advertises the public
      * address to the head and listens wherever the proxy can reach it.
      *
      * Each half overrides independently -- moving the local port while keeping the advertised one
      * is a normal thing to want, and requiring both would make that awkward.
      */
    private[hydrozoa] def peerBindAddress(
        peerBindHost: Option[String],
        peerBindPort: Option[String],
        advertised: Uri,
        ownHeadNum: HeadPeerNumber
    ): (Host, Port) = {
        val host = peerBindHost match {
            case Some(h) =>
                Host.fromString(h)
                    .getOrElse(throw IllegalArgumentException(s"peerBindHost is not a host: $h"))
            case None =>
                advertised.host
                    .flatMap(h => Host.fromString(h.value))
                    .getOrElse(
                      throw IllegalArgumentException(
                        s"own head peer $ownHeadNum webSocketAddress has no valid host: $advertised"
                      )
                    )
        }
        val port = peerBindPort match {
            case Some(p) =>
                p.toIntOption
                    .flatMap(Port.fromInt)
                    .getOrElse(throw IllegalArgumentException(s"peerBindPort is not a port: $p"))
            case None =>
                advertised.port
                    .flatMap(Port.fromInt)
                    .getOrElse(
                      throw IllegalArgumentException(
                        s"own head peer $ownHeadNum webSocketAddress has no valid port: $advertised"
                      )
                    )
        }
        (host, port)
    }

    private def mkL2Ledger(
        nodeConfig: NodeConfig,
        dataDir: Path,
    ): Resource[IO, (L2Ledger[IO], L2Screener[IO], Option[EutxoL2LedgerReader[IO]])] =
        nodeConfig.headConfig.l2Ledger match {
            case L2LedgerKind.CardanoEutxo =>
                for {
                    _ <- Resource.eval(log.info("L2 ledger: built-in cardano-eutxo"))
                    store <- RocksDbL2Store.open(
                      dataDir.resolve(s"peer-${nodeConfig.ownPeerLabel}/l2-rocksdb")
                    )
                    ledger <- Resource.eval(EutxoL2Ledger(nodeConfig, store))
                } yield (ledger, EutxoL2Screener(nodeConfig), Some(ledger))
            case L2LedgerKind.AnyRemote =>
                val tracer = Slf4jTracer.sink.contramap(RemoteL2LedgerEventFormat.humanFormat)
                val wsUri = nodeConfig.remoteLedgerUri.getOrElse(
                  throw new IllegalArgumentException(
                    "l2Ledger=any-remote requires remoteLedgerUri in the node's private config"
                  )
                )
                for {
                    _ <- Resource.eval(log.info(s"L2 ledger: remote at $wsUri"))
                    ledger <- RemoteL2Ledger.create(
                      wsUri = wsUri,
                      config = nodeConfig,
                      tracer = tracer,
                    )
                    screener <- mkRemoteScreener(nodeConfig)
                } yield (ledger, screener, Option.empty[EutxoL2LedgerReader[IO]])
        }

    /** The screener for a remote-ledger node: the stateless check every request must pass before it
      * is assigned a RequestId. With `remoteScreenerUri` configured, deposits are screened by the
      * remote ledger's screening endpoint, reached over a dedicated ember client so screening never
      * shares the mutation transport. Without it, screening is a passthrough — every request is
      * accepted here and checked only at submission.
      */
    private def mkRemoteScreener(nodeConfig: NodeConfig): Resource[IO, L2Screener[IO]] =
        nodeConfig.remoteScreenerUri match {
            case None =>
                Resource
                    .eval(log.info("L2 screener: passthrough (no remoteScreenerUri)"))
                    .map(_ => RemoteL2Screener.passthrough)
            case Some(uri) =>
                for {
                    parsedUri <- Resource.eval(
                      IO.fromEither(
                        Uri.fromString(uri)
                            .left
                            .map(e =>
                                new IllegalArgumentException(s"invalid remoteScreenerUri: $e")
                            )
                      )
                    )
                    _ <- Resource.eval(log.info(s"L2 screener: remote at $uri"))
                    // A short timeout, not ember's 45s default: screening is a fail-open
                    // advisory gate on the request path, so a hung screener should cost a
                    // request a few seconds at most before it proceeds unscreened.
                    client <- EmberClientBuilder.default[IO].withTimeout(5.seconds).build
                } yield RemoteL2Screener(
                  client,
                  parsedUri,
                  Slf4jTracer.sink.contramap(RemoteL2ScreenerEventFormat.humanFormat),
                )
        }

    /** Build the head-node transports (mesh + optional hub-coil), bind one shared `NodeWsServer`,
      * start dialers, and allocate the [[HeadMultisigRegimeManager]].
      */
    private def buildHeadNode(
        nodeConfig: NodeConfig,
        backend: CardanoBackend[IO],
        l2Ledger: L2Ledger[IO],
        l2Screener: L2Screener[IO],
        l2QueryReader: Option[EutxoL2LedgerReader[IO]],
        persistence: Persistence[IO],
        metrics: PeerMetrics,
        mrmTracer: ContraTracer[IO, HeadRegimeManagerEvent],
        wsClient: org.http4s.client.websocket.WSClient[IO],
        ownHeadNum: HeadPeerNumber,
    ): Resource[IO, NodeRun.HeadNode] = {
        given CardanoNetwork.Section = nodeConfig
        val ownHeadPeerId = nodeConfig.headPeerIds.find(_.peerNum == ownHeadNum).get
        // The inter-peer transport server binds where the shared head config advertises this peer,
        // so bind address == the address other peers dial (single source of truth). The user-facing
        // HTTP server uses the private httpHost/httpPort instead.
        //
        // The two come apart whenever something terminates the connection in front of this node --
        // a TLS proxy demanding a client certificate, say. Then peers dial a public name this host
        // cannot bind, and `peerBindHost`/`peerBindPort` in the node's private config say where to
        // listen instead. Advertised address stays shared and agreed; bind address stays local.
        val ownWsAddress = nodeConfig.headConfig.headPeers.headPeerData
            .lookup(ownHeadNum)
            .map(_.webSocketAddress)
            .getOrElse(
              throw new IllegalStateException(
                s"no webSocketAddress configured for own head peer $ownHeadNum"
              )
            )
        val (bindHost, bindPort) = peerBindAddress(
          nodeConfig.peerBindHost,
          nodeConfig.peerBindPort,
          ownWsAddress,
          ownHeadNum
        )
        val remoteHeadUris: Map[HeadPeerId, Uri] = nodeConfig.headPeerIds
            .filterNot(_.peerNum == ownHeadNum)
            .toList
            .flatMap { pid =>
                nodeConfig.headConfig.headPeers.headPeerData
                    .lookup(pid.peerNum)
                    .map(hpd => pid -> (hpd.webSocketAddress / "head"))
            }
            .toMap
        val hubbedCoils = nodeConfig.hubbedCoilPeerNums(ownHeadNum)
        val tracers = MrmTracers.fromRoot(mrmTracer)
        for {
            peerT <- Resource.eval(
              WsPeerTransport.create(
                ownHeadPeerId,
                remoteHeadUris.keys.toList,
                tracers.peerTransport
              )
            )
            hubT: Option[HubWsTransport] <-
                if hubbedCoils.isEmpty then Resource.pure[IO, Option[HubWsTransport]](None)
                else
                    Resource
                        .eval(HubWsTransport.create(hubbedCoils, tracers.hubWsTransport))
                        .map(Some(_))
            meshRoute = (wsb: WebSocketBuilder2[IO]) => peerT.routes(wsb)
            hubRoutes =
                hubT.toList.map(h => (wsb: WebSocketBuilder2[IO]) => h.routes(wsb))
            _ <- NodeWsServer.resource(
              bindHost,
              bindPort,
              meshRoute :: hubRoutes,
              tracers.nodeWsServer
            )
            _ <- peerT.startDialers(wsClient, remoteHeadUris)
            peerFactory: Resource[
              IO,
              ActorContext[
                IO,
                HeadMultisigRegimeManager.Request,
                Any
              ] => hydrozoa.multisig.consensus.transport.PeerTransport
            ] =
                Resource.pure((_: ActorContext[IO, HeadMultisigRegimeManager.Request, Any]) =>
                    peerT
                )
            hubFactory: Option[Resource[
              IO,
              ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => HubTransport
            ]] = hubT.map { h =>
                Resource.pure((_: ActorContext[IO, HeadMultisigRegimeManager.Request, Any]) =>
                    h: HubTransport
                )
            }
            mrm <- HeadMultisigRegimeManager.resource(
              nodeConfig,
              backend,
              l2Ledger,
              l2Screener,
              persistence,
              metrics,
              mrmTracer,
              peerFactory,
              hubFactory,
            )
        } yield NodeRun.HeadNode(mrm, l2QueryReader)
    }

    /** Build the coil-node uplink dialer (no inbound WebSocket server) and allocate the
      * [[CoilMultisigRegimeManager]]. The coil's HTTP surface is bound separately, in
      * `runCoilNode`.
      */
    private def buildCoilNode(
        nodeConfig: NodeConfig,
        backend: CardanoBackend[IO],
        l2Ledger: L2Ledger[IO],
        persistence: Persistence[IO],
        metrics: PeerMetrics,
        mrmTracer: ContraTracer[IO, CoilRegimeManagerEvent],
        wsClient: org.http4s.client.websocket.WSClient[IO],
        ownCoilNum: CoilPeerNumber,
    ): Resource[IO, NodeRun.CoilNode] = {
        given CardanoNetwork.Section = nodeConfig
        val hubNum = nodeConfig
            .coilPeerHub(ownCoilNum)
            .getOrElse(
              throw new IllegalStateException(s"no hub configured for coil peer $ownCoilNum")
            )
        val hubUri = nodeConfig.headConfig.headPeers.headPeerData
            .lookup(hubNum)
            .map(hpd => hpd.webSocketAddress / "hub")
            .getOrElse(
              throw new IllegalStateException(
                s"no webSocketAddress configured for hub head peer $hubNum"
              )
            )
        val cpwtTracer =
            Slf4jTracer.sink.contramap(CoilPeerWsTransportEventFormat.humanFormat(ownCoilNum))
        for {
            t <- Resource.eval(CoilPeerWsTransport.create(ownCoilNum, cpwtTracer))
            _ <- t.startDialer(wsClient, hubUri)
            coilFactory: Resource[
              IO,
              ActorContext[IO, HeadMultisigRegimeManager.Request, Any] => CoilTransport
            ] =
                Resource.pure((_: ActorContext[IO, HeadMultisigRegimeManager.Request, Any]) =>
                    t: CoilTransport
                )
            mrm <- CoilMultisigRegimeManager.resource(
              nodeConfig,
              backend,
              l2Ledger,
              persistence,
              metrics,
              mrmTracer,
              coilFactory,
            )
        } yield NodeRun.CoilNode(mrm)
    }

    /** The HTTP bind address and admin credentials, from the node's own private config. Shared by
      * both roles: a coil serves the same server minus the mutating routes, so it reads the same
      * fields.
      */
    private def httpServerConfig(nodeConfig: NodeConfig): HydrozoaServer.Config = {
        val httpHost = Host
            .fromString(nodeConfig.httpHost)
            .getOrElse(
              throw new IllegalArgumentException(
                s"Invalid httpHost in node config: ${nodeConfig.httpHost}"
              )
            )
        val httpPort = Port
            .fromString(nodeConfig.httpPort)
            .getOrElse(
              throw new IllegalArgumentException(
                s"Invalid httpPort in node config: ${nodeConfig.httpPort}"
              )
            )
        HydrozoaServer.Config(
          host = httpHost,
          port = httpPort,
          adminUsername = nodeConfig.adminUsername,
          adminPassword = nodeConfig.adminPassword
        )
    }

    private def runHeadNode(
        nodeConfig: NodeConfig,
        system: ActorSystem[IO],
        mrm: HeadMultisigRegimeManager,
        consensusReader: ConsensusStoreReader[IO],
        l2QueryReader: Option[EutxoL2LedgerReader[IO]],
        metrics: PeerMetrics,
        httpExtraTracer: ContraTracer[IO, HydrozoaHttpEvent],
    ): IO[ExitCode] =
        for {
            _ <- system.actorOf(mrm, "HeadMultisigRegimeManager")
            _ <- log.info("Hydrozoa node started successfully")

            // The HTTP server needs the RequestSequencer, which only exists once connections
            // resolve, so wait for them before binding.
            connections <- mrm.connectionsDeferred.get.flatMap(IO.fromEither)
            _ <- log.info("Starting HTTP server...")

            // `surround`, not `start.void`: the server is bound for exactly as long as the node
            // waits, and its finalizer runs when the actor system terminates. `runCoilNode` has
            // the same shape and spells out what the old one leaked.
            _ <- HydrozoaServer
                .create(
                  // Always present on a head; the `Option` exists for the coil.
                  Some(
                    connections.requestSequencer.getOrElse(
                      sys.error("RequestSequencer required on head peers")
                    )
                  ),
                  connections.blockWeaver,
                  mrm.nodeStatus.get,
                  consensusReader,
                  // Some(reader) for a cardano-eutxo node (mounts GET /l2/cardano-eutxo/...); None for
                  // a remote-ledger node, which serves no L2-query endpoints.
                  l2QueryReader,
                  nodeConfig.headConfig,
                  httpServerConfig(nodeConfig),
                  metrics,
                  Slf4jTracer.sink
                      .contramap(HydrozoaHttpEventFormat.humanFormat) |+| httpExtraTracer,
                )
                .surround(system.waitForTermination)

            exit <- abnormalTermination
        } yield exit

    /** A coil peer runs the same HTTP server as a head, minus the mutating routes: it passes `None`
      * for the request sequencer, which is what removes them (see
      * [[hydrozoa.multisig.server.HydrozoaRoutes]]).
      *
      * Serving a coil at all is for observability. It runs its own `StackComposer`, and its
      * hard-ack gates the head's next stack — so without `/head/stats` and `/metrics`, a coil
      * wedged on its single-flight gate looks exactly like an idle one.
      */
    private def runCoilNode(
        nodeConfig: NodeConfig,
        system: ActorSystem[IO],
        mrm: CoilMultisigRegimeManager,
        consensusReader: ConsensusStoreReader[IO],
        metrics: PeerMetrics,
        httpExtraTracer: ContraTracer[IO, HydrozoaHttpEvent],
    ): IO[ExitCode] =
        for {
            _ <- system.actorOf(mrm, "CoilMultisigRegimeManager")
            _ <- log.info("Hydrozoa coil node started successfully")

            connections <- mrm.connectionsDeferred.get.flatMap(IO.fromEither)
            _ <- log.info("Starting HTTP server (coil: read-only surface)...")

            // `surround` binds the server for the node's lifetime and releases it when the actor
            // system terminates. `use(_ => IO.never).start.void` -- what this was, on both roles --
            // dropped the fiber handle, so nothing could cancel it and the finalizer never ran: the
            // port stayed bound and answering after the node was dead, and a bind failure could not
            // fail the node because no one joined the fiber's outcome.
            _ <- HydrozoaServer
                .create(
                  // None on a coil — this is what removes the mutating routes.
                  connections.requestSequencer,
                  connections.blockWeaver,
                  mrm.nodeStatus.get,
                  consensusReader,
                  // A coil always runs a remote ledger, so no L2-query endpoints.
                  None,
                  nodeConfig.headConfig,
                  httpServerConfig(nodeConfig),
                  metrics,
                  Slf4jTracer.sink
                      .contramap(HydrozoaHttpEventFormat.humanFormat) |+| httpExtraTracer,
                )
                .surround(system.waitForTermination)

            exit <- abnormalTermination
        } yield exit

    /** Nothing here shuts the actor system down deliberately, and an interrupted `serve` is
      * cancelled rather than resumed past `waitForTermination`. So reaching this point means an
      * actor escalated to the guardian and took the system with it: exit non-zero, or a process
      * supervisor will treat a crashed node as a clean stop. cats-actors 2.1.0 clears the cause
      * before it can be re-raised, which is why this can only point at the log.
      */
    private def abnormalTermination: IO[ExitCode] =
        log.info(
          "Actor system terminated: an actor escalated to the guardian. " +
              "See the [EventBus] error and stack trace above for the cause."
        ).as(ExitCode.Error)

    private sealed trait NodeRun
    private object NodeRun {
        final case class HeadNode(
            mrm: HeadMultisigRegimeManager,
            l2QueryReader: Option[EutxoL2LedgerReader[IO]],
        ) extends NodeRun
        final case class CoilNode(mrm: CoilMultisigRegimeManager) extends NodeRun
    }
}
