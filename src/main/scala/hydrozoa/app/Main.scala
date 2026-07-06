package hydrozoa.app

import cats.Monoid
import cats.effect.{ExitCode, IO, Resource}
import cats.syntax.apply.*
import cats.syntax.contravariant.*
import cats.syntax.semigroup.*
import com.bloxbean.cardano.client.util.HexUtil.encodeHexString
import com.comcast.ip4s.{Host, Port}
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import com.suprnation.actor.{ActorContext, ActorSystem}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.{CoilPeerWsTransport, CoilPeerWsTransportEventFormat, CoilTransport, HubTransport, HubWsTransport, NodeWsServer, WsPeerTransport}
import hydrozoa.multisig.ledger.remote.{RemoteL2Ledger, RemoteL2LedgerEventFormat}
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import hydrozoa.multisig.persistence.{Cf, Persistence, PersistenceEventFormat}
import hydrozoa.multisig.server.{HydrozoaHttpEvent, HydrozoaHttpEventFormat, HydrozoaServer}
import hydrozoa.multisig.{CoilMultisigRegimeManager, CoilMultisigRegimeManagerEventFormat, CoilRegimeManagerEvent, HeadMultisigRegimeManager, HeadMultisigRegimeManagerEventFormat, HeadRegimeManagerEvent, MrmTracers}
import java.nio.file.Path
import org.http4s.Uri
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.websocket.WebSocketBuilder2

/** Hydrozoa application entry point.
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.app.Main <head-config.json> <peer-private.json>"
  * }}}
  *
  * Both files are produced by `GenerateSampleConfig` (or, eventually, a real bootstrap tool). All
  * settings the node needs at runtime — Blockfrost API key, wallet keys, bind host/port, Sugar Rush
  * WS URI, HTTP admin credentials — live in those files; the process reads no environment
  * variables.
  */
object Main
    extends CommandIOApp(
      name = "hydrozoa",
      header = "Run a Hydrozoa head node from a generated config"
    ) {

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa.app.Main"))

    private val headConfigPathArg: Opts[Path] =
        Opts.argument[String]("head-config.json").map(Path.of(_))

    private val privateConfigPathArg: Opts[Path] =
        Opts.argument[String]("peer-private.json").map(Path.of(_))

    override def main: Opts[IO[ExitCode]] =
        (headConfigPathArg, privateConfigPathArg).mapN((h, p) => runNode(h, p))

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

            _ <- Resource.eval(log.info(s"Connecting to L2 ledger at ${nodeConfig.sugarRushUri}"))
            remoteL2LedgerTracer = Slf4jTracer.sink
                .contramap(RemoteL2LedgerEventFormat.humanFormat)
            remoteL2Ledger <- Resource.eval(
              RemoteL2Ledger.create(
                wsUri = nodeConfig.sugarRushUri,
                config = nodeConfig,
                tracer = remoteL2LedgerTracer,
              )
            )

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
                      remoteL2Ledger,
                      persistence,
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
                      remoteL2Ledger,
                      persistence,
                      coilTracer,
                      wsClient,
                      ownCoilNum,
                    )
            }
        } yield (nodeConfig, system, nodeRun)

        resource.use { case (nodeConfig, system, nodeRun) =>
            nodeRun match {
                case NodeRun.HeadNode(mrm) =>
                    runHeadNode(nodeConfig, system, mrm, httpExtraTracer)
                case NodeRun.CoilNode(mrm) =>
                    runCoilNode(system, mrm)
            }
        }
    }

    /** Build the head-node transports (mesh + optional hub-coil), bind one shared `NodeWsServer`,
      * start dialers, and allocate the [[HeadMultisigRegimeManager]].
      */
    private def buildHeadNode(
        nodeConfig: NodeConfig,
        backend: CardanoBackend[IO],
        remoteL2Ledger: RemoteL2Ledger,
        persistence: Persistence[IO],
        mrmTracer: ContraTracer[IO, HeadRegimeManagerEvent],
        wsClient: org.http4s.client.websocket.WSClient[IO],
        ownHeadNum: HeadPeerNumber,
    ): Resource[IO, NodeRun.HeadNode] = {
        given CardanoNetwork.Section = nodeConfig
        val ownHeadPeerId = nodeConfig.headPeerIds.find(_.peerNum == ownHeadNum).get
        // The inter-peer transport server binds where the shared head config advertises this peer,
        // so bind address == the address other peers dial (single source of truth). The user-facing
        // HTTP server uses the private httpHost/httpPort instead.
        val ownWsAddress = nodeConfig.headConfig.headPeers.headPeerData
            .lookup(ownHeadNum)
            .map(_.webSocketAddress)
            .getOrElse(
              throw new IllegalStateException(
                s"no webSocketAddress configured for own head peer $ownHeadNum"
              )
            )
        val bindHost = ownWsAddress.host
            .flatMap(h => Host.fromString(h.value))
            .getOrElse(
              throw new IllegalArgumentException(
                s"own head peer $ownHeadNum webSocketAddress has no valid host: $ownWsAddress"
              )
            )
        val bindPort = ownWsAddress.port
            .flatMap(Port.fromInt)
            .getOrElse(
              throw new IllegalArgumentException(
                s"own head peer $ownHeadNum webSocketAddress has no valid port: $ownWsAddress"
              )
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
              remoteL2Ledger,
              persistence,
              mrmTracer,
              peerFactory,
              hubFactory,
            )
        } yield NodeRun.HeadNode(mrm)
    }

    /** Build the coil-node uplink dialer (no inbound server) and allocate the
      * [[CoilMultisigRegimeManager]]. A coil peer runs no user-facing HTTP server.
      */
    private def buildCoilNode(
        nodeConfig: NodeConfig,
        backend: CardanoBackend[IO],
        remoteL2Ledger: RemoteL2Ledger,
        persistence: Persistence[IO],
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
              remoteL2Ledger,
              persistence,
              mrmTracer,
              coilFactory,
            )
        } yield NodeRun.CoilNode(mrm)
    }

    private def runHeadNode(
        nodeConfig: NodeConfig,
        system: ActorSystem[IO],
        mrm: HeadMultisigRegimeManager,
        httpExtraTracer: ContraTracer[IO, HydrozoaHttpEvent],
    ): IO[ExitCode] =
        for {
            _ <- system.actorOf(mrm, "HeadMultisigRegimeManager")
            _ <- log.info("Hydrozoa node started successfully")

            // Start HTTP server once RequestSequencer is available
            _ <- mrm.connectionsDeferred.get.flatMap { connections =>
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
                val serverConfig = HydrozoaServer.Config(
                  host = httpHost,
                  port = httpPort,
                  adminUsername = nodeConfig.adminUsername,
                  adminPassword = nodeConfig.adminPassword
                )
                val httpTracer = Slf4jTracer.sink
                    .contramap(HydrozoaHttpEventFormat.humanFormat) |+| httpExtraTracer
                log.info("Starting HTTP server...") *>
                    HydrozoaServer
                        .create(
                          connections.requestSequencer.getOrElse(
                            sys.error("RequestSequencer required on head peers")
                          ),
                          connections.blockWeaver,
                          nodeConfig.headConfig,
                          serverConfig,
                          httpTracer,
                        )
                        .use(_ => IO.never)
                        .start
                        .void
            }

            _ <- system.waitForTermination
        } yield ExitCode.Success

    private def runCoilNode(
        system: ActorSystem[IO],
        mrm: CoilMultisigRegimeManager,
    ): IO[ExitCode] =
        for {
            _ <- system.actorOf(mrm, "CoilMultisigRegimeManager")
            _ <- log.info("Hydrozoa coil node started successfully")
            _ <- system.waitForTermination
        } yield ExitCode.Success

    private sealed trait NodeRun
    private object NodeRun {
        final case class HeadNode(mrm: HeadMultisigRegimeManager) extends NodeRun
        final case class CoilNode(mrm: CoilMultisigRegimeManager) extends NodeRun
    }
}
