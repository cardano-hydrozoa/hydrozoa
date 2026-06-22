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
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.{NodeWsServer, PeerTransport}
import hydrozoa.multisig.ledger.remote.{RemoteL2Ledger, RemoteL2LedgerEventFormat}
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import hydrozoa.multisig.persistence.{Cf, Persistence, PersistenceEventFormat}
import hydrozoa.multisig.server.{HydrozoaHttpEvent, HydrozoaHttpEventFormat, HydrozoaServer}
import hydrozoa.multisig.{HeadMultisigRegimeManager, HeadMultisigRegimeManagerEvent, HeadMultisigRegimeManagerEventFormat}
import java.nio.file.Path
import org.http4s.Uri
import org.http4s.jdkhttpclient.JdkWSClient

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
            loaded <- Bootstrap.loadNodeConfig(
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

            mrmTracer = Slf4jTracer.sink.contramap(
              HeadMultisigRegimeManagerEventFormat.humanFormat(
                HeadPeerNumber(nodeConfig.ownPeerIndex)
              )
            )
            bindHost = Host
                .fromString(nodeConfig.hydrozoaHost)
                .getOrElse(
                  throw new IllegalArgumentException(
                    s"Invalid hydrozoaHost in node config: ${nodeConfig.hydrozoaHost}"
                  )
                )
            bindPort = Port
                .fromString(nodeConfig.hydrozoaPort)
                .getOrElse(
                  throw new IllegalArgumentException(
                    s"Invalid hydrozoaPort in node config: ${nodeConfig.hydrozoaPort}"
                  )
                )

            // Build the WS-backed head-mesh transport: open one shared dialer client, the
            // PeerTransport instance, bind a NodeWsServer for its inbound route, and start the
            // outbound dialers. The MRM's resource() owns this Resource's lifecycle and
            // ignores the ActorContext (the WS variant doesn't need it to allocate).
            ownHeadPeerId = nodeConfig.ownPeerId match {
                case PeerId.Head(n) => nodeConfig.headPeerIds.find(_.peerNum == n).get
                case PeerId.Coil(_) =>
                    throw new IllegalStateException(
                      "HeadMultisigRegimeManager runs only on head peers"
                    )
            }
            remoteHeadUris: Map[HeadPeerId, Uri] = nodeConfig.headPeerIds
                .filterNot(_.peerNum == ownHeadPeerId.peerNum)
                .toList
                .flatMap { pid =>
                    nodeConfig.headConfig.headPeers.headPeerData
                        .lookup(pid.peerNum)
                        .map(hpd => pid -> (hpd.webSocketAddress / "head"))
                }
                .toMap
            pwtTracer = mrmTracer.contramap(HeadMultisigRegimeManagerEvent.PT.apply)
            nwsTracer = mrmTracer.contramap(HeadMultisigRegimeManagerEvent.NWS.apply)
            wsPeerTransport = {
                given CardanoNetwork.Section = nodeConfig
                for {
                    wsClient <- Resource.eval(JdkWSClient.simple[IO])
                    transport <- Resource.eval(
                      PeerTransport.create(ownHeadPeerId, remoteHeadUris, pwtTracer)
                    )
                    _ <- NodeWsServer.resource(
                      bindHost,
                      bindPort,
                      List(transport.routes),
                      nwsTracer
                    )
                    _ <- transport.startDialers(wsClient)
                } yield (_: ActorContext[IO, HeadMultisigRegimeManager.Request, Any]) => transport
            }

            mrm <- HeadMultisigRegimeManager.resource(
              nodeConfig,
              backend,
              remoteL2Ledger,
              persistence,
              mrmTracer,
              wsPeerTransport,
            )
        } yield (nodeConfig, system, mrm)

        resource.use { case (nodeConfig, system, mrm) =>
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
                            .start // Run in background
                            .void
                }

                _ <- system.waitForTermination
            } yield ExitCode.Success
        }
    }
}
