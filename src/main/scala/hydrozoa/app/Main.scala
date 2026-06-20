package hydrozoa.app

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.*
import com.comcast.ip4s.{Host, Port, host, port}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.node.NodeConfig
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.logging.{Logging, Slf4jTracer, withCtx}
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId, PeerWallet}
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import hydrozoa.multisig.persistence.{Cf, Persistence}
import hydrozoa.multisig.server.HydrozoaServer
import hydrozoa.multisig.{CoilMultisigRegimeManager, HeadMultisigRegimeManager, HeadMultisigRegimeManagerEventFormat}
import io.github.cdimascio.dotenv.Dotenv
import java.nio.file.{Files, Path}
import org.http4s.jdkhttpclient.JdkWSClient
import scala.concurrent.duration.DurationInt
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

/** Hydrozoa application entry point.
  *
  * Runs one Hydrozoa node — a head peer or a coil peer — using cats-effect IOApp for resource-safe
  * initialization and shutdown. The shared head config (the full head+coil membership, produced
  * once by [[BuildHeadConfig]]) is loaded from a JSON artifact; this node supplies only its own
  * signing key and its identity (`NODE_ID`).
  *
  * Configuration is loaded from:
  *   1. .env file in the current directory (if present)
  *   2. System environment variables (override .env values)
  *
  * Required environment variables:
  *   - BLOCKFROST_API_KEY: Blockfrost API key for Cardano backend
  *   - CARDANO_VERIFICATION_KEY: Hex-encoded Ed25519 verification key (64 hex chars = 32 bytes)
  *   - CARDANO_SIGNING_KEY: Hex-encoded Ed25519 signing key (64 hex chars = 32 bytes)
  *   - NODE_ID: this node's identity as `<head|coil>:<index>` (e.g. "head:0", "coil:3")
  *   - ADMIN_USERNAME / ADMIN_PASSWORD: credentials for the user-facing HTTP server (head nodes)
  *
  * Optional environment variables:
  *   - HEAD_CONFIG_PATH: path to the shared head config JSON (default "head-config.json")
  *   - HTTP_HOST / HTTP_PORT: user-facing HTTP server bind (default "0.0.0.0" / "8080")
  */
object Main extends IOApp {

    /** Environment configuration loaded from .env file or system environment. */
    final case class EnvConfig(
        verificationKey: VerificationKey,
        signingKey: SigningKey,
        blockfrostApiKey: String,
        sugarRushHost: String,
        sugarRushPort: String,
        tokenRecoveryAddress: Option[ShelleyAddress],
        adminUsername: String,
        adminPassword: String
    ) {
        val sugarRushUri: String = s"ws://$sugarRushHost:$sugarRushPort/ws"
    }

    /** Whether this node runs as a head peer or a coil peer. */
    enum NodeKind {
        case Head, Coil
    }

    /** This node's identity: its kind and its peer index within that kind (the `NODE_ID` env). */
    final case class NodeIdentity(kind: NodeKind, index: Int)

    private val logger = Logging.loggerIO("hydrozoa.app.Main")

    // Load .env file (if present) at startup
    private lazy val dotenv: Dotenv = Dotenv.configure().ignoreIfMissing().load()

    /** Read a required environment variable, checking .env file first, then system environment. Use
      * a default value if not found.
      * @param name
      *   variable name
      * @param default
      *   default value for the variable, if not found.
      */
    private def getOptionalEnvVar(name: String, default: => String): IO[String] =
        IO(Option(dotenv.get(name)).orElse(sys.env.get(name)).getOrElse(default))

    private def throwMissingEnvVarError(name: String): String = throw new IllegalStateException(
      s"Required environment variable not set: $name (checked .env file and system environment)"
    )

    /** Parse hex string to ByteString. */
    private def parseHex(hex: String, expectedBytes: Int, name: String): IO[ByteString] =
        IO {
            val cleaned = hex.replaceAll("\\s+", "")
            if cleaned.length != expectedBytes * 2 then {
                throw new IllegalArgumentException(
                  s"$name: expected $expectedBytes bytes (${expectedBytes * 2} hex chars), got ${cleaned.length} hex chars"
                )
            }
            val bytes = cleaned.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
            ByteString.fromArray(bytes)
        }

    /** Read a required environment variable, checking .env file first, then system environment.
      * Throw an error if not found.
      *
      * @param name
      *   variable name
      */
    private def getMandatoryEnvVar(name: String): IO[String] =
        getOptionalEnvVar(name, throwMissingEnvVarError(name))

    /** Load configuration from environment variables. */
    def loadEnv: IO[EnvConfig] =
        for {
            blockfrostKey <- getMandatoryEnvVar("BLOCKFROST_API_KEY")
            _ <- logger.info(s"Loaded Blockfrost API key: ${blockfrostKey.take(8)}...")

            vKeyHex <- getMandatoryEnvVar("CARDANO_VERIFICATION_KEY")
            vKeyBs <- parseHex(vKeyHex, 32, "CARDANO_VERIFICATION_KEY")
            vKey = VerificationKey.unsafeFromByteString(vKeyBs)
            _ <- logger.info(s"Loaded verification key: ${vKeyHex.take(16)}...")

            sKeyHex <- getMandatoryEnvVar("CARDANO_SIGNING_KEY")
            sKeyBs <- parseHex(sKeyHex, 32, "CARDANO_SIGNING_KEY")
            sKey = SigningKey.unsafeFromByteString(sKeyBs)
            _ <- logger.info("Loaded signing key")

            sugarRushHost <- getOptionalEnvVar("SUGAR_RUSH_HOST", "localhost")
            sugarRushPort <- getOptionalEnvVar("SUGAR_RUSH_PORT", "3001")

            tokenRecoveryAddressOpt <- getOptionalEnvVar("TOKEN_RECOVERY_ADDRESS", "").flatMap {
                case "" => IO.pure(None)
                case addr =>
                    IO.delay(Address.fromBech32(addr))
                        .flatMap {
                            case shelley: ShelleyAddress => IO.pure(Some(shelley))
                            case _ =>
                                IO.raiseError(
                                  new IllegalArgumentException(
                                    s"TOKEN_RECOVERY_ADDRESS must be a Shelley address, got: $addr"
                                  )
                                )
                        }
                        .handleErrorWith { err =>
                            IO.raiseError(
                              new IllegalArgumentException(
                                s"TOKEN_RECOVERY_ADDRESS must be a valid Bech32 address: ${err.getMessage}"
                              )
                            )
                        }
            }
            _ <- tokenRecoveryAddressOpt.fold(IO.unit)(addr =>
                logger.info(s"Token recovery address: ${addr.toBech32.get}")
            )

            adminUsername <- getMandatoryEnvVar("ADMIN_USERNAME")
            adminPassword <- getMandatoryEnvVar("ADMIN_PASSWORD")
            _ <- logger.info(s"Loaded admin credentials for user: $adminUsername")
        } yield EnvConfig(
          verificationKey = vKey,
          signingKey = sKey,
          blockfrostApiKey = blockfrostKey,
          sugarRushHost = sugarRushHost,
          sugarRushPort = sugarRushPort,
          tokenRecoveryAddress = tokenRecoveryAddressOpt,
          adminUsername = adminUsername,
          adminPassword = adminPassword
        )

    /** Parse the `NODE_ID` env (`<head|coil>:<index>`) into a [[NodeIdentity]]. */
    private def parseNodeId(raw: String): IO[NodeIdentity] =
        raw.trim.split(":") match {
            case Array(kindStr, idxStr) =>
                val kindIO = kindStr.toLowerCase match {
                    case "head" => IO.pure(NodeKind.Head)
                    case "coil" => IO.pure(NodeKind.Coil)
                    case other =>
                        IO.raiseError(
                          new IllegalArgumentException(
                            s"NODE_ID type must be 'head' or 'coil', got: $other"
                          )
                        )
                }
                val idxIO = IO.fromOption(idxStr.toIntOption)(
                  new IllegalArgumentException(s"NODE_ID index must be an integer, got: $idxStr")
                )
                (kindIO, idxIO).mapN(NodeIdentity.apply)
            case _ =>
                IO.raiseError(
                  new IllegalArgumentException(s"NODE_ID must be '<head|coil>:<index>', got: $raw")
                )
        }

    /** Load and resolve the shared head config artifact every node shares. */
    private def loadHeadConfig(path: String, backend: CardanoBackend[IO]): IO[HeadConfig] =
        for {
            jsonStr <- IO.blocking(Files.readString(Path.of(path)))
            headConfig <- HeadConfig.fromJson(jsonStr, backend).value.flatMap {
                case Right(hc) => IO.pure(hc)
                case Left(err) =>
                    IO.raiseError(
                      new RuntimeException(s"Failed to load head config from $path: $err")
                    )
            }
        } yield headConfig

    /** Build this node's [[NodeConfig]] from the shared head config and its own wallet, dispatching
      * on the node kind. Fails if the wallet's key is not among the head config's peers of that
      * kind, or if the derived peer index disagrees with the declared `NODE_ID` index.
      */
    private def buildNodeConfig(
        headConfig: HeadConfig,
        ownWallet: PeerWallet,
        identity: NodeIdentity,
        httpHost: String,
        httpPort: String,
        blockfrostApiKey: String
    ): IO[NodeConfig] = {
        val evacuationConfig = NodeOperationEvacuationConfig(
          evacuationBotPollingPeriod = 1.minute,
          // NOTE: reusing the node wallet; production should use a separate evacuation wallet.
          evacuationWallet = ownWallet
        )
        val multisigConfig = NodeOperationMultisigConfig.default
        val built: Option[NodeConfig] = identity.kind match {
            case NodeKind.Head =>
                NodeConfig.mkHeadConfig(
                  headConfig,
                  ownWallet,
                  evacuationConfig,
                  multisigConfig,
                  httpHost,
                  httpPort,
                  blockfrostApiKey
                )
            case NodeKind.Coil =>
                NodeConfig.mkCoilConfig(
                  headConfig,
                  ownWallet,
                  evacuationConfig,
                  multisigConfig,
                  httpHost,
                  httpPort,
                  blockfrostApiKey
                )
        }
        for {
            nodeConfig <- IO.fromOption(built)(
              new RuntimeException(
                s"the configured signing key is not among the head config's ${identity.kind} peers"
              )
            )
            _ <- IO.raiseUnless(nodeConfig.ownPeerIndex == identity.index)(
              new RuntimeException(
                s"NODE_ID index ${identity.index} does not match the index derived from the wallet " +
                    s"key (${nodeConfig.ownPeerIndex}); check NODE_ID and the signing key match the head config"
              )
            )
        } yield nodeConfig
    }

    val cardanoNetwork: StandardCardanoNetwork = CardanoNetwork.Preview
    given CardanoNetwork.Section = cardanoNetwork

    override def run(args: List[String]): IO[ExitCode] = {
        val resource = for {
            env <- Resource.eval(loadEnv)
            identity <- Resource.eval(getMandatoryEnvVar("NODE_ID").flatMap(parseNodeId))
            headConfigPath <- Resource.eval(
              getOptionalEnvVar("HEAD_CONFIG_PATH", "head-config.json")
            )
            httpHost <- Resource.eval(getOptionalEnvVar("HTTP_HOST", "0.0.0.0"))
            httpPort <- Resource.eval(getOptionalEnvVar("HTTP_PORT", "8080"))
            _ <- Resource.eval(
              logger.info(s"Starting Hydrozoa ${identity.kind} node ${identity.index}...")
            )

            _ <- Resource.eval(logger.info("Starting Cardano Blockfrost Backend..."))
            backend <- Resource.eval(
              CardanoBackendBlockfrost(
                network = Left(cardanoNetwork),
                apiKey = env.blockfrostApiKey
              )
            )

            headConfig <- Resource.eval(loadHeadConfig(headConfigPath, backend))
            ownWallet = PeerWallet.scalusWallet(env.verificationKey, env.signingKey)
            nodeConfig <- Resource.eval(
              buildNodeConfig(
                headConfig,
                ownWallet,
                identity,
                httpHost,
                httpPort,
                env.blockfrostApiKey
              )
            )
            _ <- Resource.eval(
              logger.info(s"headAddress: ${nodeConfig.headMultisigAddress.toBech32.get}")
            )

            _ <- Resource.eval(logger.info(s"Connecting to L2 ledger at ${env.sugarRushUri}"))
            remoteL2Ledger <- Resource.eval(
              RemoteL2Ledger.create(
                wsUri = env.sugarRushUri,
                config = cardanoNetwork
              )
            )

            // Per-node persistence store, keyed by this node's own peer label (e.g. "0", "c3").
            backendStore <- RocksDbBackendStore.open(
              Path.of(s".hydrozoa-data/peer-${nodeConfig.ownPeerLabel}/rocksdb"),
              Cf.mkAll(
                headPeers = nodeConfig.headConfig.headPeerNums.toList,
                coilPeers = nodeConfig.headConfig.coilPeers.coilPeerNumbers,
                hubs = nodeConfig.headConfig.coilPeers.hubHeadPeerNumbers
              )
            )
            persistence <- Resource.eval(Persistence.fromBackend(backendStore))

            // Attach cleanup to ActorSystem resource - env, backend, nodeConfig are in scope here
            system <- ActorSystem[IO]("Hydrozoa Demo").onFinalize(
              logger.info("Hydrozoa node shut down, running janitor...") *>
                  Janitor.cleanUp(
                    backend = backend,
                    peerWallet = nodeConfig.ownWallet,
                    config = nodeConfig.headConfig,
                    faucetAddress = env.verificationKey.shelleyAddress()(using cardanoNetwork),
                    tokenRecoveryAddress = env.tokenRecoveryAddress
                  )
            )

            // Per-process WS transports: bind this node's server and dial its peers. The wiring's
            // tag (Left = head, Right = coil) carries the right transports for the manager to
            // register its liaisons on and build remote proxy handles from.
            wsClient <- Resource.eval(JdkWSClient.simple[IO])
            wsWiring <- nodeConfig.ownPeerId match {
                case PeerId.Head(n) =>
                    NodeTransport.headResource(nodeConfig, wsClient, n).map(Left(_))
                case PeerId.Coil(n) =>
                    NodeTransport.coilResource(nodeConfig, wsClient, n).map(Right(_))
            }
        } yield (
          env,
          backend,
          nodeConfig,
          remoteL2Ledger,
          persistence,
          system,
          httpHost,
          httpPort,
          wsWiring
        )

        resource.use {
            case (
                  env,
                  backend,
                  nodeConfig,
                  remoteL2Ledger,
                  persistence,
                  system,
                  httpHost,
                  httpPort,
                  wsWiring
                ) =>
                // The regime-manager formatter is keyed by head peer number; for a coil node we
                // reuse it for the shared sub-actor events and override the "peer" context with the
                // coil's own label so logs stay unambiguous.
                val mrmTracer =
                    Slf4jTracer.sink
                        .withCtx(_.updated("peer", nodeConfig.ownPeerLabel))
                        .contramap(
                          HeadMultisigRegimeManagerEventFormat.humanFormat(
                            HeadPeerNumber(nodeConfig.ownPeerIndex)
                          )
                        )

                // The wiring's tag is this node's kind (Left = head, Right = coil).
                wsWiring match {
                    case Left(headWiring) =>
                        for {
                            mrm <- HeadMultisigRegimeManager.apply(
                              nodeConfig,
                              backend,
                              remoteL2Ledger,
                              persistence,
                              mrmTracer,
                              headWiring
                            )
                            _ <- system.actorOf(mrm, "HeadMultisigRegimeManager")
                            _ <- logger.info("Hydrozoa head node started successfully")

                            // Start HTTP server once RequestSequencer is available
                            _ <- mrm.connectionsDeferred.get.flatMap { connections =>
                                val serverConfig = HydrozoaServer.Config(
                                  host = Host.fromString(httpHost).getOrElse(host"0.0.0.0"),
                                  port = Port.fromString(httpPort).getOrElse(port"8080"),
                                  adminUsername = env.adminUsername,
                                  adminPassword = env.adminPassword
                                )
                                logger.info("Starting HTTP server...") *>
                                    HydrozoaServer
                                        .create(
                                          connections.requestSequencer,
                                          connections.blockWeaver,
                                          nodeConfig.headConfig,
                                          serverConfig
                                        )
                                        .use(_ => IO.never)
                                        .start // Run in background
                                        .void
                            }

                            _ <- system.waitForTermination
                        } yield ExitCode.Success

                    case Right(coilWiring) =>
                        for {
                            mrm <- CoilMultisigRegimeManager.apply(
                              nodeConfig,
                              backend,
                              remoteL2Ledger,
                              persistence,
                              mrmTracer,
                              coilWiring
                            )
                            _ <- system.actorOf(mrm, "CoilMultisigRegimeManager")
                            _ <- logger.info("Hydrozoa coil node started successfully")
                            _ <- system.waitForTermination
                        } yield ExitCode.Success
                }
        }
    }
}
