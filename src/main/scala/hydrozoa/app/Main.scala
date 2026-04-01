package hydrozoa.app

import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.bloxbean.cardano.client.util.HexUtil
import com.bloxbean.cardano.client.util.HexUtil.encodeHexString
import com.comcast.ip4s.{host, port}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.backend.cardano.CardanoBackendBlockfrost
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger
import hydrozoa.multisig.server.HydrozoaServer
import io.github.cdimascio.dotenv.Dotenv
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.Coin
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

/** Hydrozoa application entry point.
  *
  * Runs the Hydrozoa node using cats-effect IOApp for resource-safe initialization and shutdown.
  *
  * Configuration is loaded from:
  *   1. .env file in the current directory (if present)
  *   2. System environment variables (override .env values)
  *
  * Required environment variables:
  *   - BLOCKFROST_API_KEY: Blockfrost API key for Cardano backend
  *   - CARDANO_VERIFICATION_KEY: Hex-encoded Ed25519 verification key (64 hex chars = 32 bytes)
  *   - CARDANO_SIGNING_KEY: Hex-encoded Ed25519 signing key (64 hex chars = 32 bytes)
  *   - EQUITY: Minimum equity size in lovelace (e.g., "2000000" for 2 ADA)
  */
object Main extends IOApp {

    /** Environment configuration loaded from .env file or system environment. */
    final case class EnvConfig(
        verificationKey: VerificationKey,
        signingKey: SigningKey,
        minEquity: Coin,
        blockfrostApiKey: String,
        sugarRushHost: String,
        sugarRushPort: String,
        tokenRecoveryAddress: Option[ShelleyAddress],
        adminUsername: String,
        adminPassword: String
    ) {
        val sugarRushUri: String = s"ws://$sugarRushHost:$sugarRushPort/ws"
    }

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

            minEquityStr <- getMandatoryEnvVar("EQUITY")
            minEquity <- IO.fromEither(
              minEquityStr.toLongOption
                  .toRight(
                    new IllegalArgumentException(
                      s"EQUITY must be a valid long, got: $minEquityStr"
                    )
                  )
                  .map(Coin.apply)
            )

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

            _ <- logger.info(s"Minimum equity: $minEquity lovelace")
        } yield EnvConfig(
          verificationKey = vKey,
          signingKey = sKey,
          minEquity = minEquity,
          blockfrostApiKey = blockfrostKey,
          sugarRushHost = sugarRushHost,
          sugarRushPort = sugarRushPort,
          tokenRecoveryAddress = tokenRecoveryAddressOpt,
          adminUsername = adminUsername,
          adminPassword = adminPassword
        )

    val cardanoNetwork: StandardCardanoNetwork = CardanoNetwork.Preview

    override def run(args: List[String]): IO[ExitCode] =

        val setupIO = for {
            _ <- logger.info("Starting Hydrozoa node...")
            env <- loadEnv
            _ <- logger.info("Starting Cardano Blockfrost Backend...")
            backend <- CardanoBackendBlockfrost(
              network = Left(cardanoNetwork),
              apiKey = env.blockfrostApiKey
            )
            nodeConfig <- Bootstrap.mkNodeConfig(cardanoNetwork, backend)(
              vKey = env.verificationKey,
              sKey = env.signingKey,
              minEquity = env.minEquity
            )
            _ <- logger.info(s"headAddress: ${nodeConfig.headMultisigAddress.toBech32.get}")
            _ <- logger.info(s"initTx hash: ${nodeConfig.initializationTx.tx.id}")
            _ <- logger.info(s"initTx: ${encodeHexString(nodeConfig.initializationTx.tx.toCbor)}")
        } yield (env, backend, nodeConfig)

        val resource = for {
            result <- Resource.eval(setupIO)
            (env, backend, nodeConfig) = result

            _ <- Resource.eval(
              logger.info(s"Connecting to L2 ledger at ${env.sugarRushUri}")
            )
            remoteL2Ledger <- Resource.eval(
              RemoteL2Ledger.create(
                wsUri = env.sugarRushUri,
                config = cardanoNetwork
              )
            )

            // Attach cleanup to ActorSystem resource - env, backend, nodeConfig are in scope here
            system <- ActorSystem[IO]("Hydrozoa Demo").onFinalize(
              logger.info("Hydrozoa node shut down, running janitor...") *>
                  Janitor.cleanUp(
                    backend = backend,
                    headPeerWallet = nodeConfig.ownHeadWallet,
                    config = nodeConfig.headConfig,
                    faucetAddress = env.verificationKey.shelleyAddress()(using cardanoNetwork),
                    tokenRecoveryAddress = env.tokenRecoveryAddress
                  )
            )
        } yield (env, backend, nodeConfig, remoteL2Ledger, system)

        resource.use { case (env, backend, nodeConfig, remoteL2Ledger, system) =>
            for {
                mrm <- MultisigRegimeManager.apply(nodeConfig, backend, remoteL2Ledger)
                _ <- system.actorOf(mrm, "MultisigRegimeManager")
                _ <- logger.info("Hydrozoa node started successfully")

                // Start HTTP server once EventSequencer is available
                _ <- mrm.connectionsDeferred.get.flatMap { connections =>
                    val serverConfig = HydrozoaServer.Config(
                      host = host"0.0.0.0",
                      port = port"8080",
                      adminUsername = env.adminUsername,
                      adminPassword = env.adminPassword
                    )
                    logger.info("Starting HTTP server...") *>
                        HydrozoaServer
                            .create(
                              connections.eventSequencer,
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
        }

}
