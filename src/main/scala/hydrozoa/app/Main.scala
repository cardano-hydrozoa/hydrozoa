package hydrozoa.app

import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.bloxbean.cardano.client.util.HexUtil
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.lib.cardano.scalus.ShelleyAddressExtra
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.backend.cardano.CardanoBackendBlockfrost
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import io.github.cdimascio.dotenv.Dotenv
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
  *   - MIN_EQUITY: Minimum equity size in lovelace (e.g., "2000000" for 2 ADA)
  */
object Main extends IOApp {

    /** Environment configuration loaded from .env file or system environment. */
    final case class EnvConfig(
        verificationKey: VerificationKey,
        signingKey: SigningKey,
        minEquity: Coin,
        blockfrostApiKey: String
    )

    private val logger = Logging.loggerIO("hydrozoa.app.Main")

    // Load .env file (if present) at startup
    private lazy val dotenv: Dotenv = Dotenv.configure().ignoreIfMissing().load()

    /** Read a required environment variable, checking .env file first, then system environment.
      */
    private def getEnv(name: String): IO[String] =
        IO {
            Option(dotenv.get(name))
                .orElse(sys.env.get(name))
                .getOrElse(
                  throw new IllegalStateException(
                    s"Required environment variable not set: $name (checked .env file and system environment)"
                  )
                )
        }

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

    /** Load configuration from environment variables. */
    private def loadEnv: IO[EnvConfig] =
        for {
            blockfrostKey <- getEnv("BLOCKFROST_API_KEY")
            _ <- logger.info(s"Loaded Blockfrost API key: ${blockfrostKey.take(8)}...")

            vKeyHex <- getEnv("CARDANO_VERIFICATION_KEY")
            vKeyBs <- parseHex(vKeyHex, 32, "CARDANO_VERIFICATION_KEY")
            vKey = VerificationKey.unsafeFromByteString(vKeyBs)
            _ <- logger.info(s"Loaded verification key: ${vKeyHex.take(16)}...")

            sKeyHex <- getEnv("CARDANO_SIGNING_KEY")
            sKeyBs <- parseHex(sKeyHex, 32, "CARDANO_SIGNING_KEY")
            sKey = SigningKey.unsafeFromByteString(sKeyBs)
            _ <- logger.info("Loaded signing key")

            minEquityStr <- getEnv("MIN_EQUITY")
            minEquity <- IO.fromEither(
              minEquityStr.toLongOption
                  .toRight(
                    new IllegalArgumentException(
                      s"MIN_EQUITY must be a valid long, got: $minEquityStr"
                    )
                  )
                  .map(Coin.apply)
            )
            _ <- logger.info(s"Minimum equity: $minEquity lovelace")
        } yield EnvConfig(
          verificationKey = vKey,
          signingKey = sKey,
          minEquity = minEquity,
          blockfrostApiKey = blockfrostKey
        )

    val cardanoNetwork: StandardCardanoNetwork = CardanoNetwork.Preview

    override def run(args: List[String]): IO[ExitCode] =
        val resources = for {
            _ <- Resource.eval(logger.info("Starting Hydrozoa node..."))

            // Load configuration from environment
            env <- Resource.eval(loadEnv)

            _ <- Resource.eval(logger.info("Starting Cardano Blockfrost Backend..."))
            backend <- Resource.eval(
              CardanoBackendBlockfrost(
                network = Left(cardanoNetwork),
                apiKey = env.blockfrostApiKey
              )
            )

            // Generate node configuration
            nodeConfig <- Resource.eval(
              Bootstrap.mkNodeConfig(cardanoNetwork, backend)(
                vKey = env.verificationKey,
                sKey = env.signingKey,
                minEquity = env.minEquity
              )
            )

            _ <- Resource.eval(
              logger.info(s"headAddress: ${nodeConfig.headMultisigAddress.toBech32.get}")
            )
            _ <- Resource.eval(
              logger.info(
                s"initTx: ${HexUtil.encodeHexString(nodeConfig.initializationTx.tx.toCbor)}"
              )
            )

            l2Ledger <- Resource.eval(EutxoL2Ledger(nodeConfig))

            // Shutdown hook - runs on success, error, or cancellation
            _ <- Resource.onFinalizeCase { _ =>
                for {
                    _ <- logger.info("Hydrozoa node shut down, running janitor...")
                    _ <- Janitor.cleanUp(
                      backend = backend,
                      headPeerWallet = nodeConfig.ownHeadWallet,
                      config = nodeConfig.headConfig,
                      faucetAddress = ShelleyAddressExtra.mkShelleyAddress(
                        env.verificationKey,
                        cardanoNetwork.network
                      )
                    )
                } yield ()
            }

            // ActorSystem already provides resource-safety via .use
            exitCode <- Resource.eval(
              ActorSystem[IO]("Hydrozoa Demo").use { system =>
                  for {
                      mrm <- MultisigRegimeManager.apply(nodeConfig, backend, l2Ledger)
                      _ <- system.actorOf(mrm, "MultisigRegimeManager")
                      _ <- logger.info("Hydrozoa node started successfully")
                      _ <- system.waitForTermination
                  } yield ExitCode.Success
              }
            )
        } yield exitCode

        resources.use(IO.pure)
}
