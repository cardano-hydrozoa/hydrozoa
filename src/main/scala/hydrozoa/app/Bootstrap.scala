package hydrozoa.app

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import com.bloxbean.cardano.client.util.HexUtil
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info, warn}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import java.nio.file.{Files, Path}
import java.security.SecureRandom
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{EvaluatorMode, PlutusScriptEvaluator, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{Change, TransactionBuilder}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

object Bootstrap:

    /** Generate a new Ed25519 key pair for Cardano. */
    def generateKeyPair(): IO[(VerificationKey, SigningKey)] =
        IO {
            val random = new SecureRandom()
            val generator = new Ed25519KeyPairGenerator()
            generator.init(new Ed25519KeyGenerationParameters(random))

            val keyPair = generator.generateKeyPair()

            val privateKey = keyPair.getPrivate.asInstanceOf[Ed25519PrivateKeyParameters]
            val publicKey = keyPair.getPublic.asInstanceOf[Ed25519PublicKeyParameters]

            val privateKeyBytes: Array[Byte] = privateKey.getEncoded // 32 bytes (seed)
            val publicKeyBytes: Array[Byte] = publicKey.getEncoded // 32 bytes

            val vKey = VerificationKey.unsafeFromByteString(ByteString.fromArray(publicKeyBytes))
            val sKey = SigningKey.unsafeFromByteString(ByteString.fromArray(privateKeyBytes))

            (vKey, sKey)
        }

    /** Read both config files and decode the resulting [[NodeConfig]] together with the Blockfrost
      * backend the decoder constructs. Shared by every CLI that needs to act as a configured peer
      * ([[Main]], [[Migrate]], [[TokenRecovery]]).
      *
      * @param backendOverride
      *   if `Some`, used in place of the Blockfrost backend the decoder would otherwise build from
      *   the private config's API key. Tests pass a mock; CLIs leave it `None`.
      */
    def loadNodeConfig(
        headConfigPath: Path,
        privateConfigPath: Path,
        backendOverride: Option[CardanoBackend[IO]] = None,
    ): IO[(NodeConfig, CardanoBackend[IO])] =
        for {
            headStr <- IO.blocking(Files.readString(headConfigPath))
            privateStr <- IO.blocking(Files.readString(privateConfigPath))
            loaded <- NodeConfig
                .fromJson(headStr, privateStr, backendOverride)
                .foldF(
                  err => IO.raiseError(new RuntimeException(s"Failed to load NodeConfig: $err")),
                  IO.pure
                )
        } yield loaded

end Bootstrap

/** Main entry point for generating a new Ed25519 key pair.
  *
  * Prints the verification key and signing key as hex to stdout.
  */
object GenerateKeyPair extends IOApp:
    override def run(args: List[String]): IO[ExitCode] =
        Bootstrap.generateKeyPair().flatMap { case (vKey, sKey) =>
            val vKeyHex = vKey.bytes.toArray.map("%02x".format(_)).mkString
            val sKeyHex = sKey.bytes.toArray.map("%02x".format(_)).mkString
            for {
                _ <- IO.println("Generated new Ed25519 key pair:")
                _ <- IO.println(s"Verification key (32 bytes): $vKeyHex")
                _ <- IO.println(s"Signing key (32 bytes): $sKeyHex")
                _ <- IO.println(
                  s"Testnet address: ${vKey.shelleyAddress()(using CardanoNetwork.Preview).toBech32.get}"
                )
            } yield ExitCode.Success
        }
end GenerateKeyPair

/** Migrate all funds from the configured peer's address to a destination address.
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.app.Migrate <head-config.json> <peer-private.json> <bech32-dest>"
  * }}}
  *
  * Reads wallet + Blockfrost credentials from the same files Main does; signs with the peer wallet
  * carried by the private config.
  */
object Migrate
    extends CommandIOApp(
      name = "migrate",
      header = "Send all UTXOs at the peer's wallet address to a destination address"
    ):

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa.app.Migrate"))

    private val headArg: Opts[Path] =
        Opts.argument[String]("head-config.json").map(Path.of(_))
    private val privateArg: Opts[Path] =
        Opts.argument[String]("peer-private.json").map(Path.of(_))
    private val destArg: Opts[String] = Opts.argument[String]("bech32-destination")

    override def main: Opts[IO[ExitCode]] =
        (headArg, privateArg, destArg).mapN(migrateAllFunds)

    private def migrateAllFunds(
        headConfigPath: Path,
        privateConfigPath: Path,
        destinationBech32: String
    ): IO[ExitCode] =
        for {
            _ <- log.info(s"Starting migration to $destinationBech32")

            loaded <- Bootstrap.loadNodeConfig(headConfigPath, privateConfigPath)
            (nodeConfig, backend) = loaded

            destination <- IO
                .delay(Address.fromBech32(destinationBech32))
                .flatMap {
                    case addr: scalus.cardano.address.ShelleyAddress =>
                        log.info(s"Destination address: ${addr.toBech32.get}") *>
                            IO.pure(addr)
                    case other =>
                        IO.raiseError(
                          new IllegalArgumentException(
                            s"Destination must be a Shelley address, got: ${other.getClass.getSimpleName}"
                          )
                        )
                }
                .handleErrorWith(err =>
                    IO.raiseError(new IllegalArgumentException(s"Invalid bech32 address: $err"))
                )

            wallet = nodeConfig.ownWallet
            cardanoNetwork: StandardCardanoNetwork =
                nodeConfig.cardanoNetwork match {
                    case n: StandardCardanoNetwork => n
                    case _ =>
                        throw new IllegalStateException(
                          "Migrate requires a standard Cardano network in the head config"
                        )
                }
            peerAddress = wallet.exportVerificationKey.shelleyAddress()(using cardanoNetwork)
            _ <- log.info(s"Peer address: ${peerAddress.toBech32.get}")

            _ <- log.info("Fetching UTXOs from peer address...")
            utxosResult <- backend.utxosAt(peerAddress)
            utxosMap <- IO.fromEither(
              utxosResult.left.map(err => new RuntimeException(s"Failed to fetch UTXOs: $err"))
            )

            _ <-
                if utxosMap.isEmpty then
                    log.warn("No UTXOs found at peer address. Nothing to migrate.")
                        .as(
                          ExitCode.Success
                        )
                else
                    for {
                        _ <- log.info(s"Found ${utxosMap.size} UTXO(s) to migrate")

                        totalValue = Value.combine(utxosMap.map((_, output) => output.value))
                        _ <- log.info(
                          s"Total value to migrate: ${totalValue.coin.value} lovelace + " +
                              s"${totalValue.assets.assets.size} asset(s)"
                        )

                        _ <- log.info("Building transaction...")
                        unbalanced = TransactionBuilder
                            .build(
                              cardanoNetwork.network,
                              utxosMap.map { case (utxoId, output) =>
                                  Spend(Utxo(utxoId, output))
                              }.toList :+
                                  Send(
                                    TransactionOutput.Babbage(
                                      address = destination,
                                      value = totalValue,
                                      datumOption = None,
                                      scriptRef = None
                                    )
                                  )
                            )
                            .fold(
                              err =>
                                  throw new RuntimeException(
                                    s"Failed to build transaction: $err"
                                  ),
                              identity
                            )

                        _ <- log.info("Balancing transaction...")
                        protocolParams <- backend.fetchLatestParams.flatMap(
                          IO.fromEither(_)
                              .adaptError(err =>
                                  new RuntimeException(s"Failed to fetch protocol params: $err")
                              )
                        )

                        balanced = unbalanced
                            .balanceContext(
                              diffHandler = Change.changeOutputDiffHandler(
                                _,
                                _,
                                protocolParams = protocolParams,
                                changeOutputIdx = 0
                              ),
                              protocolParams = protocolParams,
                              evaluator = PlutusScriptEvaluator(
                                cardanoNetwork.cardanoInfo,
                                EvaluatorMode.EvaluateAndComputeCost
                              )
                            )
                            .fold(
                              err =>
                                  throw new RuntimeException(
                                    s"Failed to balance transaction: $err"
                                  ),
                              _.transaction
                            )

                        _ <- log.info("Signing transaction...")
                        signed = wallet.signTx(balanced)

                        _ <- log.info(s"Transaction hash: ${signed.id}")
                        _ <- log.info(
                          s"Transaction CBOR: ${HexUtil.encodeHexString(signed.toCbor)}"
                        )

                        _ <- log.info("Submitting transaction...")
                        submitResult <- backend.submitTx(signed)
                        _ <- IO.fromEither(
                          submitResult.left.map(err =>
                              new RuntimeException(s"Failed to submit transaction: $err")
                          )
                        )

                        _ <- log.info("Transaction submitted successfully")
                        _ <- log.info(
                          s"Explorer: https://preview.cexplorer.io/tx/${signed.id.toHex}"
                        )
                    } yield ExitCode.Success

        } yield ExitCode.Success

end Migrate
