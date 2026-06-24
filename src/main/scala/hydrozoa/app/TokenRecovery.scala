package hydrozoa.app

import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.bloxbean.cardano.client.util.HexUtil
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.config.head.network.StandardCardanoNetwork
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, error, info}
import java.nio.file.Path
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{Coin, EvaluatorMode, PlutusScriptEvaluator, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{Change, PubKeyWitness, TransactionBuilder}

/** Recovers tokens from the configured peer's wallet address to a recovery address.
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.app.TokenRecovery <head-config.json> <peer-private.json> <bech32-recovery>"
  * }}}
  *
  * Queries all UTXOs at the peer's wallet address, extracts those containing tokens, and sends:
  *   - All tokens (with minimum ADA) to the recovery address
  *   - Excess ADA back to the peer address as change
  *
  * The transaction is signed using the peer wallet carried by the private config.
  */
object TokenRecovery
    extends CommandIOApp(
      name = "token-recovery",
      header = "Recover tokens at the peer's wallet address to a recovery address"
    ):

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa.app.TokenRecovery"))

    private val headArg: Opts[Path] =
        Opts.argument[String]("head-config.json").map(Path.of(_))
    private val privateArg: Opts[Path] =
        Opts.argument[String]("peer-private.json").map(Path.of(_))
    private val recoveryArg: Opts[String] = Opts.argument[String]("bech32-recovery")

    override def main: Opts[IO[ExitCode]] =
        (headArg, privateArg, recoveryArg).mapN(recover)

    private def recover(
        headConfigPath: Path,
        privateConfigPath: Path,
        recoveryBech32: String
    ): IO[ExitCode] = (for {
        _ <- log.info("Starting token recovery from peer wallet...")

        loaded <- NodeConfig.load(headConfigPath, privateConfigPath)
        (nodeConfig, backend) = loaded

        recoveryAddress <- IO
            .delay(Address.fromBech32(recoveryBech32))
            .flatMap {
                case addr: ShelleyAddress => IO.pure(addr)
                case other =>
                    IO.raiseError(
                      new IllegalArgumentException(
                        s"Recovery destination must be a Shelley address, got: ${other.getClass.getSimpleName}"
                      )
                    )
            }
        _ <- log.info(s"Recovery address: ${recoveryAddress.toBech32.get}")

        cardanoNetwork: StandardCardanoNetwork =
            nodeConfig.cardanoNetwork match {
                case n: StandardCardanoNetwork => n
                case _ =>
                    throw new IllegalStateException(
                      "TokenRecovery requires a standard Cardano network in the head config"
                    )
            }
        wallet = nodeConfig.ownWallet
        vKey = wallet.exportVerificationKey
        peerAddress = vKey.shelleyAddress()(using cardanoNetwork)
        _ <- log.info(s"Peer address: ${peerAddress.toBech32.get}")

        _ <- log.info("Querying peer address for UTXOs...")
        peerUtxos <- backend.utxosAt(peerAddress).flatMap(_.fold(IO.raiseError, IO.pure))
        _ <- log.info(s"Found ${peerUtxos.size} UTXO(s) at peer address")

        utxosWithTokens = peerUtxos.filter { case (_, output) => output.value.assets.nonEmpty }

        result <-
            if utxosWithTokens.isEmpty then
                log.info("No UTXOs with tokens found. Nothing to recover.").as(ExitCode.Success)
            else
                for {
                    _ <- log.info(s"Found ${utxosWithTokens.size} UTXO(s) containing tokens")

                    totalTokens = Value.combine(utxosWithTokens.map((_, o) => o.value))
                    tokensOnly = Value(Coin.zero, totalTokens.assets)
                    _ <- log.info(s"Total tokens to recover: ${tokensOnly.assets}")

                    _ <- log.info("Building recovery transaction...")
                    tokenOutput = TransactionOutput
                        .Babbage(address = recoveryAddress, value = tokensOnly)
                        .ensureMinAda(cardanoNetwork)
                    _ <- log.info(
                      s"Token output requires ${tokenOutput.value.coin} lovelace minimum"
                    )

                    unbalanced = TransactionBuilder
                        .build(
                          cardanoNetwork.network,
                          utxosWithTokens.map { case (utxoId, output) =>
                              Spend(utxo = Utxo(utxoId, output), witness = PubKeyWitness)
                          }.toList :+ Send(tokenOutput)
                        )
                        .fold(err => throw new RuntimeException(err.toString), identity)

                    balanced = unbalanced
                        .balanceContext(
                          diffHandler = Change.changeOutputDiffHandler(
                            _,
                            _,
                            protocolParams = cardanoNetwork.cardanoProtocolParams,
                            changeOutputIdx = 0
                          ),
                          protocolParams = cardanoNetwork.cardanoProtocolParams,
                          evaluator = PlutusScriptEvaluator(
                            cardanoNetwork.cardanoInfo,
                            EvaluatorMode.EvaluateAndComputeCost
                          )
                        )
                        .fold(err => throw new RuntimeException(err.toString), _.transaction)

                    _ <- log.info("Signing transaction with peer wallet...")
                    signed = balanced.attachVKeyWitnesses(List(wallet.mkVKeyWitness(balanced)))

                    _ <- log.info(s"Recovery tx: ${HexUtil.encodeHexString(signed.toCbor)}")

                    _ <- log.info("Submitting transaction...")
                    submitResult <- backend.submitTx(signed)
                    _ <- log.info(s"Submission result: $submitResult")
                    _ <- log.info("Token recovery completed successfully")
                } yield ExitCode.Success

    } yield result).handleErrorWith { err =>
        log.error(s"Token recovery failed: ${err.getMessage}") >>
            IO.pure(ExitCode.Error)
    }

end TokenRecovery
