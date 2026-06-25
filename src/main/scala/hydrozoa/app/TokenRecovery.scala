package hydrozoa.app

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.config.head.network.StandardCardanoNetwork
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.cardano.wallet.WalletModule
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, error, info}
import hydrozoa.multisig.backend.cardano.{CardanoBackendBlockfrost, CardanoBackendEventFormat}
import scalus.cardano.ledger.{Coin, EvaluatorMode, PlutusScriptEvaluator, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{Change, PubKeyWitness, TransactionBuilder}

/** Recovers tokens from the faucet address to the token recovery address.
  *
  * This tool queries all UTXOs at the faucet address, extracts those containing tokens, and sends:
  *   - All tokens (with minimum ADA) to the token recovery address
  *   - Excess ADA back to the faucet address as change
  *
  * The transaction is signed using the faucet wallet (from CARDANO_SIGNING_KEY in .env).
  */
object TokenRecovery extends IOApp:

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa.app.TokenRecovery"))

    override def run(args: List[String]): IO[ExitCode] = {
        val cardanoNetwork: StandardCardanoNetwork = Main.cardanoNetwork

        (for {
            _ <- log.info("Starting token recovery from faucet...")

            // Load environment config
            env <- Main.loadEnv
            _ <- log.info("Loaded environment configuration")

            // Check that token recovery address is defined
            tokenRecoveryAddress <- env.tokenRecoveryAddress match {
                case Some(addr) => IO.pure(addr)
                case None =>
                    log.error("TOKEN_RECOVERY_ADDRESS is not set in .env file") >>
                        IO.raiseError(
                          new IllegalStateException(
                            "TOKEN_RECOVERY_ADDRESS must be set to recover tokens"
                          )
                        )
            }
            _ <- log.info(s"Token recovery address: ${tokenRecoveryAddress.toBech32.get}")

            // Create faucet address from verification key
            faucetAddress = env.verificationKey.shelleyAddress()(using cardanoNetwork)
            _ <- log.info(s"Faucet address: ${faucetAddress.toBech32.get}")

            // Initialize backend
            _ <- log.info("Initializing Cardano backend...")
            backend <- CardanoBackendBlockfrost(
              network = Left(cardanoNetwork),
              apiKey = env.blockfrostApiKey,
              tracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
            )

            // Query faucet UTXOs
            _ <- log.info("Querying faucet address for UTXOs...")
            faucetUtxos <- backend
                .utxosAt(faucetAddress)
                .flatMap(_.fold(IO.raiseError, IO.pure))

            _ <- log.info(s"Found ${faucetUtxos.size} UTXO(s) at faucet address")

            // Filter UTXOs containing tokens
            utxosWithTokens = faucetUtxos.filter { case (_, output) =>
                output.value.assets.nonEmpty
            }

            _ <-
                if utxosWithTokens.isEmpty then
                    log.info("No UTXOs with tokens found. Nothing to recover.")
                else log.info(s"Found ${utxosWithTokens.size} UTXO(s) containing tokens")

            // Calculate total tokens to recover
            totalTokens = Value.combine(utxosWithTokens.map((_, o) => o.value))
            tokensOnly = Value(Coin.zero, totalTokens.assets)

            _ <- log.info(s"Total tokens to recover: ${tokensOnly.assets}")

            // Build transaction
            _ <- log.info("Building recovery transaction...")

            // Create token output with min ADA
            tokenOutput = TransactionOutput
                .Babbage(
                  address = tokenRecoveryAddress,
                  value = tokensOnly
                )
                .ensureMinAda(cardanoNetwork)

            _ <- log.info(
              s"Token output requires ${tokenOutput.value.coin} lovelace minimum"
            )

            // Build transaction steps
            unbalanced = TransactionBuilder
                .build(
                  cardanoNetwork.network,
                  utxosWithTokens.map { case (utxoId, output) =>
                      Spend(
                        utxo = Utxo(utxoId, output),
                        witness = PubKeyWitness // Will be signed by faucet wallet
                      )
                  }.toList :+
                      Send(tokenOutput)
                )
                .fold(err => throw RuntimeException(err.toString), identity)

            // Balance transaction (this will add change output for excess ADA back to faucet)
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
                .fold(err => throw RuntimeException(err.toString), _.transaction)

            // Sign with faucet wallet
            _ <- log.info("Signing transaction with faucet wallet...")
            walletModule = WalletModule.Scalus
            witness = walletModule.signTx(balanced, env.verificationKey, env.signingKey)
            signed = balanced.attachVKeyWitnesses(List(witness))

            _ <- log.info(s"Recovery tx: ${HexUtil.encodeHexString(signed.toCbor)}")

            // Submit transaction
            _ <- log.info("Submitting transaction...")
            result <- backend.submitTx(signed)

            _ <- log.info(s"Submission result: $result")
            _ <- log.info("Token recovery completed successfully!")

        } yield ExitCode.Success).handleErrorWith { err =>
            log.error(s"Token recovery failed: ${err.getMessage}") >>
                IO.pure(ExitCode.Error)
        }
    }
end TokenRecovery
