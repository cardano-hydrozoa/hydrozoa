package hydrozoa.app

import cats.effect.{ExitCode, IO, IOApp}
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.cardano.wallet.WalletModule
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.backend.cardano.CardanoBackendBlockfrost
import scalus.cardano.address.ShelleyAddress
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

    val logger = Logging.loggerIO("hydrozoa.app.TokenRecovery")

    override def run(args: List[String]): IO[ExitCode] = {
        val cardanoNetwork: StandardCardanoNetwork = Main.cardanoNetwork

        (for {
            _ <- logger.info("Starting token recovery from faucet...")

            // Load environment config
            env <- Main.loadEnv
            _ <- logger.info("Loaded environment configuration")

            // Check that token recovery address is defined
            tokenRecoveryAddress <- env.tokenRecoveryAddress match {
                case Some(addr) => IO.pure(addr)
                case None =>
                    logger.error("TOKEN_RECOVERY_ADDRESS is not set in .env file") >>
                        IO.raiseError(
                          new IllegalStateException(
                            "TOKEN_RECOVERY_ADDRESS must be set to recover tokens"
                          )
                        )
            }
            _ <- logger.info(s"Token recovery address: ${tokenRecoveryAddress.toBech32.get}")

            // Create faucet address from verification key
            faucetAddress = env.verificationKey.shelleyAddress()(using cardanoNetwork)
            _ <- logger.info(s"Faucet address: ${faucetAddress.toBech32.get}")

            // Initialize backend
            _ <- logger.info("Initializing Cardano backend...")
            backend <- CardanoBackendBlockfrost(
              network = Left(cardanoNetwork),
              apiKey = env.blockfrostApiKey
            )

            // Query faucet UTXOs
            _ <- logger.info("Querying faucet address for UTXOs...")
            faucetUtxos <- backend
                .utxosAt(faucetAddress)
                .flatMap(_.fold(IO.raiseError, IO.pure))

            _ <- logger.info(s"Found ${faucetUtxos.size} UTXO(s) at faucet address")

            // Filter UTXOs containing tokens
            utxosWithTokens = faucetUtxos.filter { case (_, output) =>
                output.value.assets.nonEmpty
            }

            _ <-
                if utxosWithTokens.isEmpty then
                    logger.info("No UTXOs with tokens found. Nothing to recover.")
                else logger.info(s"Found ${utxosWithTokens.size} UTXO(s) containing tokens")

            // Calculate total tokens to recover
            totalTokens = Value.combine(utxosWithTokens.map((_, o) => o.value))
            tokensOnly = Value(Coin.zero, totalTokens.assets)

            _ <- logger.info(s"Total tokens to recover: ${tokensOnly.assets}")

            // Build transaction
            _ <- logger.info("Building recovery transaction...")

            // Create token output with min ADA
            tokenOutput = TransactionOutput
                .Babbage(
                  address = tokenRecoveryAddress,
                  value = tokensOnly
                )
                .ensureMinAda(cardanoNetwork)

            _ <- logger.info(s"Token output requires ${tokenOutput.value.coin} lovelace minimum")

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
            _ <- logger.info("Signing transaction with faucet wallet...")
            walletModule = WalletModule.Scalus
            witness = walletModule.signTx(balanced, env.verificationKey, env.signingKey)
            signed = balanced.attachVKeyWitnesses(List(witness))

            _ <- logger.info(s"Recovery tx: ${HexUtil.encodeHexString(signed.toCbor)}")

            // Submit transaction
            _ <- logger.info("Submitting transaction...")
            result <- backend.submitTx(signed)

            _ <- logger.info(s"Submission result: $result")
            _ <- logger.info("Token recovery completed successfully!")

        } yield ExitCode.Success).handleErrorWith { err =>
            logger.error(s"Token recovery failed: ${err.getMessage}") >>
                IO.pure(ExitCode.Error)
        }
    }
end TokenRecovery
