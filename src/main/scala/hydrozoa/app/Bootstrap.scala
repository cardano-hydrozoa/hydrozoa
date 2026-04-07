package hydrozoa.app

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO, IOApp}
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.app.Main.loadEnv
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.HeadConfig.Preinit
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.fallback.FallbackContingency.mkFallbackContingencyWithDefaults
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.config.node.NodeConfig
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.wallet.WalletModule
import hydrozoa.lib.logging.Logging
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, HeadPeerWallet}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap, evacuationKeyOrdering}
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.evacuationKeyToData
import java.security.SecureRandom
import monocle.Focus.focus
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.concurrent.duration.DurationInt
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Coin, KeepRaw, PlutusScriptEvaluator, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.TransactionBuilderStep.Spend
import scalus.cardano.txbuilder.{TransactionBuilder, TransactionBuilderStep}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

/** This generates the full Hydrozoa config out of a couple of envvars.
  */
object Bootstrap:

    private val logger = Logging.loggerIO("hydrozoa.app.Bootstrap")

    /** Generate a new Ed25519 key pair for Cardano.
      *
      * @return
      *   A tuple of (VerificationKey, SigningKey) wrapped in IO
      */
    def generateKeyPair(): IO[(VerificationKey, SigningKey)] =
        IO {
            val random = new SecureRandom()
            val generator = new Ed25519KeyPairGenerator()
            generator.init(new Ed25519KeyGenerationParameters(random))

            val keyPair = generator.generateKeyPair()

            val privateKey = keyPair.getPrivate.asInstanceOf[Ed25519PrivateKeyParameters]
            val publicKey = keyPair.getPublic.asInstanceOf[Ed25519PublicKeyParameters]

            // Raw bytes
            val privateKeyBytes: Array[Byte] = privateKey.getEncoded // 32 bytes (seed)
            val publicKeyBytes: Array[Byte] = publicKey.getEncoded // 32 bytes

            // Wrap in Scalus types
            val vKey = VerificationKey.unsafeFromByteString(ByteString.fromArray(publicKeyBytes))
            val sKey = SigningKey.unsafeFromByteString(ByteString.fromArray(privateKeyBytes))

            (vKey, sKey)
        }

    /** @return
      *   NodeConfig, such that:
      *   - one node is in the head
      *   - the provided key pair [[vKey]]/[[sKey]] is used for that only node
      *   - the initial equity is not less than [[minEquity]]
      *   - the initial l2 is empty
      *   - Cardano preview is used
      *   - utxos available on l1 VKey's enterprise address will be used for funding the head
      */
    def mkNodeConfig(cardanoNetwork: CardanoNetwork, backend: CardanoBackend[IO])(
        vKey: VerificationKey,
        sKey: SigningKey,
        minEquity: Coin,
    ): IO[NodeConfig] = for {
        _ <- IO.pure(())

        ownHeadWallet = HeadPeerWallet.scalusWallet(HeadPeerNumber.zero, vKey, sKey)
        startTimeInstant <- realTimeQuantizedInstant(cardanoNetwork.slotConfig)
        blockCreationStartTime = BlockCreationStartTime(startTimeInstant)

        headParams = HeadParameters(
          txTiming = TxTiming.demo(cardanoNetwork.slotConfig),
          fallbackContingency = cardanoNetwork.mkFallbackContingencyWithDefaults(
            tallyTxFee = Coin.ada(3),
            voteTxFee = Coin.ada(3)
          ),
          disputeResolutionConfig = DisputeResolutionConfig.default(cardanoNetwork.slotConfig),
          settlementConfig = SettlementConfig(PositiveInt.unsafeApply(100))
        )

        // This is the temporary hard-coded evacuation map - 10 ADA
        // goes to the fee account so the whole Sugar Rush ledger can be
        // correctly evacuated at any point of time.
        feeAccValue = Value.ada(10L)
        evacMap: EvacuationMap = EvacuationMap.apply(
          TreeMap(
            EvacuationKey
                .apply(
                  ByteString.fromHex(
                    "ef779a7b07ef490490ae0039458bccb3e78df0776dbf014e3cf780c600000000"
                  )
                )
                .get ->
                Payout.Obligation
                    .apply(
                      output = KeepRaw.apply(
                        TransactionOutput.apply(
                          address = Address
                              .fromBech32(
                                "addr_test1vrhh0xnmqlh5jpys4cqrj3vteje70r0swakm7q2w8nmcp3sh5wdk4"
                              ),
                          value = feeAccValue
                        )
                      ),
                      network = cardanoNetwork
                    )
                    .toOption
                    .get
          )
        )

        peerAddress = vKey.shelleyAddress()(using cardanoNetwork)
        _ <- logger.info(s"Peer address: ${peerAddress.toBech32.get}")

        // Fetch UTXOs from backend
        utxosResult <- backend.utxosAt(peerAddress)
        utxosMap <- IO.fromEither(
          utxosResult.left.map(err =>
              new RuntimeException(s"Failed to fetch UTXOs from backend: $err")
          )
        )

        // Calculate required value: minEquity + contingency + max non-Plutus tx fee for initialization
        maxNonPlutusTxFee = cardanoNetwork.maxNonPlutusTxFee
        zeroPeerContingency = headParams.fallbackContingency.totalContingencyFor(
          HeadPeerNumber.zero
        )
        requiredValue = Value(minEquity + zeroPeerContingency + maxNonPlutusTxFee)
        _ <- logger.info(
          s"Required value: ${minEquity.value} lovelace (equity) + " +
              s"${zeroPeerContingency} lovelace (peer contingency) + " +
              s"${maxNonPlutusTxFee.value} lovelace (fee) = ${requiredValue.coin.value} lovelace"
        )

        // Select UTXOs that cover the required value
        utxosSelected <- {
            if utxosMap.isEmpty then {
                logger.error(s"No UTXOs found at address ${peerAddress.toBech32.get}") *>
                    IO.raiseError(
                      new RuntimeException(
                        s"No UTXOs available at address ${peerAddress.toBech32.get}. " +
                            s"Please fund this address with at least ${requiredValue.coin.value} lovelace."
                      )
                    )
            } else {
                // Accumulate UTXOs until we have enough value (only ADA, no multi-assets)
                val utxosList = utxosMap.toList.map { case (txIn, txOut) => Utxo(txIn, txOut) }
                val (selected, accumulated) =
                    utxosList.foldLeft((List.empty[Utxo], Value.lovelace(0))) {
                        case (acc @ (selectedUtxos, accValue), utxo) =>
                            if accValue.coin.value >= requiredValue.coin.value then acc
                            else (selectedUtxos :+ utxo, accValue + utxo.output.value)
                    }

                if accumulated.coin.value < requiredValue.coin.value then {
                    val errorMsg =
                        s"Insufficient funds at address ${peerAddress.toBech32.get}. " +
                            s"Available: ${accumulated.coin.value} lovelace, " +
                            s"Required: ${requiredValue.coin.value} lovelace (${minEquity.value} equity + ${maxNonPlutusTxFee.value} fee). " +
                            s"Please fund address ${peerAddress.toBech32.get} with at least ${requiredValue.coin.value - accumulated.coin.value} more lovelace."
                    logger.error(errorMsg) *>
                        IO.raiseError(new RuntimeException(errorMsg))
                } else {
                    NonEmptyList.fromList(selected) match {
                        case Some(nel) =>
                            logger.info(
                              s"Selected ${nel.size} UTXO(s) with total value: ${accumulated.coin.value} lovelace"
                            ) *>
                                IO.pure(nel)
                        case None =>
                            // This should never happen due to the isEmpty check above
                            IO.raiseError(
                              new RuntimeException("Internal error: selected UTXOs list is empty")
                            )
                    }
                }
            }
        }

        seedUtxo = utxosSelected.head
        valueSelected = Value.combine(utxosSelected.map(_.output.value).toList)

        initializationParameters = InitializationParameters(
          initialEvacuationMap = evacMap,
          initialEquityContributions =
              NonEmptyMap(HeadPeerNumber.zero -> minEquity, SortedMap.empty),
          initialSeedUtxo = seedUtxo,
          headId = HeadId(CIP67.HeadTokenNames(seedUtxo.input).treasuryTokenName),
          initialAdditionalFundingUtxos = utxosSelected.tail.map(_.toTuple).toMap,
          initialChangeOutputs = List(
            TransactionOutput.Babbage(
              address = peerAddress,
              value = valueSelected - Value(minEquity) -
                  Value(zeroPeerContingency) - feeAccValue,
              datumOption = None,
              scriptRef = None
            )
          )
        )

        preinit = Preinit(
          cardanoNetwork = cardanoNetwork,
          headParams = headParams,
          headPeers = HeadPeers.apply(List(vKey)).get,
          initializationParams = initializationParameters
        ).get

        endTimeInstant <- IO.realTimeInstant.map(instant =>
            instant.quantize(cardanoNetwork.slotConfig)
        )
        blockCreationEndTime = BlockCreationEndTime(endTimeInstant)

        initTxSeq = InitializationTxSeq
            .Build(preinit)(blockCreationEndTime)
            .result
            .fold(
              e => {
                  logger.error(e.toString).unsafeRunSync()
                  throw e
              },
              x => x
            )

        fallbackTxStartTime = initTxSeq.fallbackTx.fallbackTxStartTime
        forcedMajorBlockTime = headParams.txTiming.forcedMajorBlockTime(fallbackTxStartTime)
        majorBlockWakeupTime = TxTiming.majorBlockWakeupTime(forcedMajorBlockTime, None)

    } yield {
        val initialBlock = InitialBlock(
          Block.MultiSigned.Initial(
            blockBrief = BlockBrief.Initial(
              BlockHeader.Initial(
                startTime = blockCreationStartTime,
                endTime = blockCreationEndTime,
                fallbackTxStartTime = fallbackTxStartTime,
                majorBlockWakeupTime = majorBlockWakeupTime,
                kzgCommitment = evacMap.kzgCommitment
              )
            ),
            effects = BlockEffects.MultiSigned.Initial(
              initializationTx = initTxSeq.initializationTx
                  .focus(_.tx)
                  .modify(ownHeadWallet.signTx),
              fallbackTx = initTxSeq.fallbackTx
                  .focus(_.tx)
                  .modify(ownHeadWallet.signTx)
            )
          )
        )

        val headConfig = HeadConfig(
          headConfigPreinit = preinit,
          initialBlock = initialBlock.initialBlock
        ).get

        NodeConfig(
          headConfig = headConfig,
          ownHeadWallet = ownHeadWallet,
          nodeOperationEvacuationConfig = NodeOperationEvacuationConfig(
            evacuationBotPollingPeriod = 1.minute,
            // NOTE: Reusing the same multisig wallet, in production this should be a different wallet
            evacuationWallet = ownHeadWallet
          ),
          nodeOperationMultisigConfig = NodeOperationMultisigConfig.default,
          // TODO: I assume that these will be pre-populated on preview, pre-prod, and mainnet, and that we'll have
          // a different utility to publish the utxo to a given network.
          // We should probably query these utxos using the backend, and then parse.
          scriptReferenceUtxos = ???
        ).get
    }

end Bootstrap

/** Main entry point for generating a new Ed25519 key pair.
  *
  * Prints the verification key and signing key as hex to stdout.
  */
object GenerateKeyPair extends IOApp:
    override def run(args: List[String]): IO[ExitCode] =
        Bootstrap.generateKeyPair().flatMap { case (vKey, sKey) =>
            // Convert to hex strings
            val vKeyHex = vKey.bytes.toArray.map("%02x".format(_)).mkString
            val sKeyHex = sKey.bytes.toArray.map("%02x".format(_)).mkString
            for {
                _ <- IO.println("Generated new Ed25519 key pair:")
                _ <- IO.println(s"Verification key (32 bytes): $vKeyHex")
                _ <- IO.println(s"Signing key (32 bytes): $sKeyHex")
                _ <- IO.println(
                  s"Testnet address: ${vKey.shelleyAddress()(using CardanoNetwork.Preview).toBech32.get}"
                )

                _ <- IO.println("\nAdd these to your .env file:")
                _ <- IO.println(s"CARDANO_VERIFICATION_KEY=$vKeyHex")
                _ <- IO.println(s"CARDANO_SIGNING_KEY=$sKeyHex")

            } yield ExitCode.Success
        }
end GenerateKeyPair

/** Migrate all funds from the peer's address to a new address.
  *
  * Usage: sbt "runMain hydrozoa.app.Migrate <bech32-address>"
  *
  * Reads configuration from .env (same as Main):
  *   - BLOCKFROST_API_KEY
  *   - CARDANO_VERIFICATION_KEY
  *   - CARDANO_SIGNING_KEY
  */
object Migrate extends IOApp:

    private val logger = Logging.loggerIO("hydrozoa.app.Migrate")

//    /** Load environment configuration from .env file (copied from Main). */
//    private lazy val dotenv = io.github.cdimascio.dotenv.Dotenv.configure().ignoreIfMissing().load()

    override def run(args: List[String]): IO[ExitCode] =
        if args.isEmpty then
            IO.println("Usage: sbt \"runMain hydrozoa.app.Migrate <bech32-address>\"") *>
                IO.println("Example: sbt \"runMain hydrozoa.app.Migrate addr_test1vz...\"") *>
                IO.pure(ExitCode.Error)
        else
            val destinationBech32 = args.head
            migrateAllFunds(destinationBech32)

    private def migrateAllFunds(destinationBech32: String): IO[ExitCode] =
        for {
            _ <- logger.info(s"Starting migration to $destinationBech32")

            // Load environment configuration (same as Main)
            env <- loadEnv

            // Parse destination address
            destination <- IO
                .delay(scalus.cardano.address.Address.fromBech32(destinationBech32))
                .flatMap {
                    case addr: scalus.cardano.address.ShelleyAddress =>
                        logger.info(s"Destination address: ${addr.toBech32.get}") *>
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

            // Create backend
            cardanoNetwork: StandardCardanoNetwork = CardanoNetwork.Preview
            _ <- logger.info("Creating Cardano Blockfrost backend...")

            backend <- CardanoBackendBlockfrost(
              network = Left(cardanoNetwork),
              apiKey = env.blockfrostApiKey
            )

            // Get peer address
            peerAddress = env.verificationKey.shelleyAddress()(using cardanoNetwork)
            _ <- logger.info(s"Peer address: ${peerAddress.toBech32.get}")

            // Fetch all UTXOs from peer address
            _ <- logger.info("Fetching UTXOs from peer address...")
            utxosResult <- backend.utxosAt(peerAddress)
            utxosMap <- IO.fromEither(
              utxosResult.left.map(err => new RuntimeException(s"Failed to fetch UTXOs: $err"))
            )

            _ <-
                if utxosMap.isEmpty then
                    logger.warn("No UTXOs found at peer address. Nothing to migrate.") *>
                        IO.pure(ExitCode.Success)
                else
                    for {
                        _ <- logger.info(s"Found ${utxosMap.size} UTXO(s) to migrate")

                        // Calculate total value
                        totalValue = Value.combine(utxosMap.map((_, output) => output.value))
                        _ <- logger.info(
                          s"Total value to migrate: ${totalValue.coin.value} lovelace + ${totalValue.assets.assets.size} asset(s)"
                        )

                        // Build transaction
                        _ <- logger.info("Building transaction...")
                        unbalanced = TransactionBuilder
                            .build(
                              cardanoNetwork.network,
                              utxosMap.map { case (utxoId, output) =>
                                  Spend(Utxo(utxoId, output))
                              }.toList :+
                                  scalus.cardano.txbuilder.TransactionBuilderStep.Send(
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
                                  throw new RuntimeException(s"Failed to build transaction: $err"),
                              identity
                            )

                        // Balance transaction
                        _ <- logger.info("Balancing transaction...")
                        protocolParams <- backend.fetchLatestParams.flatMap(
                          IO.fromEither(_)
                              .adaptError(err =>
                                  new RuntimeException(s"Failed to fetch protocol params: $err")
                              )
                        )

                        balanced = unbalanced
                            .balanceContext(
                              diffHandler = scalus.cardano.txbuilder.Change.changeOutputDiffHandler(
                                _,
                                _,
                                protocolParams = protocolParams,
                                changeOutputIdx = 0
                              ),
                              protocolParams = protocolParams,
                              evaluator = scalus.cardano.ledger.PlutusScriptEvaluator(
                                cardanoNetwork.cardanoInfo,
                                scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
                              )
                            )
                            .fold(
                              err =>
                                  throw new RuntimeException(
                                    s"Failed to balance transaction: $err"
                                  ),
                              _.transaction
                            )

                        wallet = HeadPeerWallet(
                          HeadPeerNumber.zero,
                          WalletModule.Scalus,
                          env.verificationKey,
                          env.signingKey
                        )

                        // Sign transaction
                        _ <- logger.info("Signing transaction...")
                        signed = wallet.signTx(balanced)

                        _ <- logger.info(s"Transaction hash: ${signed.id}")
                        _ <- logger.info(
                          s"Transaction CBOR: ${HexUtil.encodeHexString(signed.toCbor)}"
                        )

                        // Submit transaction
                        _ <- logger.info("Submitting transaction...")
                        submitResult <- backend.submitTx(signed)
                        _ <- IO.fromEither(
                          submitResult.left.map(err =>
                              new RuntimeException(s"Failed to submit transaction: $err")
                          )
                        )

                        _ <- logger.info("✅ Transaction submitted successfully!")
                        _ <- logger.info(
                          s"Explorer: https://preview.cexplorer.io/tx/${signed.id.toHex}"
                        )
                    } yield ExitCode.Success

        } yield ExitCode.Success

end Migrate
