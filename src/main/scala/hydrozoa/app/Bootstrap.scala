package hydrozoa.app

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.effect.{ExitCode, IO, IOApp}
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.HeadConfig.Preinit
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.fallback.FallbackContingency.mkFallbackContingencyWithDefaults
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.config.node.NodeConfig
import hydrozoa.config.node.operation.liquidation.NodeOperationLiquidationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.lib.cardano.scalus.ShelleyAddressExtra
import hydrozoa.lib.logging.Logging
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, HeadPeerWallet}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import java.security.SecureRandom
import monocle.Focus.focus
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import scala.collection.immutable.SortedMap
import scalus.cardano.ledger.{Coin, TransactionOutput, Utxo, Value}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

/** This generates the full Hydrozoa config out of a couple of
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
        // headConfig = ???
        ownHeadWallet = HeadPeerWallet.scalusWallet(HeadPeerNumber.zero, vKey, sKey)
        startTime <- IO.realTimeInstant.map(instant => instant.quantize(cardanoNetwork.slotConfig))

        headParams = HeadParameters(
          txTiming = TxTiming.demo(cardanoNetwork.slotConfig),
          fallbackContingency = cardanoNetwork.mkFallbackContingencyWithDefaults(
            tallyTxFee = Coin.ada(3),
            voteTxFee = Coin.ada(3)
          ),
          disputeResolutionConfig = DisputeResolutionConfig.default(cardanoNetwork.slotConfig),
          settlementConfig = SettlementConfig(PositiveInt.unsafeApply(100))
        )

        evacMap = EvacuationMap.empty

        peerAddress = ShelleyAddressExtra.mkShelleyAddress(vKey, cardanoNetwork.network)
        _ <- logger.info(s"Peer address: ${peerAddress.toBech32.get}")

        // Fetch UTXOs from backend
        utxosResult <- backend.utxosAt(peerAddress)
        utxosMap <- IO.fromEither(
          utxosResult.left.map(err =>
              new RuntimeException(s"Failed to fetch UTXOs from backend: $err")
          )
        )

        // Calculate required value: minEquity + max non-Plutus tx fee for initialization
        maxNonPlutusTxFee = cardanoNetwork.maxNonPlutusTxFee
        requiredValue = Value(minEquity + maxNonPlutusTxFee)
        _ <- logger.info(
          s"Required value: ${minEquity.value} lovelace (equity) + ${maxNonPlutusTxFee.value} lovelace (fee) = ${requiredValue.coin.value} lovelace"
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

        valueSelected = Value.combine(utxosSelected.map(_.output.value).toList)

        initializationParameters = InitializationParameters(
          headStartTime = startTime,
          initialEvacuationMap = evacMap,
          initialEquityContributions =
              NonEmptyMap(HeadPeerNumber.zero -> minEquity, SortedMap.empty),
          initialSeedUtxo = utxosSelected.head,
          initialAdditionalFundingUtxos = utxosSelected.tail.map(_.toTuple).toMap,
          initialChangeOutputs = List(
            TransactionOutput.Babbage(
              address = peerAddress,
              value = valueSelected - Value(minEquity) - Value(
                headParams.fallbackContingency.totalContingencyFor(HeadPeerNumber.zero)
              ),
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

        initTxSeq = InitializationTxSeq.Build(preinit).result.fold(e => throw e, x => x)
    } yield {

        val initialBlock = InitialBlock(
          Block.MultiSigned.Initial(
            blockBrief = BlockBrief.Initial(
              BlockHeader.Initial(
                startTime = startTime,
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
          nodeOperationLiquidationConfig = NodeOperationLiquidationConfig.default,
          nodeOperationMultisigConfig = NodeOperationMultisigConfig.default
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
                _ <- IO.println("\nAdd these to your .env file:")
                _ <- IO.println(s"CARDANO_VERIFICATION_KEY=$vKeyHex")
                _ <- IO.println(s"CARDANO_SIGNING_KEY=$sKeyHex")
            } yield ExitCode.Success
        }
end GenerateKeyPair
