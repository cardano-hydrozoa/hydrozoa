package hydrozoa.app

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
import scalus.cardano.ledger.Coin
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

/** This generates the full Hydrozoa config out of a couple of
  */
object Bootstrap:

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

        initializationParameters = InitializationParameters(
          headStartTime = startTime,
          initialEvacuationMap = evacMap,
          initialEquityContributions = ???,
          initialSeedUtxo = ???,
          initialAdditionalFundingUtxos = ???,
          initialChangeOutputs = ???
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
