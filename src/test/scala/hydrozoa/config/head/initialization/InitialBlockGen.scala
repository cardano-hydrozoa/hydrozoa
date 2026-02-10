package hydrozoa.config.head.initialization

import cats.data.NonEmptyList
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.fallback.FallbackContingency.mkFallbackContingencyWithDefaults
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.{CardanoNetwork, cardanoNetworkGenerator}
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.kzgCommitment
import org.scalacheck.Test.Parameters
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Prop, Properties}
import scalus.cardano.ledger.Coin

import scala.concurrent.duration.DurationInt
import test.{TestPeer, testPeersGenerator}

/** Hardcoded values:
  *   - fallback contingency made with defaults and max plutus tx fee for vote + tally
  *   - default tx timing
  *   - 24 voting duration
  * @param testPeers
  * @param generateCardanoNetwork
  * @param generateInitializationParameters
  * @param generateFallbackContingency
  * @return
  */
def generateInitialBlock(testPeers: NonEmptyList[TestPeer])(
    generateCardanoNetwork: Option[Gen[CardanoNetwork]],
    generateInitializationParameters: Option[Gen[InitializationParameters]],
): Gen[InitialBlock] = {
    for {
        // These are the 5 sections needed for a InitializationTxOps.Config
        cardanoNetwork <- generateCardanoNetwork.getOrElse(cardanoNetworkGenerator)
        headPeers = testPeers.asHeadPeers
        fallbackContingency = cardanoNetwork.mkFallbackContingencyWithDefaults(
          Coin.ada(3), Coin.ada(3)
        )
        txTiming = TxTiming.default(cardanoNetwork.slotConfig)
        initializationParameters <- generateInitializationParameters
            .getOrElse(
              initializationParametersGen(testPeers)(
                generateCardanoNetwork = Some(Gen.const(cardanoNetwork))
              )
            )
        config = HeadConfig.Preinit.HeadConfig(
          cardanoNetwork = cardanoNetwork,
          headParams = HeadParameters(
            txTiming = txTiming,
            fallbackContingency = fallbackContingency,
            disputeResolutionConfig = DisputeResolutionConfig(
              QuantizedTime.QuantizedFiniteDuration(cardanoNetwork.slotConfig, 24.hours)
            )
          ),
          headPeers = headPeers,
          initializationParams = initializationParameters
        ) // Should be of type InitializationTxOps.Config, by the object is private

        initTxSeq =
            InitializationTxSeq.Build(config).result match {
                case Left(e) =>
                    throw e
                case Right(x) => x
            }

    } yield InitialBlock(
      Block.MultiSigned.Initial(
        blockBrief = BlockBrief.Initial(
          BlockHeader.Initial(
            startTime = initializationParameters.headStartTime,
            kzgCommitment = initializationParameters.initialL2Utxos.kzgCommitment
          )
        ),
        effects = BlockEffects.MultiSigned.Initial(
          initializationTx = initTxSeq.initializationTx,
          fallbackTx = initTxSeq.fallbackTx
        )
      )
    )
}

object InitialBlockGenerator extends Properties("Sanity Check") {
  override def overrideParameters(p: Parameters): Parameters = {
    p.withInitialSeed(Seed.fromBase64("ShMPS6wOudyfOt-X0UCllm6t-1R2OS3gG6HXxf21dhI=").get)
  }

    val _ = property("Sanity Check") = Prop.forAll(testPeersGenerator())(testPeers =>
        Prop.forAll(
          generateInitialBlock(testPeers)(
            None,
            None
          )
        )(_ => true)
    )
}
