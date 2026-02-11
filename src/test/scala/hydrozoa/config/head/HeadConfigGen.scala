package hydrozoa.config.head

import hydrozoa.config.head.initialization.{GenInitializationParameters, currentTimeHeadStartTime, generateInitialBlock, generateInitializationParameters}
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.{CardanoNetwork, generateStandardCardanoNetwork}
import hydrozoa.config.head.parameters.{GenHeadParams, generateHeadParameters}
import hydrozoa.config.head.peers.{TestPeers, generateTestPeers}
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import org.scalacheck.Gen
import scalus.cardano.ledger.SlotConfig

enum HeadPeersSpec:
    case Random
    case Ranged(minPeers: Int, maxPeers: Int)
    case Exact(peers: Int)

    import HeadPeersSpec.*

    def generate: Gen[TestPeers] = this match {
        case Random                     => generateTestPeers()
        case Ranged(minPeers, maxPeers) => generateTestPeers(minPeers, maxPeers)
        case Exact(peers)               => generateTestPeers(peers, peers)
    }

def generateHeadConfig(headPeers: HeadPeersSpec)(
    generateCardanoNetwork: Gen[CardanoNetwork] = generateStandardCardanoNetwork,
    generateHeadStartTime: SlotConfig => Gen[QuantizedInstant] = currentTimeHeadStartTime,
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateHeadParameters: GenHeadParams = generateHeadParameters,
    generateInitializationParameters: GenInitializationParameters =
        generateInitializationParameters,
): Gen[HeadConfig] = for {
    testPeers <- headPeers.generate
    cardanoNetwork <- generateCardanoNetwork
    headParams <- generateHeadParameters(cardanoNetwork)(
      generateTxTiming,
      generateFallbackContingency,
      generateDisputeResolutionConfig
    )
    initializationParams <- generateInitializationParameters(testPeers)(
      Gen.const(cardanoNetwork),
      generateHeadStartTime,
      generateFallbackContingency
    )
    initialBlock <- generateInitialBlock(testPeers)(
      generateCardanoNetwork = Gen.const(cardanoNetwork),
      generateTxTiming = _ => Gen.const(headParams.txTiming),
      generateHeadParameters = _ => (_, _, _) => Gen.const(headParams),
      generateHeadStartTime = _ => Gen.const(initializationParams.headStartTime),
      generateInitializationParameters = (_, _, _) => Gen.const(initializationParams)
    )
} yield HeadConfig(
  cardanoNetwork = cardanoNetwork,
  headParams = headParams,
  headPeers = testPeers.headPeers,
  initialBlock = initialBlock.initialBlock,
  initializationParams = initializationParams
).get
