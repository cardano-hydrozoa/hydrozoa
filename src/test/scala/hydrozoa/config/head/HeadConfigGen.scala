package hydrozoa.config.head

import hydrozoa.config.head.HeadPeersSpec.{Exact, Random}
import hydrozoa.config.head.initialization.{GenesisUtxosGen, InitializationParametersGenBottomUp, currentTimeHeadStartTime, generateInitialBlock, generateRandomPeersUtxosL1}
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.{CardanoNetwork, generateStandardCardanoNetwork}
import hydrozoa.config.head.parameters.{GenHeadParams, HeadParameters, generateHeadParameters}
import hydrozoa.config.head.peers.{TestPeers, generateTestPeers}
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import org.scalacheck.{Gen, Prop, Properties, Test}
import scalus.cardano.ledger.{Coin, SlotConfig}

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

def generateHeadConfigPreInit(headPeers: HeadPeersSpec)(
    generateCardanoNetwork: Gen[CardanoNetwork] = generateStandardCardanoNetwork,
    generateHeadStartTime: SlotConfig => Gen[QuantizedInstant] = currentTimeHeadStartTime,
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateHeadParameters: GenHeadParams = generateHeadParameters,
    generateGenesisUtxo: GenesisUtxosGen = generateRandomPeersUtxosL1,
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters =
        InitializationParametersGenBottomUp.generateInitializationParameters,
    equityRange: (Coin, Coin) = Coin(5_000_000) -> Coin(500_000_000),
): Gen[HeadConfig.Preinit.HeadConfig] = for {
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
      generateFallbackContingency,
    )

} yield HeadConfig.Preinit.HeadConfig(
  cardanoNetwork = cardanoNetwork,
  headParams = headParams,
  headPeers = testPeers.headPeers,
  initializationParams = initializationParams
)

def generateHeadConfig(headPeers: HeadPeersSpec)(
    generateCardanoNetwork: Gen[CardanoNetwork] = generateStandardCardanoNetwork,
    generateHeadStartTime: SlotConfig => Gen[QuantizedInstant] = currentTimeHeadStartTime,
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateHeadParameters: GenHeadParams = generateHeadParameters,
    generateGenesisUtxo: GenesisUtxosGen = generateRandomPeersUtxosL1,
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters =
        InitializationParametersGenBottomUp.generateInitializationParameters,
    equityRange: (Coin, Coin) = Coin(5_000_000) -> Coin(500_000_000),
): Gen[HeadConfig] =
    for {
        peers <- headPeers.generate
        // we have to re-use this in generating the preinit config and the initial block
        exactPeers = Exact(peers.headPeers.nHeadPeers.toInt)
        preinit <- generateHeadConfigPreInit(exactPeers)(
          generateCardanoNetwork = generateCardanoNetwork,
          generateHeadStartTime = generateHeadStartTime,
          generateTxTiming = generateTxTiming,
          generateFallbackContingency = generateFallbackContingency,
          generateDisputeResolutionConfig = generateDisputeResolutionConfig,
          generateHeadParameters = generateHeadParameters,
          generateGenesisUtxo = generateGenesisUtxo,
          generateInitializationParameters = generateInitializationParameters,
          equityRange = equityRange
        )
        initialBlock <- generateInitialBlock(TestPeers(peers._testPeers.map(_._2).toList))(
          generateCardanoNetwork = Gen.const(preinit.cardanoNetwork),
          generateTxTiming = _ => Gen.const(preinit.headParams.txTiming),
          generateHeadParameters = _ => (_, _, _) => Gen.const(preinit.headParams),
          generateHeadStartTime = _ => Gen.const(preinit.initializationParams.headStartTime),
          generateInitializationParameters =
              (_, _, _, _, _) => Gen.const(preinit.initializationParams)
        )
    } yield HeadConfig(
      cardanoNetwork = preinit.cardanoNetwork,
      headParams = preinit.headParams,
      headPeers = preinit.headPeers,
      initializationParams = preinit.initializationParams,
      initialBlock = initialBlock.initialBlock
    ).get

object HeadConfigGen extends Properties("Sanity Check") {
    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withMinSuccessfulTests(1000)

    val _ = property("head config generator doesn't throw") =
        Prop.forAll(generateHeadConfig(Random)())(_ => true)
}
