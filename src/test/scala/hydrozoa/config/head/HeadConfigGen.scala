package hydrozoa.config.head

import hydrozoa.config.head.HeadPeersSpec.{Exact, Random}
import hydrozoa.config.head.initialization.HeadStartTimeGen.currentTimeHeadStartTime
import hydrozoa.config.head.initialization.{InitializationParametersGenBottomUp, InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.settlement.{SettlementConfigGen, generateSettlementConfig}
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.{CardanoNetwork, generateStandardCardanoNetwork}
import hydrozoa.config.head.parameters.{GenHeadParams, HeadParameters, generateHeadParameters}
import hydrozoa.config.head.peers.{TestPeers, generateTestPeers}
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import org.scalacheck.{Gen, Prop, Properties, Test}
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

def generateHeadConfigPreInit(headPeers: HeadPeersSpec)(
    generateCardanoNetwork: Gen[CardanoNetwork] = generateStandardCardanoNetwork,
    generateHeadStartTime: SlotConfig => Gen[QuantizedInstant] = currentTimeHeadStartTime,
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateHeadParameters: GenHeadParams = generateHeadParameters,
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
        InitializationParametersGenTopDown.GenWithDeps =
        InitializationParametersGenBottomUp.generateInitializationParameters,
    generateSettlementConfig: SettlementConfigGen = generateSettlementConfig
): Gen[HeadConfig.Preinit] = for {
    testPeers <- headPeers.generate
    cardanoNetwork <- generateCardanoNetwork
    headParams <- generateHeadParameters(cardanoNetwork)(
      generateTxTiming,
      generateFallbackContingency,
      generateDisputeResolutionConfig,
      generateSettlementConfig
    )
    initializationParams <- generateInitializationParameters match {
        case g: InitializationParametersGenBottomUp.GenInitializationParameters =>
            g(testPeers)(
              Gen.const(cardanoNetwork),
              generateHeadStartTime,
              generateFallbackContingency,
            )
        case InitializationParametersGenTopDown.GenWithDeps(
              generator,
              generateGenesisUtxosL1,
              equityRange
            ) =>
            generator(testPeers)(
              Gen.const(cardanoNetwork),
              generateHeadStartTime,
              generateFallbackContingency,
              generateGenesisUtxosL1,
              equityRange
            )
    }

} yield HeadConfig
    .Preinit(
      cardanoNetwork = cardanoNetwork,
      headParams = headParams,
      headPeers = testPeers.headPeers,
      initializationParams = initializationParams
    )
    .get

def generateHeadConfig(headPeers: HeadPeersSpec)(
    generateCardanoNetwork: Gen[CardanoNetwork] = generateStandardCardanoNetwork,
    generateHeadStartTime: SlotConfig => Gen[QuantizedInstant] = currentTimeHeadStartTime,
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateHeadParameters: GenHeadParams = generateHeadParameters,
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
        InitializationParametersGenTopDown.GenWithDeps =
        InitializationParametersGenBottomUp.generateInitializationParameters
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
          generateInitializationParameters = generateInitializationParameters
        )
        initialBlock <- generateInitialBlock(TestPeers(peers._testPeers.map(_._2).toList))(
          generateCardanoNetwork = Gen.const(preinit.cardanoNetwork),
          generateTxTiming = _ => Gen.const(preinit.headParams.txTiming),
          generateHeadParameters = _ => (_, _, _, _) => Gen.const(preinit.headParams),
          generateHeadStartTime = _ => Gen.const(preinit.initializationParams.headStartTime),
          generateInitializationParameters = preinit.initializationParams
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
