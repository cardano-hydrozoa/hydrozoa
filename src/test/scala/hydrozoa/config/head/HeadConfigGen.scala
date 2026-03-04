package hydrozoa.config.head

import hydrozoa.config.head.initialization.HeadStartTimeGen.currentTimeHeadStartTime
import hydrozoa.config.head.initialization.{InitializationParametersGenBottomUp, InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.settlement.{SettlementConfigGen, generateSettlementConfig}
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.{GenHeadParams, generateHeadParameters}
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import org.scalacheck.{Gen, Prop, Properties}
import scalus.cardano.ledger.SlotConfig
import test.{TestPeers, TestPeersSpec}

type HeadConfigGen =
    (testPeers: TestPeers) => (
        generateHeadStartTime: SlotConfig => Gen[QuantizedInstant],
        generateTxTiming: TxTimingGen,
        generateFallbackContingency: FallbackContingencyGen,
        generateDisputeResolutionConfig: DisputeResolutionConfigGen,
        generateHeadParameters: GenHeadParams,
        generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
            InitializationParametersGenTopDown.GenWithDeps
    ) => Gen[HeadConfig]

def generateHeadConfig(testPeers: TestPeers)(
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
        preinit <- generateHeadConfigPreInit(testPeers)(
          generateHeadStartTime = generateHeadStartTime,
          generateTxTiming = generateTxTiming,
          generateFallbackContingency = generateFallbackContingency,
          generateDisputeResolutionConfig = generateDisputeResolutionConfig,
          generateHeadParameters = generateHeadParameters,
          generateInitializationParameters = generateInitializationParameters
        )
        initialBlock <- generateInitialBlock(testPeers)(
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

def generateHeadConfigPreInit(testPeers: TestPeers)(
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
    cardanoNetwork <- testPeers.network.generateStandardCardanoNetwork
    headParams <- generateHeadParameters(cardanoNetwork)(
      generateTxTiming,
      generateFallbackContingency,
      generateDisputeResolutionConfig,
      generateSettlementConfig
    )
    initializationParams <- generateInitializationParameters match {
        case g: InitializationParametersGenBottomUp.GenInitializationParameters =>
            g(testPeers)(
              generateHeadStartTime,
              generateFallbackContingency,
            )
        case InitializationParametersGenTopDown.GenWithDeps(
              generator,
              generateGenesisUtxosL1,
              equityRange
            ) =>
            generator(testPeers)(
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
      headPeers = testPeers.mkHeadPeers,
      initializationParams = initializationParams
    )
    .get

object HeadConfigGen extends Properties("Head config") {
    val _ = property("pre-init config generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(TestPeers.generate)
          .flatMap(generateHeadConfigPreInit(_)())
    )(_ => true)

    val _ = property("full config generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(TestPeers.generate)
          .flatMap(generateHeadConfig(_)())
    )(_ => true)
}
