package hydrozoa.config.head

import hydrozoa.config.head.InitParamsType.{BottomUp, Constant}
import hydrozoa.config.head.initialization.BlockCreationEndTimeGen.currentTimeBlockCreationEndTime
import hydrozoa.config.head.initialization.{InitializationParameters, InitializationParametersGenBottomUp, InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.settlement.{SettlementConfigGen, generateSettlementConfig}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.{GenHeadParams, generateHeadParameters}
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import org.scalacheck.{Gen, Prop, Properties}
import scalus.cardano.ledger.SlotConfig
import test.{TestPeers, TestPeersSpec}

type HeadConfigGen =
    (testPeers: TestPeers) => (
        generateBlockCreationEndTime: SlotConfig => Gen[BlockCreationEndTime],
        generateTxTiming: TxTimingGen,
        generateFallbackContingency: FallbackContingencyGen,
        generateDisputeResolutionConfig: DisputeResolutionConfigGen,
        generateHeadParameters: GenHeadParams,
        generateInitializationParameters: InitParamsType
    ) => Gen[HeadConfig]

def generateHeadConfig(testPeers: TestPeers)(
    generateBlockCreationEndTime: SlotConfig => Gen[BlockCreationEndTime] =
        currentTimeBlockCreationEndTime,
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateHeadParameters: GenHeadParams = generateHeadParameters,
    generateInitializationParameters: InitParamsType =
        BottomUp(InitializationParametersGenBottomUp.generateInitializationParameters)
): Gen[HeadConfig] =
    for {
        preinit <- generateHeadConfigPreInit(testPeers)(
          generateTxTiming = generateTxTiming,
          generateFallbackContingency = generateFallbackContingency,
          generateDisputeResolutionConfig = generateDisputeResolutionConfig,
          generateHeadParameters = generateHeadParameters,
          generateInitializationParameters = generateInitializationParameters
        )
        initialBlock <- generateInitialBlock(testPeers)(
          generateTxTiming = _ => Gen.const(preinit.headParams.txTiming),
          generateHeadParameters = _ => (_, _, _, _) => Gen.const(preinit.headParams),
          generateBlockCreationEndTime = generateBlockCreationEndTime,
          generateInitializationParameters = Constant(preinit.initializationParams)
        )
    } yield HeadConfig(
      cardanoNetwork = preinit.cardanoNetwork,
      headParams = preinit.headParams,
      headPeers = preinit.headPeers,
      initializationParams = preinit.initializationParams,
      initialBlock = initialBlock.initialBlock
    ).get

enum InitParamsType:
    case BottomUp(initializationParametersGenBottomUp: InitializationParametersGenBottomUp.GenInitializationParameters)
    case TopDown(initializationParametersGenTopDown: InitializationParametersGenTopDown.GenWithDeps)
    case Constant(params : InitializationParameters)

def generateHeadConfigPreInit(testPeers: TestPeers)(
    generateBlockCreationEndTime: SlotConfig => Gen[BlockCreationEndTime] =
        currentTimeBlockCreationEndTime,
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateHeadParameters: GenHeadParams = generateHeadParameters,
    generateInitializationParameters: InitParamsType =
        InitParamsType.BottomUp(InitializationParametersGenBottomUp.generateInitializationParameters),
    generateSettlementConfig: SettlementConfigGen = generateSettlementConfig
): Gen[HeadConfig.Preinit] = for {
    cardanoNetwork <- Gen.const(testPeers.network)
    headParams <- generateHeadParameters(cardanoNetwork)(
      generateTxTiming,
      generateFallbackContingency,
      generateDisputeResolutionConfig,
      generateSettlementConfig
    )
    initializationParams <- generateInitializationParameters match {
        case  InitParamsType.BottomUp(g) =>
            g(testPeers)(
              generateFallbackContingency,
            )
        case InitParamsType.TopDown(InitializationParametersGenTopDown.GenWithDeps(
              generator,
              generateGenesisUtxosL1,
              equityRange
            )) =>
            generator(testPeers)(
              generateFallbackContingency,
              generateGenesisUtxosL1,
              equityRange
            )
        case Constant(p) => Gen.const(p)
    }

} yield HeadConfig
    .Preinit(
      cardanoNetwork = cardanoNetwork,
      headParams = headParams,
      headPeers = testPeers.mkHeadPeers,
      initializationParams = initializationParams
    )
    .get

object HeadConfigTest extends Properties("Head config") {

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
