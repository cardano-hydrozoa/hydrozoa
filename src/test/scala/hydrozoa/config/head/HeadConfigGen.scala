package hydrozoa.config.head

import hydrozoa.config.head.initialization.BlockCreationEndTimeGen.{BlockCreationEndTimeGen, currentTimeBlockCreationEndTime}
import hydrozoa.config.head.initialization.{InitializationParametersGenBottomUp, InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency, mkFallbackContingency}
import hydrozoa.config.head.multisig.settlement.{SettlementConfigGen, generateSettlementConfig}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.{GenHeadParams, HeadParameters, generateHeadParameters}
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import org.scalacheck.{Gen, Prop, Properties}
import scalus.cardano.ledger.SlotConfig
import test.{TestPeers, TestPeersSpec}

type HeadConfigGen =
    (testPeers: TestPeers) => (
        generateBlockCreationEndTime: BlockCreationEndTimeGen,
        generateHeadParameters: GenHeadParams,
        generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
            InitializationParametersGenTopDown.GenWithDeps
    ) => Gen[HeadConfig]

def generateHeadConfig(testPeers: TestPeers)(
    generateBlockCreationEndTime: BlockCreationEndTimeGen =
        currentTimeBlockCreationEndTime,
    generateHeadParameters: GenHeadParams = generateHeadParameters(),
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
        InitializationParametersGenTopDown.GenWithDeps =
        InitializationParametersGenBottomUp.generateInitializationParameters
): Gen[HeadConfig] =
    for {
        preinit <- generateHeadConfigPreInit(testPeers)(
          generateHeadParams = generateHeadParameters,
          generateInitializationParameters = generateInitializationParameters
        )
        initialBlock <- generateInitialBlock(testPeers)(
          generateBlockCreationEndTime = generateBlockCreationEndTime,
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
    generateHeadParams : GenHeadParams = generateHeadParameters(),
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
        InitializationParametersGenTopDown.GenWithDeps =
        InitializationParametersGenBottomUp.generateInitializationParameters,
): Gen[HeadConfig.Preinit] = for {
    cardanoNetwork <- Gen.const(testPeers.network)
    headParams <- generateHeadParams
    initializationParams <- generateInitializationParameters match {
        case g: InitializationParametersGenBottomUp.GenInitializationParameters =>
            g(testPeers)(
              generateFallbackContingency,
            )
        case InitializationParametersGenTopDown.GenWithDeps(
              generator,
              generateGenesisUtxosL1,
              equityRange
            ) =>
            generator(testPeers)(
              generateFallbackContingency,
              generateGenesisUtxosL1,
              equityRange
            )
    }

} yield HeadConfig
    .Preinit(
      cardanoNetwork = cardanoNetwork,
      headParams = headParams(using cardanoNetwork),
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
