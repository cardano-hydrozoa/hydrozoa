package hydrozoa.config.head

import cats.data.{Kleisli, ReaderT}
import hydrozoa.config.head.initialization.BlockCreationEndTimeGen.{BlockCreationEndTimeGen, currentTimeBlockCreationEndTime}
import hydrozoa.config.head.initialization.{InitializationParametersGenBottomUp, InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.fallback.generateFallbackContingency
import hydrozoa.config.head.parameters.{GenHeadParams, generateHeadParameters}
import org.scalacheck.{Gen, Prop, Properties}
import test.given
import test.{TestPeers, TestPeersSpec}

type HeadConfigGen = (
    generateBlockCreationEndTime: BlockCreationEndTimeGen,
    generateHeadParameters: GenHeadParams,
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
        InitializationParametersGenTopDown.GenWithDeps
) => ReaderT[Gen, TestPeers, HeadConfig]

def generateHeadConfig(
    generateBlockCreationEndTime: BlockCreationEndTimeGen = currentTimeBlockCreationEndTime,
    generateHeadParameters: GenHeadParams = generateHeadParameters(),
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
        InitializationParametersGenTopDown.GenWithDeps =
        InitializationParametersGenBottomUp.generateInitializationParameters
): ReaderT[Gen, TestPeers, HeadConfig] =
    for {
        preinit <- generateHeadConfigPreInit(
          generateHeadParams = generateHeadParameters,
          generateInitializationParameters = generateInitializationParameters
        )
        initialBlock <- generateInitialBlock(
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

def generateHeadConfigPreInit(
    generateHeadParams: GenHeadParams = generateHeadParameters(),
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
        InitializationParametersGenTopDown.GenWithDeps =
        InitializationParametersGenBottomUp.generateInitializationParameters,
): ReaderT[Gen, TestPeers, HeadConfig.Preinit] =
    for {
        testPeers <- Kleisli.ask
        cardanoNetwork = testPeers.cardanoNetwork
        headParams <- generateHeadParams
        initializationParams <- generateInitializationParameters match {
            case g: InitializationParametersGenBottomUp.GenInitializationParameters =>
                g(generateFallbackContingency)
            case InitializationParametersGenTopDown.GenWithDeps(
                  generator,
                  generateGenesisUtxosL1,
                  equityRange
                ) =>
                generator(
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

object HeadConfigTest extends Properties("Head config") {

    val _ = property("pre-init config generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(TestPeers.generate)
          .flatMap(generateHeadConfigPreInit().run(_))
    )(_ => true)

    val _ = property("full config generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(TestPeers.generate)
          .flatMap(generateHeadConfig().run(_))
    )(_ => true)
}
