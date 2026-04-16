package hydrozoa.config.head

import cats.data.{Kleisli, ReaderT, Validated}
import hydrozoa.config.head.InitParamsType.{BottomUp, Constant, TopDown}
import hydrozoa.config.head.initialization.BlockCreationEndTimeGen.currentTimeBlockCreationEndTime
import hydrozoa.config.head.initialization.{InitializationParameters, InitializationParametersGenBottomUp, InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.fallback.generateFallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.parameters.{HeadParameters, generateHeadParameters}
import org.scalacheck.{Prop, Properties}
import test.{GenWithTestPeers, TestPeers, TestPeersSpec, given}
import hydrozoa.config.head.multisig.timing.generateDefaultTxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.{GenHeadParams, generateHeadParameters}
import hydrozoa.config.head.rulebased.dispute.generateDisputeResolutionConfig
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.ledger.{Hash32, SlotConfig}
import test.{TestPeers, TestPeersSpec}

type HeadConfigGen = (
    generateBlockCreationEndTime: GenWithTestPeers[BlockCreationEndTime],
    generateHeadParameters: GenWithTestPeers[HeadParameters],
    generateInitializationParameters: InitParamsType
) => GenWithTestPeers[HeadConfig]

def generateHeadConfig(
    generateBlockCreationEndTime: GenWithTestPeers[BlockCreationEndTime] =
        currentTimeBlockCreationEndTime,
    generateHeadParameters: GenWithTestPeers[HeadParameters] = generateHeadParameters(),
    generateInitializationParameters: InitParamsType = BottomUp(
      InitializationParametersGenBottomUp.generateInitializationParameters
    )
): GenWithTestPeers[HeadConfig] =
    for {
        preinit <- generateHeadConfigPreInit(
          generateHeadParams = generateHeadParameters,
          generateInitializationParameters = generateInitializationParameters
        )
        initialBlock <- generateInitialBlock(
          generateBlockCreationEndTime = generateBlockCreationEndTime,
          generateInitializationParameters = Constant(preinit.initializationParams)
        )
    } yield HeadConfig(
      cardanoNetwork = preinit.cardanoNetwork,
      headParams = preinit.headParams,
      headPeers = preinit.headPeers,
      initializationParams = preinit.initializationParams,
      initialBlock = initialBlock.initialBlock
    ) match {
        case Validated.Valid(x) => x
        case Validated.Invalid(errors) =>
            throw RuntimeException(s"HeadConfig generation failed: $errors")
    }

enum InitParamsType:
    case BottomUp(
        initializationParametersGenBottomUp: InitializationParametersGenBottomUp.GenInitializationParameters
    )
    case TopDown(initializationParametersGenTopDown: InitializationParametersGenTopDown.GenWithDeps)
    case Constant(params: InitializationParameters)

def generateHeadConfigPreInit(
    generateHeadParams: GenWithTestPeers[HeadParameters] = generateHeadParameters(),
    generateInitializationParameters: InitParamsType =
        BottomUp(InitializationParametersGenBottomUp.generateInitializationParameters),
): GenWithTestPeers[HeadConfig.Preinit] =
    for {
        testPeers <- Kleisli.ask
        cardanoNetwork = testPeers.cardanoNetwork
        headParams <- generateHeadParams
        initializationParams <- generateInitializationParameters match {
            case BottomUp(g) =>
                g(generateFallbackContingency)
            case TopDown(InitializationParametersGenTopDown.GenWithDeps(
                  generator,
                  generateGenesisUtxosL1,
                  equityRange
                )) =>
                generator(
                  generateFallbackContingency,
                  generateGenesisUtxosL1,
                  equityRange
                )
            case Constant(p) => ReaderT.pure(p)
        }

} yield HeadConfig
    .Preinit(
      cardanoNetwork = cardanoNetwork,
      headParams = headParams,
      headPeers = testPeers.mkHeadPeers,
      initializationParams = initializationParams
    ) match {
    case Validated.Valid(x) => x
        case Validated.Invalid(errors) =>
        throw RuntimeException(s"generating HeadConfig.Preinit failed: $errors")
}

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
