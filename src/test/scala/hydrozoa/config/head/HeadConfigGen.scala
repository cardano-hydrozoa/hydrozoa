package hydrozoa.config.head

import cats.data.{Kleisli, ReaderT, Validated}
import hydrozoa.config.head.InitParamsType.{BottomUp, Constant, TopDown}
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters, InitializationParametersGenBottomUp, InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.fallback.generateFallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.parameters.{HeadParameters, generateHeadParameters}
import hydrozoa.config.{ScriptReferenceUtxos, generateScriptReferenceUtxos}
import org.scalacheck.{Prop, Properties}
import test.{GenWithTestPeers, TestPeers, TestPeersSpec, given}

type HeadConfigGen = (
    generateBlockCreationEndTime: GenWithTestPeers[BlockCreationEndTime],
    generateHeadConfigBootstrap: GenWithTestPeers[HeadConfig.Bootstrap]
) => GenWithTestPeers[HeadConfig]

def generateHeadConfig(
    genHeadConfigBootstrap: GenWithTestPeers[HeadConfig.Bootstrap] = generateHeadConfigBootstrap(),
    generateInitialBlock: HeadConfig.Bootstrap => GenWithTestPeers[InitialBlock] =
        bootstrapConfig => generateInitialBlock(ReaderT.pure(bootstrapConfig))
): GenWithTestPeers[HeadConfig] =
    for {
        bootstrap <- genHeadConfigBootstrap
        initialBlock <- generateInitialBlock(bootstrap)
    } yield HeadConfig(
      headConfigBootstrap = bootstrap,
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

def generateHeadConfigBootstrap(
    generateHeadParams: GenWithTestPeers[HeadParameters] = generateHeadParameters(),
    generateInitializationParameters: InitParamsType = BottomUp(
      InitializationParametersGenBottomUp.generateInitializationParameters
    ),
    generateScriptReferenceUtxos: GenWithTestPeers[ScriptReferenceUtxos] =
        generateScriptReferenceUtxos
): GenWithTestPeers[HeadConfig.Bootstrap] =
    for {
        testPeers <- Kleisli.ask
        cardanoNetwork = testPeers.cardanoNetwork
        headParams <- generateHeadParams
        initializationParams <- generateInitializationParameters match {
            case BottomUp(g) =>
                g(generateFallbackContingency)
            case TopDown(
                  InitializationParametersGenTopDown.GenWithDeps(
                    generator,
                    generateGenesisUtxosL1,
                    equityRange
                  )
                ) =>
                generator(
                  generateFallbackContingency,
                  generateGenesisUtxosL1,
                  equityRange
                )
            case Constant(p) => ReaderT.pure(p)(using genMonad)
        }
        scriptReferenceUtxos <- generateScriptReferenceUtxos

    } yield HeadConfig
        .Bootstrap(
          cardanoNetwork = cardanoNetwork,
          headParams = headParams,
          headPeers = testPeers.headPeers,
          coilPeers = List.empty,
          initializationParams = initializationParams,
          scriptReferenceUtxos = scriptReferenceUtxos
        ) match {
        case Validated.Valid(x) => x
        case Validated.Invalid(errors) =>
            throw RuntimeException(s"generating HeadConfig.Bootstrap failed: $errors")
    }

object HeadConfigTest extends Properties("Head config") {

    val _ = property("bootstrap config generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(TestPeers.generate)
          .flatMap(generateHeadConfigBootstrap().run(_))
    )(_ => true)

    val _ = property("full config generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(TestPeers.generate)
          .flatMap(generateHeadConfig().run(_))
    )(_ => true)
}
