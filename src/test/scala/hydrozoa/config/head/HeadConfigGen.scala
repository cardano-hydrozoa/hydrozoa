package hydrozoa.config.head

import cats.data.{Kleisli, ReaderT, Validated}
import hydrozoa.bootstrap.InitializationFunding
import hydrozoa.config.head.InitParamsType.{BottomUp, Constant, TopDown}
import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters, InitializationParametersGenBottomUp, InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.fallback.generateFallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.parameters.{HeadParameters, generateHeadParameters}
import hydrozoa.config.{ScriptReferenceUtxos, generateScriptReferenceUtxos}
import org.scalacheck.{Prop, Properties}
import test.{GenWithTestPeers, TestPeers, TestPeersSpec, given}

type HeadConfigGen = (
    generateBlockCreationEndTime: GenWithTestPeers[BlockCreationEndTime],
    generateHeadConfigBootstrap: GenWithTestPeers[(HeadConfig.Bootstrap, InitializationFunding)]
) => GenWithTestPeers[HeadConfig]

def generateHeadConfig(
    genHeadConfigBootstrap: GenWithTestPeers[(HeadConfig.Bootstrap, InitializationFunding)] =
        generateHeadConfigBootstrap(),
    generateInitialBlock: (HeadConfig.Bootstrap, InitializationFunding) => GenWithTestPeers[
      InitialBlock
    ] = (bootstrapConfig, funding) => generateInitialBlock(ReaderT.pure((bootstrapConfig, funding)))
): GenWithTestPeers[HeadConfig] =
    for {
        bootstrapAndFunding <- genHeadConfigBootstrap
        (bootstrap, funding) = bootstrapAndFunding
        initialBlock <- generateInitialBlock(bootstrap, funding)
    } yield HeadConfig(
      headConfigBootstrap = bootstrap,
      initialBlock = initialBlock
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
    case Constant(params: InitializationParameters, funding: InitializationFunding)

def generateHeadConfigBootstrap(
    generateHeadParams: GenWithTestPeers[HeadParameters] = generateHeadParameters(),
    generateInitializationParameters: InitParamsType = BottomUp(
      InitializationParametersGenBottomUp.generateInitializationParameters
    ),
    generateScriptReferenceUtxos: GenWithTestPeers[ScriptReferenceUtxos] =
        generateScriptReferenceUtxos,
    coilPeers: CoilPeers = CoilPeers.empty
): GenWithTestPeers[(HeadConfig.Bootstrap, InitializationFunding)] =
    for {
        testPeers <- Kleisli.ask
        cardanoNetwork = testPeers.cardanoNetwork
        headParams <- generateHeadParams
        initializationParamsAndFunding <- generateInitializationParameters match {
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
            case Constant(p, f) => ReaderT.pure((p, f))(using genMonad)
        }
        (initializationParams, funding) = initializationParamsAndFunding
        scriptReferenceUtxos <- generateScriptReferenceUtxos

    } yield {
        val bootstrap = HeadConfig
            .Bootstrap(
              cardanoNetwork = cardanoNetwork,
              headParams = headParams,
              headPeers = testPeers.headPeers,
              coilPeers = coilPeers,
              initializationParams = initializationParams,
              scriptReferenceUtxos = scriptReferenceUtxos
            ) match {
            case Validated.Valid(x) => x
            case Validated.Invalid(errors) =>
                throw RuntimeException(s"generating HeadConfig.Bootstrap failed: $errors")
        }
        (bootstrap, funding)
    }

object HeadConfigTest extends Properties("Head config") {

    val _ = property("bootstrap config generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(TestPeers.generate)
          .flatMap(generateHeadConfigBootstrap().run(_))
          .map(_._1)
    )(_ => true)

    val _ = property("full config generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(TestPeers.generate)
          .flatMap(generateHeadConfig().run(_))
    )(_ => true)
}
