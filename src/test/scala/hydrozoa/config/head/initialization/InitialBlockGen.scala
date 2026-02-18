package hydrozoa.config.head.initialization

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.initialization.HeadStartTimeGen.{HeadStartTimeGen, currentTimeHeadStartTime}
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.{CardanoNetwork, generateStandardCardanoNetwork}
import hydrozoa.config.head.parameters.{GenHeadParams, generateHeadParameters}
import hydrozoa.config.head.peers.{TestPeers, generateTestPeers}
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.kzgCommitment
import monocle.Focus.focus
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Prop, Properties}

def generateInitialBlock(testPeers: TestPeers)(
    generateCardanoNetwork: Gen[CardanoNetwork] = generateStandardCardanoNetwork,
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateHeadParameters: GenHeadParams = generateHeadParameters,
    generateHeadStartTime: HeadStartTimeGen = currentTimeHeadStartTime,
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
        InitializationParametersGenTopDown.GenWithDeps | InitializationParameters =
        InitializationParametersGenBottomUp.generateInitializationParameters
): Gen[InitialBlock] = {
    for {
        cardanoNetwork <- generateCardanoNetwork

        headParams <- generateHeadParameters(cardanoNetwork)(
          generateTxTiming,
          generateFallbackContingency,
          generateDisputeResolutionConfig
        )

        initializationParameters <- generateInitializationParameters match {
            case g: InitializationParametersGenBottomUp.GenInitializationParameters =>
                g(testPeers)(
                  Gen.const(cardanoNetwork),
                  generateHeadStartTime,
                  _ => Gen.const(headParams.fallbackContingency)
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
            case ps: InitializationParameters => Gen.const(ps)
        }

        config = HeadConfig.Preinit.HeadConfig(
          cardanoNetwork = cardanoNetwork,
          headParams = headParams,
          headPeers = testPeers.headPeers,
          initializationParams = initializationParameters
        )

        initTxSeq =
            InitializationTxSeq.Build(config).result match {
                case Left(e) =>
                    throw e
                case Right(x) => x
            }

    } yield InitialBlock(
      Block.MultiSigned.Initial(
        blockBrief = BlockBrief.Initial(
          BlockHeader.Initial(
            startTime = initializationParameters.headStartTime,
            kzgCommitment = initializationParameters.initialL2Utxos.kzgCommitment
          )
        ),
        effects = BlockEffects.MultiSigned.Initial(
          initializationTx = initTxSeq.initializationTx
              .focus(_.tx)
              .modify(testPeers.multisignTx),
          fallbackTx = initTxSeq.fallbackTx
              .focus(_.tx)
              .modify(testPeers.multisignTx)
        )
      )
    )
}

object InitialBlockGenerator extends Properties("Sanity Check") {
    override def overrideParameters(p: Parameters): Parameters = {
        p.withMinSuccessfulTests(1_000)
    }

    val _ = property("Sanity Check") = Prop.forAll(generateTestPeers())(testPeers =>
        Prop.forAll(generateInitialBlock(testPeers)())(_ => true)
    )
}
