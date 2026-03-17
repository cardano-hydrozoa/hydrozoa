package hydrozoa.config.head.initialization

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.initialization.BlockCreationEndTimeGen.{BlockCreationEndTimeGen, currentTimeBlockCreationEndTime}
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.settlement.{SettlementConfigGen, generateSettlementConfig}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationStartTime
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.{GenHeadParams, generateHeadParameters}
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import monocle.Focus.focus
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Prop, Properties}
import scala.concurrent.duration.DurationInt
import test.{TestPeers, TestPeersSpec}

def generateInitialBlock(testPeers: TestPeers)(
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateHeadParameters: GenHeadParams = generateHeadParameters,
    generateBlockCreationEndTime: BlockCreationEndTimeGen = currentTimeBlockCreationEndTime,
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
        InitializationParametersGenTopDown.GenWithDeps | InitializationParameters =
        InitializationParametersGenBottomUp.generateInitializationParameters,
    generateSettlementConfig: SettlementConfigGen = generateSettlementConfig
): Gen[InitialBlock] = {
    for {
        cardanoNetwork <- Gen.const(testPeers.network)

        headParams <- generateHeadParameters(cardanoNetwork)(
          generateTxTiming,
          generateFallbackContingency,
          generateDisputeResolutionConfig,
          generateSettlementConfig
        )

        initializationParameters <- generateInitializationParameters match {
            case g: InitializationParametersGenBottomUp.GenInitializationParameters =>
                g(testPeers)(_ => Gen.const(headParams.fallbackContingency))
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
            case ps: InitializationParameters => Gen.const(ps)
        }

        config = HeadConfig
            .Preinit(
              cardanoNetwork = cardanoNetwork,
              headParams = headParams,
              headPeers = testPeers.mkHeadPeers,
              initializationParams = initializationParameters
            )
            .get

        blockCreationEndTime <- generateBlockCreationEndTime(config.slotConfig)

        initTxSeq =
            InitializationTxSeq.Build(config)(blockCreationEndTime).result match {
                case Left(e) =>
                    throw new RuntimeException(e.toString, e)
                case Right(x) => x
            }

    } yield InitialBlock(
      Block.MultiSigned.Initial(
        blockBrief = BlockBrief.Initial(
          BlockHeader.Initial(
            startTime = BlockCreationStartTime(blockCreationEndTime - 10.seconds),
            endTime = blockCreationEndTime,
            fallbackTxStartTime = initTxSeq.fallbackTx.fallbackTxStartTime,
            kzgCommitment = initializationParameters.initialEvacuationMap.kzgCommitment
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

object InitialBlockTest extends Properties("Initial block") {
    override def overrideParameters(p: Parameters): Parameters = {
        p.withMinSuccessfulTests(100)
    }

    val _ = property("generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(TestPeers.generate)
          .flatMap(generateInitialBlock(_)())
    )(_ => true)
}
