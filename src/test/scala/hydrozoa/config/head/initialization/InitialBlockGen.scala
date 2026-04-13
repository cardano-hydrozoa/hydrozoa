package hydrozoa.config.head.initialization

import hydrozoa.config.head.InitParamsType.{BottomUp, Constant, TopDown}
import hydrozoa.config.head.initialization.BlockCreationEndTimeGen.{BlockCreationEndTimeGen, currentTimeBlockCreationEndTime}
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.settlement.{SettlementConfigGen, generateSettlementConfig}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationStartTime
import hydrozoa.config.head.multisig.timing.{TxTiming, TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.{GenHeadParams, generateHeadParameters}
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import hydrozoa.config.head.{HeadConfig, InitParamsType}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import monocle.Focus.focus
import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.ledger.Hash32
import test.{TestPeers, TestPeersSpec}

def generateInitialBlock(testPeers: TestPeers)(
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateHeadParameters: GenHeadParams = generateHeadParameters,
    generateBlockCreationEndTime: BlockCreationEndTimeGen = currentTimeBlockCreationEndTime,
    generateInitializationParameters: InitParamsType = BottomUp(
      InitializationParametersGenBottomUp.generateInitializationParameters
    ),
    generateSettlementConfig: SettlementConfigGen = generateSettlementConfig,
    generateL2ParamsHash: Gen[Hash32] = Arbitrary.arbitrary[Hash32]
): Gen[InitialBlock] = {
    for {
        cardanoNetwork <- Gen.const(testPeers.network)

        headParams <- generateHeadParameters(cardanoNetwork)(
          generateTxTiming,
          generateFallbackContingency,
          generateDisputeResolutionConfig,
          generateSettlementConfig,
          generateL2ParamsHash
        )

        initializationParameters <- generateInitializationParameters match {
            case BottomUp(g) =>
                g(testPeers)(_ => Gen.const(headParams.fallbackContingency))
            case TopDown(
                  InitializationParametersGenTopDown.GenWithDeps(
                    generator,
                    generateGenesisUtxosL1,
                    equityRange
                  )
                ) =>
                generator(testPeers)(
                  generateFallbackContingency,
                  generateGenesisUtxosL1,
                  equityRange
                )
            case Constant(ps) => Gen.const(ps)
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

        fallbackTxStartTime = initTxSeq.fallbackTx.fallbackTxStartTime
        forcedMajorBlockTime = headParams.txTiming.forcedMajorBlockTime(fallbackTxStartTime)
        majorBlockWakeupTime = TxTiming.majorBlockWakeupTime(forcedMajorBlockTime, None)

    } yield InitialBlock(
      Block.MultiSigned.Initial(
        blockBrief = BlockBrief.Initial(
          BlockHeader.Initial(
            startTime = BlockCreationStartTime(blockCreationEndTime - 10.seconds),
            endTime = blockCreationEndTime,
            fallbackTxStartTime = initTxSeq.fallbackTx.fallbackTxStartTime,
            majorBlockWakeupTime = majorBlockWakeupTime,
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
