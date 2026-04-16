package hydrozoa.config.head.initialization

import cats.data.{Kleisli, ReaderT}
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.initialization.BlockCreationEndTimeGen.currentTimeBlockCreationEndTime
import hydrozoa.config.head.multisig.fallback.generateFallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.parameters.{HeadParameters, generateHeadParameters}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import monocle.Focus.focus
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Prop, Properties}

import scala.concurrent.duration.DurationInt
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.ledger.Hash32
import test.{GenWithTestPeers, TestPeers, TestPeersSpec, given}

def generateInitialBlock(
    generateHeadParameters: GenWithTestPeers[HeadParameters] = generateHeadParameters(),
    generateBlockCreationEndTime: GenWithTestPeers[BlockCreationEndTime] =
        currentTimeBlockCreationEndTime,
    generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
        InitializationParametersGenTopDown.GenWithDeps | InitializationParameters =
        InitializationParametersGenBottomUp.generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters,
): GenWithTestPeers[InitialBlock] = {
    for {
        testPeers <- ReaderT.ask
        cardanoNetwork = testPeers.cardanoNetwork

        headParams <- generateHeadParameters

        initializationParameters <- generateInitializationParameters match {
            case g: InitializationParametersGenBottomUp.GenInitializationParameters =>
                g(Kleisli.liftF(Gen.const(headParams.fallbackContingency)))
            case InitializationParametersGenTopDown.GenWithDeps(
                  generator,
                  generateGenesisUtxosL1,
                  equityRange
                )) =>
                generator(
                  generateFallbackContingency,
                  generateGenesisUtxosL1,
                  equityRange
                )
            case ps: InitializationParameters => ReaderT.liftF(Gen.const(ps))
        }

        config = HeadConfig
            .Preinit(
              cardanoNetwork = cardanoNetwork,
              headParams = headParams,
              headPeers = testPeers.mkHeadPeers,
              initializationParams = initializationParameters
            ) match {
            case Validated.Valid(x) => x
            case Validated.Invalid(errors) =>
                throw RuntimeException(s"Generating HeadConfig.Preinit failed: $errors")
        }

        blockCreationEndTime <- generateBlockCreationEndTime

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
          .flatMap(generateInitialBlock().run(_))
    )(_ => true)
}
