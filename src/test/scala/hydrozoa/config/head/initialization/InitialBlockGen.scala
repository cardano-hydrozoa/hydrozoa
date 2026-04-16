package hydrozoa.config.head.initialization

import cats.data.ReaderT
import hydrozoa.config.head.initialization.BlockCreationEndTimeGen.currentTimeBlockCreationEndTime
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.{HeadConfig, generateHeadConfigPreinit}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import monocle.Focus.focus
import org.scalacheck.Test.Parameters
import org.scalacheck.{Prop, Properties}
import scala.concurrent.duration.DurationInt
import test.{GenWithTestPeers, TestPeers, TestPeersSpec, given}

def generateInitialBlock(
    genHeadConfigPreinit: GenWithTestPeers[HeadConfig.Preinit] = generateHeadConfigPreinit(),
    generateBlockCreationEndTime: GenWithTestPeers[BlockCreationEndTime] =
        currentTimeBlockCreationEndTime,
): GenWithTestPeers[InitialBlock] = {
    for {
        testPeers <- ReaderT.ask
        config <- genHeadConfigPreinit

        blockCreationEndTime <- generateBlockCreationEndTime

        initTxSeq =
            InitializationTxSeq.Build(config)(blockCreationEndTime).result match {
                case Left(e) =>
                    throw new RuntimeException(e.toString, e)
                case Right(x) => x
            }

        fallbackTxStartTime = initTxSeq.fallbackTx.fallbackTxStartTime
        forcedMajorBlockTime = config.headParams.txTiming.forcedMajorBlockTime(fallbackTxStartTime)
        majorBlockWakeupTime = TxTiming.majorBlockWakeupTime(forcedMajorBlockTime, None)

    } yield InitialBlock(
      Block.MultiSigned.Initial(
        blockBrief = BlockBrief.Initial(
          BlockHeader.Initial(
            startTime = BlockCreationStartTime(blockCreationEndTime - 10.seconds),
            endTime = blockCreationEndTime,
            fallbackTxStartTime = initTxSeq.fallbackTx.fallbackTxStartTime,
            majorBlockWakeupTime = majorBlockWakeupTime,
            kzgCommitment = config.initializationParams.initialEvacuationMap.kzgCommitment
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
