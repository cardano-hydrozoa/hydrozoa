package hydrozoa.config.head.initialization

import cats.data.ReaderT
import hydrozoa.bootstrap.InitializationFunding
import hydrozoa.config.head.initialization.BlockCreationEndTimeGen.currentTimeBlockCreationEndTime
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.{HeadConfig, HeadParamsHash, generateHeadConfigBootstrap}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import org.scalacheck.Test.Parameters
import org.scalacheck.{Prop, Properties}
import scala.concurrent.duration.DurationInt
import test.{GenWithTestPeers, TestPeers, TestPeersSpec, given}

def generateInitialBlock(
    genHeadConfigBootstrap: GenWithTestPeers[(HeadConfig.Bootstrap, InitializationFunding)] =
        generateHeadConfigBootstrap(),
    generateBlockCreationEndTime: GenWithTestPeers[BlockCreationEndTime] =
        currentTimeBlockCreationEndTime,
): GenWithTestPeers[InitialBlock] = {
    for {
        _ <- ReaderT.ask
        configAndFunding <- genHeadConfigBootstrap
        (config, funding) = configAndFunding

        blockCreationEndTime <- generateBlockCreationEndTime

        // The header is built before the transactions, as `Bootstrap.mkSharedHeadConfig` does:
        // `headParamsHash` covers it and the init tx's treasury datum carries that digest.
        fallbackTxStartTime = config.headParameters.txTiming.newFallbackStartTime(
          blockCreationEndTime
        )
        forcedMajorBlockWakeupTime = config.headParameters.txTiming.forcedMajorBlockWakeupTime(
          fallbackTxStartTime
        )
        header = BlockHeader.Initial(
          startTime = BlockCreationStartTime(blockCreationEndTime - 10.seconds),
          endTime = blockCreationEndTime,
          fallbackTxStartTime = fallbackTxStartTime,
          forcedMajorBlockWakeupTime = forcedMajorBlockWakeupTime,
          mDepositDecisionWakeupTime = None,
        )

        initTxSeqResult = InitializationTxSeq
            .Build(config, funding)(blockCreationEndTime, HeadParamsHash(config, header))
            .result

        initTxSeq = initTxSeqResult match {
            case Left(e)  => throw new RuntimeException(e.toString, e)
            case Right(x) => x
        }

    } yield InitialBlock(
      Block.Unsigned.Initial(
        blockBrief = BlockBrief.Initial(header),
        // Unsigned — slow consensus stack-0 signs them at startup.
        effects = BlockEffects.Unsigned.Initial(
          initializationTx = initTxSeq.initializationTx,
          fallbackTx = initTxSeq.fallbackTx
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
