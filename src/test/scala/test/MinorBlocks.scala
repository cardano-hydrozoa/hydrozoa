package test

import cats.effect.IO
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import scala.concurrent.duration.DurationInt

/** An empty minor block brief, timed off a head config's slot config.
  *
  * Shared because several suites need a block brief only as a *position* — something that occupies
  * a block number so the code under test can read it back — and not for anything in its body.
  */
object MinorBlocks {

    def brief(config: HeadConfig, blockNum: Int): IO[BlockBrief.Next] =
        realTimeQuantizedInstant(config.slotConfig).map { now =>
            val end = BlockCreationEndTime(now + 1.second)
            val fallback = config.txTiming.newFallbackStartTime(end)
            BlockBrief.Minor(
              BlockHeader.Minor(
                blockNum = BlockNumber(blockNum),
                blockVersion = BlockVersion.Full(0, 0),
                startTime = BlockCreationStartTime(now),
                endTime = end,
                fallbackTxStartTime = fallback,
                forcedMajorBlockWakeupTime = config.txTiming.forcedMajorBlockWakeupTime(fallback),
                mDepositDecisionWakeupTime = None
              ),
              BlockBody.Minor(requests = List.empty, depositsRejected = List.empty)
            )
        }
}
