package hydrozoa.multisig.ledger.stack

import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.block.BlockNumber
import io.circe.*
import io.circe.generic.semiauto.*

/** Slow leader's stack-composition announcement: the block range `[firstBlockNum, lastBlockNum]`
  * the leader chose for this stack, plus the wall-clock time at which the leader finished composing
  * the brief (`creationEndTime`).
  *
  * Wire payload: per spec, only the brief (not the derived effects) is broadcast. Other peers
  * re-derive effects locally from their own [[BlockResult]] streams.
  *
  * `creationEndTime` is used by the [[hydrozoa.multisig.consensus.limiter.Limiter]] sitting on the
  * SlowConsensusActor → StackComposer lane to bound the rate at which the next stack can be closed:
  * hard-confirmation for stack `N` is held until `N.creationEndTime + hardStackMinPeriod` has
  * elapsed wall-clock.
  *
  * No `firstMajorBlockNum`: the partition-by-major structure is fully deterministic from the block
  * *types* every peer already has (head peers via their own BlockResult stream; coil peers, when
  * added, will also have all block data). A first-major hint would only help a party that lacks
  * block data — which no party does — so it's redundant.
  */
final case class StackBrief(
    stackNum: StackNumber,
    firstBlockNum: BlockNumber,
    lastBlockNum: BlockNumber,
    creationEndTime: StackCreationEndTime
)

object StackBrief {
    given (using CardanoNetwork.Section): Codec[StackBrief] = deriveCodec[StackBrief]
}
