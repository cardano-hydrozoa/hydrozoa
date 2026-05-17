package hydrozoa.multisig.ledger.stack

import hydrozoa.multisig.ledger.block.BlockNumber
import io.circe.*
import io.circe.generic.semiauto.*

/** Slow leader's stack-composition announcement: just the block range `[firstBlockNum,
  * lastBlockNum]` the leader chose for this stack.
  *
  * Wire payload: per spec, only the brief (not the derived effects) is broadcast. Other peers
  * re-derive effects locally from their own [[BlockResult]] streams.
  *
  * No `firstMajorBlockNum`: the partition-by-major structure is fully deterministic from the block
  * *types* every peer already has (head peers via their own BlockResult stream; coil peers, when
  * added, will also have all block data). A first-major hint would only help a party that lacks
  * block data — which no party does — so it's redundant.
  */
final case class StackBrief(
    stackNum: StackNumber,
    firstBlockNum: BlockNumber,
    lastBlockNum: BlockNumber
)

object StackBrief {
    given Codec[StackBrief] = deriveCodec[StackBrief]
}
