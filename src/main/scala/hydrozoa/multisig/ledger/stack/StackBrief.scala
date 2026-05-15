package hydrozoa.multisig.ledger.stack

import hydrozoa.multisig.ledger.block.BlockNumber
import io.circe.*
import io.circe.generic.semiauto.*

/** Slow leader's stack-composition announcement.
  *
  * Wire payload: per spec, only the brief (not the derived effects) is broadcast. Other peers
  * re-derive effects locally from their own [[BlockResult]] streams.
  *
  * @param firstMajorBlockNum
  *   present iff the stack contains at least one major block; demarcates the first major
  *   partition's starting block.
  */
final case class StackBrief(
    stackNum: StackNumber,
    firstBlockNum: BlockNumber,
    lastBlockNum: BlockNumber,
    firstMajorBlockNum: Option[BlockNumber]
)

object StackBrief {
    given Codec[StackBrief] = deriveCodec[StackBrief]
}
