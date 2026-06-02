package hydrozoa.multisig.ledger.stack

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.block.{BlockResult, BlockType, BlockVersion}

/** A **partition** of a regular slow-consensus stack — a contiguous block run classified by the
  * block that **opens** it (head-based; PR #446 review model). The opener owns the
  * same-major-version minors that follow it. See [[StackPartition.partition]] for the
  * classification rules and [[StackEffectsBuilder]] for the per-kind L1 effects.
  *
  * @param blocks
  *   the partition's blocks in stack order (opener first)
  * @param majorVersion
  *   the opener's major version (for a [[StackPartition.Kind.Minor]] partition: the minors' carried
  *   version — minors don't bump it)
  * @param kind
  *   how the partition is classified — by its opening block
  */
final case class StackPartition(
    blocks: NonEmptyList[BlockResult],
    majorVersion: BlockVersion.Major,
    kind: StackPartition.Kind
)

object StackPartition {

    /** How a partition is classified — by its **opening** block. `Initial` is the stack-0 bootstrap
      * case (handled by the separate `StackEffects.Initial` path, never produced by [[partition]]);
      * regular stacks yield only `Minor` / `Major` / `Final`.
      */
    enum Kind:
        case Initial, Major, Final, Minor

    /** Partition a regular stack's blocks into **head-based** partitions (PR #446 review model).
      * Classify by the block that OPENS a partition; the opener owns the same-major-version minors
      * that FOLLOW it:
      *
      *   - **Minor** — a leading run of minors. Only ever the FIRST partition: every other minor
      *     run is consumed as a Major's tail (below). Runs to the next Major / Final or stack end.
      *     Its `majorVersion` is the minors' carried version (the preceding stack's major, or 0 for
      *     a stack-1 minor-only run).
      *   - **Major** — a Major block plus the minors trailing it (same major version), up to the
      *     next Major / Final or stack end.
      *   - **Final** — the Final block alone. A Final terminates the head ⇒ it is necessarily the
      *     LAST block of the stack; at most one Final partition, last. `require` enforces it.
      *
      * `BlockResult.brief` is a [[hydrozoa.multisig.ledger.block.BlockBrief.Next]] (never Initial —
      * stack 0 runs the separate Initial path), so only Minor/Major/Final arise. A regular stack
      * always has >= 1 block ⇒ >= 1 partition, hence `NonEmptyList`.
      */
    def partition(blocks: NonEmptyList[BlockResult]): NonEmptyList[StackPartition] = {
        def isMinor(b: BlockResult): Boolean = b.brief match {
            case _: BlockType.Minor => true
            case _                  => false
        }

        // Tail-recursive: each step consumes one partition's worth of blocks from the front.
        // Minor head → the leading minor run (acc necessarily empty). Major head → itself +
        // trailing minors. Final head → itself, terminal (nothing may follow it).
        @annotation.tailrec
        def loop(
            remaining: List[BlockResult],
            acc: List[StackPartition]
        ): List[StackPartition] =
            remaining match {
                case Nil => acc.reverse
                case head :: rest =>
                    head.brief match {
                        case _: BlockType.Minor =>
                            val (minors, rest2) = remaining.span(isMinor)
                            val part = NonEmptyList.fromListUnsafe(minors)
                            loop(
                              rest2,
                              StackPartition(
                                part,
                                part.last.brief.blockVersion.major,
                                Kind.Minor
                              ) :: acc
                            )
                        case _: BlockType.Major =>
                            val (trailingMinors, rest2) = rest.span(isMinor)
                            loop(
                              rest2,
                              StackPartition(
                                NonEmptyList(head, trailingMinors),
                                head.brief.blockVersion.major,
                                Kind.Major
                              ) :: acc
                            )
                        case _ =>
                            // BlockBrief.Next ⇒ Minor | Major | Final, so this is Final.
                            require(
                              rest.isEmpty,
                              s"Final block ${head.brief.blockNum} is not the last block of " +
                                  s"the stack (${rest.size} block(s) follow)"
                            )
                            (StackPartition(
                              NonEmptyList.one(head),
                              head.brief.blockVersion.major,
                              Kind.Final
                            ) :: acc).reverse
                    }
            }

        NonEmptyList.fromListUnsafe(loop(blocks.toList, Nil))
    }
}
