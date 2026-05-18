package hydrozoa.multisig.ledger.stack

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.block.{BlockResult, BlockType, BlockVersion}

/** A **partition** of a slow-consensus stack: a maximal contiguous run of blocks that share the
  * same major version. Per spec (`consensus/slow-consensus`) the stack is partitioned by major
  * version, and within each partition all non-standalone-evac-commitment effects are kept plus only
  * the LAST standalone evac commitment.
  *
  * The first block of a non-leading partition is by definition a [[Block.Major]] (since crossing a
  * partition boundary requires a major-version increment).
  *
  * The closing block of a partition determines what L1 effects are produced for it:
  *
  *   - **Major-closed**: the closing major's settlement tx absorbs the entire partition's
  *     accumulated evac map state — no standalone evac commitment is needed.
  *   - **Final-closed**: the finalization tx subsumes everything; no settlement, no standalone.
  *   - **Trailing-minors** (only possible for the LAST partition in the stack, when no major
  *     follows it): keep only the LAST evac commitment of the partition. Earlier ones are
  *     superseded by the later one (each commits to cumulative state).
  */
final case class StackPartition(
    blocks: NonEmptyList[BlockResult],
    majorVersion: BlockVersion.Major,
    closing: StackPartition.Closing
)

object StackPartition {
    enum Closing:
        case Major, Final, TrailingMinors

    /** Partition the stack's blocks. Each partition is a maximal contiguous run of blocks ending at
      * a Major / Final block (close-after-major: the boundary block is the LAST block of the
      * partition it closes); a trailing run of minors with no closing major / final is one
      * [[StackPartition.Closing.TrailingMinors]] partition.
      *
      * Structural invariants this encodes (and enforces):
      *   - A **Final** block terminates the head, so it is necessarily the LAST block of the stack
      *     ⇒ there is at most ONE Final-closed partition and it is the last partition. A `require`
      *     rejects a stack with blocks after a Final.
      *   - A **TrailingMinors** partition exists only when no Major / Final follows the trailing
      *     minors ⇒ there is at most ONE, and it is the last partition (the recursion terminates as
      *     soon as it is produced — nothing can follow it).
      *   - Every other partition is Major-closed.
      *
      * The major version of a partition is the version of its (last) boundary block; for a
      * TrailingMinors partition it is the version of the last minor (minors don't bump the major
      * version, so this is whatever the preceding Major produced, or 0 for a stack-1 minor-only
      * run).
      */
    def partition(blocks: NonEmptyList[BlockResult]): List[StackPartition] = {
        def isMinor(b: BlockResult): Boolean = b.brief match {
            case _: BlockType.Minor => true
            case _                  => false
        }

        // `acc` holds completed partitions in reverse order. Tail-recursive: the only
        // self-call (Major case) is in tail position; the TrailingMinors / Final cases are
        // terminal because nothing can follow either of them in a well-formed stack.
        @annotation.tailrec
        def loop(
            remaining: List[BlockResult],
            acc: List[StackPartition]
        ): List[StackPartition] =
            remaining match {
                case Nil =>
                    // Reached only when the stack's last block was a Major (no trailing
                    // minors, no Final) — that Major's partition is already in `acc`.
                    acc.reverse
                case all =>
                    val (leadingMinors, rest) = all.span(isMinor)
                    rest match {
                        case Nil =>
                            // No Major / Final boundary in what's left ⇒ pure trailing
                            // minors. Necessarily the last partition: there is nothing after
                            // it, so we finish here.
                            val part = NonEmptyList.fromListUnsafe(leadingMinors)
                            (StackPartition(
                              part,
                              part.last.brief.blockVersion.major,
                              StackPartition.Closing.TrailingMinors
                            ) :: acc).reverse
                        case boundary :: tail =>
                            val partBlocks =
                                NonEmptyList.fromListUnsafe(leadingMinors :+ boundary)
                            boundary.brief match {
                                case _: BlockType.Major =>
                                    loop(
                                      tail,
                                      StackPartition(
                                        partBlocks,
                                        boundary.brief.blockVersion.major,
                                        StackPartition.Closing.Major
                                      ) :: acc
                                    )
                                case _: BlockType.Final =>
                                    require(
                                      tail.isEmpty,
                                      s"Final block ${boundary.brief.blockNum} is not the " +
                                          s"last block of the stack (${tail.size} block(s) " +
                                          "follow)"
                                    )
                                    (StackPartition(
                                      partBlocks,
                                      boundary.brief.blockVersion.major,
                                      StackPartition.Closing.Final
                                    ) :: acc).reverse
                                case _: BlockType.Minor =>
                                    // Unreachable: `span(isMinor)` guarantees `boundary` is
                                    // the first non-minor block.
                                    throw new IllegalStateException(
                                      "span(isMinor) yielded a minor boundary"
                                    )
                            }
                    }
            }

        loop(blocks.toList, Nil)
    }
}
