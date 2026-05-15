package hydrozoa.multisig.ledger.effects

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.block.{BlockResult, BlockVersion}

/** Per spec (`consensus/slow-consensus`): partition the stack's blocks by major version, and within
  * each partition keep all non-standalone-evac-commitment effects + only the LAST standalone evac
  * commitment.
  *
  * A **partition** is a maximal contiguous run of blocks in the stack that share the same major
  * version. The first block of a non-leading partition is by definition a [[Block.Major]] (since
  * crossing a partition boundary requires a major-version increment).
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
final case class Partition(
    blocks: NonEmptyList[BlockResult],
    majorVersion: BlockVersion.Major,
    closing: Partition.Closing
)

object Partition {
    enum Closing:
        case Major, Final, TrailingMinors
}

object NecessaryEffectsPolicy {

    /** Partition the stack's blocks by major version. The major version of the block transitions at
      * Major / Final blocks; consecutive blocks sharing a major version belong to the same
      * partition.
      *
      * TODO(slow-consensus): implement. Algorithm sketch:
      *   1. Walk `blocks` in order, tracking `currentMajor: BlockVersion.Major`.
      *   2. Open a partition with the first block; close it whenever a Major or Final is
      *      encountered (close-after-major), then open a new one starting with the next block.
      *   3. The closing-block type of each partition determines `Partition.Closing`.
      *   4. The last partition may be `TrailingMinors` if the stack ends with minor blocks after a
      *      major.
      */
    def selectNecessaryEffects(blocks: NonEmptyList[BlockResult]): List[Partition] =
        ??? // PR2: implement partition-by-major + last-evac-commit compression
}
