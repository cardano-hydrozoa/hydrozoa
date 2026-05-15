package hydrozoa.multisig.ledger.effects

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.block.{BlockResult, BlockType, BlockVersion}

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

    /** Partition the stack's blocks into a list of [[Partition]]s. Each partition is a maximal
      * contiguous run of blocks ending at a Major / Final block (close-after-major). A trailing run
      * of minors with no closing major / final yields one final
      * [[Partition.Closing.TrailingMinors]] partition.
      *
      * Per spec ("close-after-major"): a Major or Final block belongs to the partition it closes
      * (it is the LAST block of that partition), then the next partition begins at the subsequent
      * block. The major version of each partition is the version of its last block.
      */
    def selectNecessaryEffects(blocks: NonEmptyList[BlockResult]): List[Partition] = {
        val out = List.newBuilder[Partition]
        var current = List.newBuilder[BlockResult]

        for b <- blocks.toList do {
            current += b
            b.brief match {
                case _: BlockType.Major =>
                    val partBlocks = NonEmptyList.fromListUnsafe(current.result())
                    out += Partition(
                      partBlocks,
                      b.brief.blockVersion.major,
                      Partition.Closing.Major
                    )
                    current = List.newBuilder[BlockResult]
                case _: BlockType.Final =>
                    val partBlocks = NonEmptyList.fromListUnsafe(current.result())
                    out += Partition(
                      partBlocks,
                      b.brief.blockVersion.major,
                      Partition.Closing.Final
                    )
                    current = List.newBuilder[BlockResult]
                case _: BlockType.Minor =>
                    () // keep accumulating into the current partition
            }
        }

        val tail = current.result()
        if tail.nonEmpty then {
            // Trailing minors: no closing major / final in this stack (or after the last one).
            // Major version is the version of the last minor — minors don't change major
            // version, so this equals whatever previous Major produced (or 0 for stack 1 with
            // only minors after init).
            val partBlocks = NonEmptyList.fromListUnsafe(tail)
            out += Partition(
              partBlocks,
              partBlocks.last.brief.blockVersion.major,
              Partition.Closing.TrailingMinors
            )
        }

        out.result()
    }
}
