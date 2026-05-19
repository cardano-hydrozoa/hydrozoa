package hydrozoa.multisig.ledger.stack

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, FinalizationTx, RefundTx, RolloutTx, SettlementTx}

/** Per-partition L1 effects — the partition is the spine of a regular stack's effects (PR #446
  * review). Exactly one [[PartitionEffects]] per [[StackPartition]], same order; the kind mirrors
  * [[StackPartition.Kind]] (regular stacks: `Minor` / `Major` / `Final` only).
  *
  * Generic in the standalone-evac-commitment representation `S`: the bare
  * [[StandaloneEvacuationCommitment]] while unsigned,
  * [[StandaloneEvacuationCommitment.MultiSigned]] once hard-confirmed. The tx wrappers are
  * unchanged across signing phases — the multisig `VKeyWitness`es live inside each
  * [[hydrozoa.multisig.ledger.l1.tx.Tx]] body — so only `S` differs between
  * `StackEffects.Unsigned.Regular` and `StackEffects.HardConfirmed.Regular`.
  */
sealed trait PartitionEffects[+S]

object PartitionEffects:

    /** [[StackPartition.Kind.Major]] partition: the Major's settlement sequence (settlement +
      * fallback + rollouts) + its trailing minors' post-dated refunds, plus an SEC for the
      * partition's **last minor** iff the partition has >= 1 trailing minor.
      *
      * The SEC is NOT redundant with the settlement: the settlement's L1 execution is not
      * guaranteed, and the post-settlement trailing minors advance KZG beyond the settlement
      * snapshot — voting in the rule-based regime needs the latest minor's commitment.
      */
    final case class Major[+S](
        settlement: SettlementTx,
        fallback: FallbackTx,
        rollouts: List[RolloutTx],
        refunds: List[RefundTx],
        sec: Option[S]
    ) extends PartitionEffects[S]

    /** [[StackPartition.Kind.Final]] partition: the Final block alone — finalization + its rollouts
      * (deinit subsumed). No SEC (no minors), no fallback.
      */
    final case class Final(
        finalization: FinalizationTx,
        rollouts: List[RolloutTx]
    ) extends PartitionEffects[Nothing]

    /** [[StackPartition.Kind.Minor]] partition: a leading minor run — a **mandatory** SEC for the
      * latest minor + those minors' post-dated refunds. No settlement (no Major here).
      */
    final case class Minor[+S](
        sec: S,
        refunds: List[RefundTx]
    ) extends PartitionEffects[S]

    /** The round-2 *unlock* selector — the single shared rule (PR #446 review): the **first**
      * `Major` partition's settlement; else (no `Major`) the `Final` partition's finalization; else
      * `None` (all-`Minor` ⇒ 1-phase / sole, no unlock). Called by the signer to pack round-1 vs
      * round-2 and by the verifier to know round-2's target — one function, not a re-derived plan.
      * `partitionIndex` is the position in the partition list.
      */
    enum Unlock:
        case Settlement(partitionIndex: Int)
        case Finalization(partitionIndex: Int)

    def unlock(partitions: NonEmptyList[PartitionEffects[?]]): Option[Unlock] = {
        val indexed = partitions.toList.zipWithIndex
        indexed
            .collectFirst { case (_: Major[?], i) => Unlock.Settlement(i) }
            .orElse(indexed.collectFirst { case (_: Final, i) => Unlock.Finalization(i) })
    }
