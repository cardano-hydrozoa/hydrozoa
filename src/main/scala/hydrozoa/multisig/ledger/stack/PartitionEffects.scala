package hydrozoa.multisig.ledger.stack

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, FinalizationTx, RefundTx, RolloutTx, SettlementTx}
import scalus.cardano.ledger.Transaction

/** Per-partition L1 effects — the partition is the spine of a regular stack's effects (PR #446
  * review). Exactly one [[PartitionEffects]] per [[StackPartition]], same order; the kind mirrors
  * [[StackPartition.Kind]] (regular stacks: `Minor` / `Major` / `Final` only).
  *
  * Treasury rotation across stacks happens only via a `Major`'s settlement or the `Final`'s
  * finalization, in partition order — never via a `Minor` (an SEC is not a treasury mutation).
  *
  * Generic in the standalone-evac-commitment representation `S`: the bare
  * [[StandaloneEvacuationCommitment]] while unsigned,
  * [[StandaloneEvacuationCommitment.MultiSigned]] once hard-confirmed. The tx wrappers are
  * unchanged across signing phases — the multisig `VKeyWitness`es live inside each
  * [[hydrozoa.multisig.ledger.l1.tx.EnrichedTx]] body — so only `S` differs between
  * `StackEffects.Unsigned.Regular` and `StackEffects.HardConfirmed.Regular`.
  */
sealed trait PartitionEffects[+S]

object PartitionEffects:

    /** [[StackPartition.Kind.Major]] partition: the Major's settlement sequence (settlement +
      * fallback + rollouts) + its trailing minors' post-dated refunds, plus an SEC for the
      * partition's **last minor** iff the partition has >= 1 trailing minor.
      *
      * The SEC commits the partition's last minor, which has the SAME major version as the Major.
      * It complements rather than duplicates the settlement: the settlement snapshots state AT the
      * Major; the trailing minors advance L2 state past that snapshot at the same major version.
      * Carrying the SEC in THIS partition — not deferring it to be subsumed by the next major's
      * settlement — keeps the latest-minor commitment available to the rule-based regime even if
      * the NEXT partition's settlement never executes onchain.
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
      * round-2 and by the verifier to know round-2's target — one shared function for both.
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

    /** The L1 transaction the round-2 unlock signs — the [[Unlock]]'s partition's settlement
      * (Major) or finalization (Final). Pairs with [[unlock]].
      */
    def unlockTxOf[S](parts: NonEmptyList[PartitionEffects[S]], u: Unlock): Transaction =
        u match {
            case Unlock.Settlement(i) =>
                parts.toList(i) match {
                    case Major(settlement, _, _, _, _) => settlement.tx
                    case _ =>
                        throw new IllegalStateException(
                          s"unlock Settlement($i) not a Major partition"
                        )
                }
            case Unlock.Finalization(i) =>
                parts.toList(i) match {
                    case Final(finalization, _) => finalization.tx
                    case _ =>
                        throw new IllegalStateException(
                          s"unlock Finalization($i) not a Final partition"
                        )
                }
        }
