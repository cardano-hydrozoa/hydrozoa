package hydrozoa.multisig.ledger.block

import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout

/** Per-block local data emitted by [[hydrozoa.multisig.ledger.joint.JointLedger]] on local block
  * completion. Independent of soft-confirmation — fires immediately after the block is built
  * locally, so the slow side can begin assembling stacks without waiting on the fast cycle.
  *
  * Carries everything the slow side needs to derive every L1 effect for a stack containing this
  * block — no JointLedger state lookup at slow-side derivation time:
  *
  *   - `evacuationMapDiff` — per-block delta to the evacuation map. Drives the block's standalone
  *     evac commitment (KZG commitment) and feeds into the next major's settlement tx.
  *   - `payoutObligations` — refund + rollout obligations to be realized on L1. Drive the block's
  *     refund txs (for minors absorbing deposits) and rollout txs (for L2 requests producing
  *     immediate L1 payouts).
  */
final case class BlockResult(
    brief: BlockBrief.Next,
    evacuationMapDiff: Seq[EvacuationDiff],
    payoutObligations: List[Payout.Obligation]
)
