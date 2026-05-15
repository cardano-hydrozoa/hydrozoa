package hydrozoa.multisig.ledger.block

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.FallbackTxStartTime
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo

/** Per-block local data emitted by [[hydrozoa.multisig.ledger.joint.JointLedger]] on local block
  * completion. Independent of soft-confirmation — fires immediately after the block is built
  * locally, so the slow side can begin assembling stacks without waiting on the fast cycle.
  *
  * Carries everything the slow side needs to derive every L1 effect for a stack containing this
  * block — no JointLedger state lookup at slow-side derivation time:
  *
  *   - `evacuationMapDiff` — per-block delta to the evacuation map. Drives the block's standalone
  *     evac commitment (KZG commitment) and feeds into the next major's settlement tx.
  *   - `payoutObligations` — L2 payout obligations visible at this block (snapshot of the L2
  *     ledger's `payouts` after this block's L2 mutations applied). The next Major / Final block in
  *     the stack drains these into the settlement / finalization tx.
  *   - `postDatedRefundTxs` — pre-built refund txs from deposit registration (signed at deposit
  *     time per spec); attached to the block that absorbs the corresponding deposits and surfaced
  *     on L1 by the slow side.
  *   - `absorbedDeposits` — deposit utxos this block decided to absorb. Used by the next Major
  *     block's settlement tx construction (`mkSettlementTxSeq.absorbedDeposits`).
  *   - `competingFallbackTxTime` — fallback-tx validity start that the block's settlement /
  *     finalization tx must compete with. Carried from JointLedger.Producing's
  *     `competingFallbackTxTime`.
  */
final case class BlockResult(
    brief: BlockBrief.Next,
    evacuationMapDiff: Seq[EvacuationDiff],
    payoutObligations: List[Payout.Obligation],
    postDatedRefundTxs: List[RefundTx.PostDated],
    absorbedDeposits: List[DepositUtxo],
    competingFallbackTxTime: FallbackTxStartTime
)
