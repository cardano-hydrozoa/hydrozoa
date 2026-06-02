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
  *   - `brief` — the block's [[BlockBrief.Next]] (Minor | Major | Final).
  *     `StackPartition.partition` reads its kind to group blocks into Minor / Major / Final
  *     partitions; its `blockNum` keys block pairing and the stack's `[first, last]` range; its
  *     `header` (block version, end time) feeds each SEC and the Major settlement's validity
  *     window.
  *   - `evacuationMapDiff` — per-block delta to the evacuation map. The builder folds these into a
  *     cumulative map threaded across the stack; that map's KZG commitment supplies each Major
  *     settlement's `nextKzg` and each last-of-partition minor's SEC.
  *   - `payoutObligations` — L2 **withdrawal** obligations visible at this block (snapshot of the
  *     L2 ledger's `payouts` after this block's L2 mutations applied) — funds an L2 request moves
  *     out to L1. A Major block's snapshot drains into its own settlement tx
  *     (`mkSettlementTxSeq.payoutObligations`). Final blocks ignore this field: JointLedger emits
  *     no cumulative snapshot for Final, so the builder recovers the remaining payouts from its
  *     running evacuation map.
  *   - `postDatedRefundTxs` — pre-built refund txs from deposit registration (signed at deposit
  *     time per spec); the L1 way to return a deposit that was not absorbed. The builder collects
  *     them per partition (`partitionRefunds`) into each Minor / Major `PartitionEffects.refunds`.
  *   - `absorbedDeposits` — matured deposit utxos this (Major) block decided to absorb. Spent by
  *     this block's own settlement tx (`mkSettlementTxSeq.absorbedDeposits`).
  *   - `competingFallbackTxTime` — fallback-tx validity start that the block's settlement /
  *     finalization tx must compete with. Passed to `mkSettlementTxSeq` / `finalizeLedger` as
  *     `competingFallbackValidityStart` to determine settlement tx TTL. Carried from
  *     JointLedger.Producing's `competingFallbackTxTime`.
  */
final case class BlockResult(
    brief: BlockBrief.Next,
    evacuationMapDiff: Seq[EvacuationDiff],
    payoutObligations: List[Payout.Obligation],
    postDatedRefundTxs: List[RefundTx.PostDated],
    absorbedDeposits: List[DepositUtxo],
    competingFallbackTxTime: FallbackTxStartTime
)
