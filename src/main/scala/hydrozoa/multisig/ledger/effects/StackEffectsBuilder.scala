package hydrozoa.multisig.ledger.effects

import hydrozoa.multisig.ledger.l1.L1LedgerM
import hydrozoa.multisig.ledger.stack.StackEffects

/** Slow-side effect derivation: from the partitioned stack content, produce the necessary L1 effect
  * transactions. Wraps the existing [[L1LedgerM]] tx-construction helpers (`mkSettlementTxSeq`,
  * `finalizeLedger`) and adds slow-side concerns:
  *
  *   - Treasury rotation across stacks (re-enabled in M10 once slow consensus drives it).
  *   - Standalone evacuation-commitment construction for trailing-minor partitions.
  *
  * Pure functions of `(L1LedgerM state at stack open) + List[Partition]` — no JointLedger lookup,
  * all required state threaded through inputs.
  */
object StackEffectsBuilder {

    /** Build the regular-stack effect bundle for a non-initial stack.
      *
      * TODO(slow-consensus): implement. Per partition:
      *   - `Closing.Major` → `L1LedgerM.mkSettlementTxSeq(...)` to build settlement + fallback +
      *     rollouts; refunds from absorbed-deposit obligations; no standalone evac commit.
      *   - `Closing.Final` → `L1LedgerM.finalizeLedger(...)` for finalization tx + rollouts; no
      *     settlement; no standalone evac commit.
      *   - `Closing.TrailingMinors` → build a [[StandaloneEvacCommitTx]] over the partition's final
      *     cumulative evac-map state; refunds from absorbed-deposit obligations.
      *
      * The first settlement / finalization across the produced lists is the round-2 unlock; the
      * caller (StackComposer) is responsible for marking it as such when building the HardAck
      * payload.
      */
    def deriveRegular(
        partitions: List[Partition]
    ): L1LedgerM[StackEffects.Regular] =
        ??? // PR2: implement per-partition tx construction + treasury rotation

    /** Build the initial-stack (stack 0) effect bundle. The init tx is exogenous (head config); the
      * fallback tx is derived locally from `(initTx + headParams)`.
      *
      * TODO(slow-consensus): implement. Inputs come from the head config / bootstrap data; this
      * function should be callable without an L1LedgerM action since stack 0 happens before any
      * treasury rotation. Likely signature change once StackComposer's `Bootstrap` is in.
      */
    def deriveInitial: StackEffects.Initial =
        ??? // PR2: implement initialization-stack effect construction
}
