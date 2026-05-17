package hydrozoa.multisig.ledger.stack

import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, FinalizationTx, InitializationTx, RefundTx, RolloutTx, SettlementTx, StandaloneEvacuationCommitment}

/** Necessary L1 effects for a closed stack, derived locally by each peer.
  *
  * Not every effect is a transaction: settlement / fallback / rollout / refund / finalization are
  * txs (hard-acked per tx body), but a standalone evac commitment commits a *block header* (KZG
  * lives on the header) and is hard-acked with a header signature, not a tx-body signature — see
  * [[hydrozoa.multisig.ledger.l1.tx.StandaloneEvacuationCommitment]].
  *
  * **Never wire-broadcast** — only [[StackBrief]] travels on the wire. Each peer derives the
  * effects from its own [[hydrozoa.multisig.ledger.block.BlockResult]] stream + the leader's brief;
  * deterministic derivation makes the resulting bytes byte-identical across peers.
  */
sealed trait StackEffects

object StackEffects:

    /** Stack 1+. Regular slow-consensus flow.
      *
      * The lists are ordered. For [[Regular]] stacks containing at least one major or final block,
      * the **first** element of `settlements ++ finalization.toList` is the "unlock" tx — round-2
      * hard-ack signs over its body. All other effects are signed in round 1.
      *
      * @param settlements
      *   one per major-partition; the first is the unlock when no finalization precedes it.
      * @param fallbacks
      *   one per major-partition.
      * @param rollouts
      *   rollout txs emitted by each major/final partition's settlement/finalization tx-seq — one
      *   batch per Major (from `mkSettlementTxSeq`) and per Final (from `finalizeLedger`).
      *   Minor-only / TrailingMinors partitions produce no rollouts.
      * @param refunds
      *   post-dated refund txs, one per Minor block that decided a deposit refund (carried in
      *   `BlockResult.postDatedRefundTxs`). Major/Final blocks contribute none — those drain
      *   refunds via settlement/finalization. Not a per-partition quantity.
      * @param evacCommits
      *   one per **TrailingMinors** partition only (the partition's LAST minor block). Per spec, a
      *   *standalone* evacuation commitment exists only for minor blocks; for initial/major blocks
      *   the evacuation commitment is implicit in the initialization/settlement effect (carried to
      *   L1 immediately when that effect executes — no standalone record, no separate signature).
      *   It is a contingent / dormant L1 effect — a record `(headId, blockVersion, kzg)` presented
      *   to the L1 dispute-resolution scripts in the rules-based regime, never submitted
      *   immediately, never a treasury mutation. Hard-acked by signing the minor block's header
      *   (KZG lives on the header). Necessary-effects compression: only the LAST standalone
      *   commitment of the partition's minor run is kept (each supersedes its predecessors).
      * @param finalization
      *   present iff the stack contains a Final block; the unlock when no settlement precedes it in
      *   the stack.
      */
    final case class Regular(
        settlements: List[SettlementTx],
        fallbacks: List[FallbackTx],
        rollouts: List[RolloutTx],
        refunds: List[RefundTx],
        evacCommits: List[StandaloneEvacuationCommitment],
        finalization: Option[FinalizationTx]
    ) extends StackEffects

    /** Stack 0 only. The initialization tx is exogenous (head config).
      *
      *   - Round 1 signs the fallback tx body (locally derived from initTx + head params).
      *   - Round 2 signs the init tx body + each peer's individual key witnesses for utxos spent
      *     from its individual addresses.
      */
    final case class Initial(
        initializationTx: InitializationTx,
        fallbackTx: FallbackTx
    ) extends StackEffects
