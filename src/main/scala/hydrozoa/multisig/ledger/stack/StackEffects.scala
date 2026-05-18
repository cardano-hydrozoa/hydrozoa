package hydrozoa.multisig.ledger.stack

import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, FinalizationTx, InitializationTx, RefundTx, RolloutTx, SettlementTx}

/** Necessary L1 effects for a closed stack, derived locally by each peer.
  *
  * Signing state is explicit at the type level, mirroring
  * [[hydrozoa.multisig.ledger.block.BlockEffects]]:
  *
  *   - [[StackEffects.Unsigned]] — bodies as derived, no aggregated hard-ack signatures. Held by
  *     [[Stack.Unsigned]] / [[Stack.Round1Confirmed]].
  *   - [[StackEffects.HardConfirmed]] — every head peer's hard-ack signature aggregated in: tx
  *     bodies carry the multisig `VKeyWitness`es; the standalone evac commitment carries every
  *     peer's header signature ([[StandaloneEvacuationCommitment.MultiSigned]]). Held by
  *     [[Stack.HardConfirmed]]; L1-submittable / dispute-usable as is.
  *
  * Not every effect is a transaction: settlement / fallback / rollout / refund / finalization are
  * txs (hard-acked per tx body), but a standalone evac commitment commits a *block header* (KZG
  * lives on the header) and is hard-acked with a header signature, not a tx-body signature — its
  * multisigned form is the set of all peers' header signatures, usable in the rule-based regime's
  * vote tx (see [[StandaloneEvacuationCommitment]]).
  *
  * **Never wire-broadcast** — only [[StackBrief]] travels on the wire. Each peer derives the
  * effects from its own [[hydrozoa.multisig.ledger.block.BlockResult]] stream + the leader's brief;
  * deterministic derivation makes the resulting bytes byte-identical across peers.
  */
sealed trait StackEffects

object StackEffects:

    /** Effects as locally derived — no aggregated hard-ack signatures yet. */
    sealed trait Unsigned extends StackEffects

    object Unsigned:

        /** Stack 0 only. The initialization tx is exogenous (head config).
          *
          *   - Round 1 signs the fallback tx body (locally derived from initTx + head params).
          *   - Round 2 signs the init tx body + each peer's individual key witnesses for utxos
          *     spent from its individual addresses.
          */
        final case class Initial(
            initializationTx: InitializationTx,
            fallbackTx: FallbackTx
        ) extends Unsigned

        /** Stack 1+. Regular slow-consensus flow.
          *
          * The lists are ordered. For [[Regular]] stacks containing at least one major or final
          * block, the **first** element of `settlements ++ finalization.toList` is the "unlock" tx
          * — round-2 hard-ack signs over its body. All other effects are signed in round 1.
          *
          * @param settlements
          *   one per major-partition; the first is the unlock when no finalization precedes it.
          * @param fallbacks
          *   one per major-partition.
          * @param rollouts
          *   rollout txs emitted by each major/final partition's settlement/finalization tx-seq —
          *   one batch per Major (from `mkSettlementTxSeq`) and per Final (from `finalizeLedger`).
          *   Minor-only / TrailingMinors partitions produce no rollouts.
          * @param refunds
          *   post-dated refund txs, one per Minor block that decided a deposit refund (carried in
          *   `BlockResult.postDatedRefundTxs`). Major/Final blocks contribute none — those drain
          *   refunds via settlement/finalization. Not a per-partition quantity.
          * @param evacCommit
          *   the single standalone evacuation commitment of the stack's TrailingMinors partition,
          *   if it has one (`None` otherwise). A stack has AT MOST ONE TrailingMinors partition (it
          *   is necessarily the last — see [[StackPartition]] invariants) ⇒ at most one standalone
          *   commitment. Per spec a *standalone* evac commitment exists only for minor blocks; for
          *   initial/major blocks the evac commitment is implicit in the initialization/settlement
          *   effect (carried to L1 immediately when that effect executes — no standalone record, no
          *   separate signature). It is a contingent / dormant L1 effect — a record
          *   `(headId, blockVersion, kzg)` presented to the L1 dispute-resolution scripts in the
          *   rules-based regime, never submitted immediately, never a treasury mutation. Hard-acked
          *   by signing the minor block's header (KZG lives on the header). Necessary-effects
          *   compression: only the LAST minor of that partition's run is kept (each supersedes its
          *   predecessors).
          * @param finalization
          *   present iff the stack contains a Final block; the unlock when no settlement precedes
          *   it in the stack.
          */
        final case class Regular(
            settlements: List[SettlementTx],
            fallbacks: List[FallbackTx],
            rollouts: List[RolloutTx],
            refunds: List[RefundTx],
            evacCommit: Option[StandaloneEvacuationCommitment],
            finalization: Option[FinalizationTx]
        ) extends Unsigned

    /** Effects with every head peer's hard-ack signature aggregated in: tx bodies carry the
      * multisig `VKeyWitness`es; the standalone evac commitment carries every peer's header
      * signature. L1-submittable (txs) / dispute-usable (evac commitment) as is.
      *
      * The tx wrapper types are unchanged from [[Unsigned]] — the multisig `VKeyWitness`es live
      * inside each [[hydrozoa.multisig.ledger.l1.tx.Tx]] body. Only the evac member differs in
      * type: an [[StandaloneEvacuationCommitment.MultiSigned]] (the dormant record + all peers'
      * header signatures) rather than the bare record.
      */
    sealed trait HardConfirmed extends StackEffects

    object HardConfirmed:

        /** Stack 0, hard-confirmed. @see [[Unsigned.Initial]]. */
        final case class Initial(
            initializationTx: InitializationTx,
            fallbackTx: FallbackTx
        ) extends HardConfirmed

        /** Stack 1+, hard-confirmed. @see [[Unsigned.Regular]]. The only field that differs in type
          * from [[Unsigned.Regular]] is `evacCommit`.
          */
        final case class Regular(
            settlements: List[SettlementTx],
            fallbacks: List[FallbackTx],
            rollouts: List[RolloutTx],
            refunds: List[RefundTx],
            evacCommit: Option[StandaloneEvacuationCommitment.MultiSigned],
            finalization: Option[FinalizationTx]
        ) extends HardConfirmed
