package hydrozoa.multisig.ledger.stack

import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, FinalizationTx, InitializationTx, RefundTx, RolloutTx, SettlementTx, StandaloneEvacCommitTx}

/** Necessary L1 effects for a closed stack, derived locally by each peer.
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
      *   per-partition rollout txs.
      * @param refunds
      *   per-partition refund txs.
      * @param evacCommits
      *   ≤ one per partition: the LAST evac commitment of each partition that ends without a major
      *   to absorb it. Earlier standalones in the same partition are dropped by the
      *   necessary-effects compression.
      * @param finalization
      *   present iff the stack contains a Final block; the unlock when no settlement precedes it in
      *   the stack.
      */
    final case class Regular(
        settlements: List[SettlementTx],
        fallbacks: List[FallbackTx],
        rollouts: List[RolloutTx],
        refunds: List[RefundTx],
        evacCommits: List[StandaloneEvacCommitTx],
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
