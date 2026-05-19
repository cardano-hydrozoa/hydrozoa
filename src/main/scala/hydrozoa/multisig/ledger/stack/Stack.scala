package hydrozoa.multisig.ledger.stack

/** A closed slow-consensus stack at one of three signing stages.
  *
  *   - [[Stack.Unsigned]] — composed locally (leader) or validated against the leader's brief
  *     (follower); per-effect bodies are derived but no hard-acks are aggregated yet.
  *   - [[Stack.Round1Confirmed]] — round-1 hard-acks (per-effect signatures for everything except
  *     the first settlement / finalization unlock) are saturated.
  *   - [[Stack.HardConfirmed]] — every required round is saturated (round 2 / the unlock for
  *     2-phase stacks; the sole round for minor-only ones). The collected per-peer hard-ack
  *     signatures have been aggregated into `VKeyWitness`es and attached onto the effect tx bodies:
  *     [[HardConfirmed.effects]] is the **multisigned** effect set, submittable on L1 as is. (The
  *     raw acks have served their purpose at this point — they are verified and aggregated by
  *     [[hydrozoa.multisig.consensus.SlowConsensusActor]], not carried further.)
  */
sealed trait Stack

object Stack:
    /** @param brief
      *   the stack's composition (the only thing wire-broadcast)
      * @param effects
      *   the locally-derived, partition-indexed effects. `BlockResult`s are a construction-only
      *   input (consumed by partitioning + derivation in `StackComposer.mkUnsigned`) and are NOT
      *   retained — every datum the slow side still needs (incl. each SEC's minor header bytes) is
      *   carried on `effects` itself (PR #446 review).
      */
    final case class Unsigned(
        brief: StackBrief,
        effects: StackEffects.Unsigned
    ) extends Stack

    final case class Round1Confirmed(
        unsigned: Unsigned
        // round1Acks live transiently inside SlowConsensusActor's cell; once round 2 / sole
        // saturates they are aggregated into witnesses on HardConfirmed.effects, not stored here.
    ) extends Stack

    /** @param effects
      *   the partition-indexed effects with every head peer's signature aggregated in: tx bodies
      *   carry the multisig `VKeyWitness`es; each partition's standalone evac commitment is a
      *   [[StandaloneEvacuationCommitment.MultiSigned]] (the dormant record + all peers' header
      *   signatures). Tx bodies are L1-submittable as is; the SEC is dispute-usable.
      */
    final case class HardConfirmed(
        round1: Round1Confirmed,
        effects: StackEffects.HardConfirmed
    ) extends Stack
