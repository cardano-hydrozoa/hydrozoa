package hydrozoa.multisig.ledger.stack

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.block.{Block, BlockResult}

/** A closed slow-consensus stack at one of three signing stages.
  *
  *   - [[Stack.Unsigned]] — composed locally (leader) or validated against the leader's brief
  *     (follower); per-effect bodies are derived but no hard-acks are aggregated yet.
  *   - [[Stack.Round1Confirmed]] — round-1 hard-acks (per-effect signatures for everything except
  *     the first settlement / finalization unlock) are saturated.
  *   - [[Stack.HardConfirmed]] — round-2 hard-acks (the unlock tx body) are saturated. For 1-phase
  *     (minor-only) stacks, `round2Acks` is empty and the sole-round acks live inside
  *     [[Stack.Round1Confirmed.round1Acks]].
  */
sealed trait Stack

object Stack:
    final case class Unsigned(
        brief: StackBrief,
        results: NonEmptyList[BlockResult],
        softConfirmations: NonEmptyList[Block.SoftConfirmed],
        effects: StackEffects
    ) extends Stack

    final case class Round1Confirmed(
        unsigned: Unsigned
        // round1Acks: NonEmptyList[HardAckRound1] — populated by SlowConsensusActor (M3/M6)
    ) extends Stack

    final case class HardConfirmed(
        round1: Round1Confirmed
        // round2Acks: List[HardAckRound2] — empty for minor-only 1-phase stacks
    ) extends Stack
