package hydrozoa.multisig.consensus

import hydrozoa.multisig.ledger.stack.StackNumber

/** Typed events emitted by [[SlowConsensusActor]]. Pure data; formatters in
  * [[SlowConsensusActorEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait SlowConsensusActorEvent

object SlowConsensusActorEvent:

    /** Stack handed off from StackComposer: own acks verified, cell created, own acks broadcast.
      * `phase` is `"2-phase"` or `"sole"`.
      */
    final case class StackHandedOff(stackNum: StackNumber, phase: String)
        extends SlowConsensusActorEvent

    /** Round-1 saturated: all peers' round-1 acks collected; own round-2 ack released. */
    final case class Round1Confirmed(stackNum: StackNumber) extends SlowConsensusActorEvent

    /** All acks collected and aggregated; [[Stack.HardConfirmed]] emitted downstream. */
    final case class StackHardConfirmed(stackNum: StackNumber) extends SlowConsensusActorEvent
