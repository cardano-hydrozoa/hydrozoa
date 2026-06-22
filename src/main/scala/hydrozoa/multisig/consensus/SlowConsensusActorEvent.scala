package hydrozoa.multisig.consensus

import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.stack.{Stack, StackNumber}

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

    /** This peer's own hard-ack arrived back on the hub hard-ack lane (a hub re-publishes each coil
      * peer's acks to every coil peer it serves, the author included) and was dropped — the
      * original is already held locally.
      */
    final case class OwnHardAckEchoIgnored(stackNum: StackNumber) extends SlowConsensusActorEvent

    /** A surplus hard-ack was ignored rather than raised as a protocol violation: a round-1 ack
      * from a coil peer beyond the round's `coilQuorum` arriving after the cell already advanced
      * past round 1, or any late ack for a stack already hard-confirmed (its cell dropped). The
      * signer contributes no witness — `selectSigners` keeps only `coilQuorum` coils — so the ack
      * is redundant and dropped.
      */
    final case class SurplusHardAckIgnored(stackNum: StackNumber, peer: PeerId)
        extends SlowConsensusActorEvent

    /** All acks collected and aggregated; [[Stack.HardConfirmed]] emitted downstream. */
    final case class StackHardConfirmed(stack: Stack.HardConfirmed) extends SlowConsensusActorEvent
