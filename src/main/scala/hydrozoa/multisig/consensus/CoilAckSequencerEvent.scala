package hydrozoa.multisig.consensus

import hydrozoa.multisig.consensus.ack.{HardAckNumber, HubHardAckNumber}
import hydrozoa.multisig.consensus.peer.CoilPeerNumber

/** Typed events emitted by [[CoilAckSequencer]]. Pure data; formatters in
  * [[CoilAckSequencerEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait CoilAckSequencerEvent

object CoilAckSequencerEvent:

    /** A coil peer's hard-ack was stamped with the hub-local sequence number and fanned out. */
    final case class SequencedCoilAck(
        coilNum: CoilPeerNumber,
        hardAckNum: HardAckNumber,
        seqNum: HubHardAckNumber
    ) extends CoilAckSequencerEvent
