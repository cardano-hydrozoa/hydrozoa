package hydrozoa.multisig.consensus

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestNumber

/** Typed events emitted by [[EventSequencer]]. Pure data; formatters in
  * [[EventSequencerEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait EventSequencerEvent

object EventSequencerEvent:
    final case class RequestIdAssigned(peerNum: HeadPeerNumber, requestNum: RequestNumber)
        extends EventSequencerEvent
