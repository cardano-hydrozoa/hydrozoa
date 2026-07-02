package hydrozoa.multisig.consensus.transport

import hydrozoa.multisig.consensus.peer.HeadPeerId

/** Events emitted by [[FirewalledPeerTransport]] — one variant per firewalled channel. */
sealed trait FirewalledPeerTransportEvent

object FirewalledPeerTransportEvent:
    final case class DroppedOutbound(remote: HeadPeerId) extends FirewalledPeerTransportEvent
