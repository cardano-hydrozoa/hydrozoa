package hydrozoa.multisig.consensus.liaison

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEvent.*
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}

/** Renderers from [[PeerLiaisonEvent]] to [[LogEvent]]. */
object PeerLiaisonEventFormat:

    def humanFormat(own: HeadPeerNumber, remote: PeerId)(e: PeerLiaisonEvent): LogEvent = {
        val remoteLabel = renderPeerLabel(remote)
        val ev = LogEvent.From(
          Map("peer" -> own.toString, "remote" -> remoteLabel),
          s"PeerLiaison.$own->$remoteLabel"
        )
        import ev.*
        e match {
            case Started =>
                info(s"starting, remote peer: $remoteLabel")
            case StaleBatchDropped(received, outstanding) =>
                debug(s"dropping stale reply batch=$received (outstanding=$outstanding)")
            case BatchRejected(batchNum, reason) =>
                warn(s"rejecting reply batch=$batchNum: $reason")
        }
    }

    /** Short peer label matching `OwnPeerPublic.ownPeerLabel` (`0` for head 0, `c0` for coil 0). */
    private def renderPeerLabel(peerId: PeerId): String = peerId match {
        case PeerId.Head(n) => s"${n: Int}"
        case PeerId.Coil(n) => s"c${n: Int}"
    }
