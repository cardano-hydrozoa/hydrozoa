package hydrozoa.multisig.consensus.liaison

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEvent.*
import hydrozoa.multisig.consensus.peer.PeerId

/** Renderers from [[PeerLiaisonEvent]] to [[LogEvent]]. */
object PeerLiaisonEventFormat:

    /** Routes under `PeerLiaison.<kind>.<own>-><remote>` — the `PeerLiaison` parent tunes all
      * liaisons at once, a `PeerLiaison.<kind>` child tunes one liaison kind.
      */
    def humanFormat(own: PeerId, remote: PeerId)(e: PeerLiaisonEvent): LogEvent = {
        val ownLabel = renderPeerLabel(own)
        val remoteLabel = renderPeerLabel(remote)
        val ev = LogEvent.From(
          Map("peer" -> ownLabel, "remote" -> remoteLabel),
          s"PeerLiaison.${renderLiaisonKind(own, remote)}.$ownLabel->$remoteLabel"
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

    /** Route segment naming the liaison kind, read off the (own, remote) peer kinds. */
    private def renderLiaisonKind(own: PeerId, remote: PeerId): String = (own, remote) match {
        case (PeerId.Head(_), PeerId.Head(_)) => "HeadToHead"
        case (PeerId.Head(_), PeerId.Coil(_)) => "HubToCoil"
        case (PeerId.Coil(_), PeerId.Head(_)) => "CoilToHub"
        // No coil-to-coil liaison exists; rendered totally so the formatter cannot throw.
        case (PeerId.Coil(_), PeerId.Coil(_)) => "CoilToCoil"
    }
