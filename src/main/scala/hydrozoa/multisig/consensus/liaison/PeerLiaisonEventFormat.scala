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
            case BatchRequested(batchNum, detail) =>
                debug(s"-> GetMsgBatch=$batchNum ${detail.value}")
            case BatchReceived(batchNum, detail) =>
                debug(s"<- NewMsgBatch=$batchNum ${detail.value}")
            case StaleBatchDropped(received, outstanding) =>
                debug(s"dropping stale reply batch=$received (outstanding=$outstanding)")
            case BatchRejected(batchNum, reason) =>
                warn(s"rejecting reply batch=$batchNum: $reason")
            case CoilSeeded(startStack, ownHardAck) =>
                info(s"seeding coil at stack=$startStack, own hard-ack from $ownHardAck")
            case CoilCaughtUp =>
                info("coil is within catch-up range; serving it from where it stands")
            case CoilNotSeeded(reason) =>
                info(s"no start point to offer ($reason); coil bootstraps stack 0")
            case JoinOfferTooLate(startStack) =>
                warn(s"declining a start point at stack=$startStack: past join mode")
            case JoinStarted =>
                info("join mode: announced marks, waiting for the hub's answer")
            case JoinStillWaiting =>
                warn("join mode: cold store and no answer yet — this node cannot boot without one")
            case JoinAdopting(startStack) =>
                info(s"join mode: adopting a start point at stack=$startStack (wipes this store)")
            case JoinAdopted(startStack) =>
                info(s"join mode: adopted stack=$startStack; becoming the regular coil liaison")
            case JoinNothingToAdopt(reason) =>
                info(s"join mode: hub has nothing to seed from ($reason)")
            case JoinHubSilent(waited) =>
                warn(s"join mode: no answer within $waited; proceeding on own history")
            case JoinIgnoredServe =>
                debug("join mode: ignoring a served batch until the start point is settled")
            case JoinIgnoredLocal =>
                warn("join mode: ignoring a local artifact — no local actor should exist yet")
            case CoilHardAckHeadRefused(hub, askedStack, ceilingStack) =>
                debug(
                  s"coil-hard-ack lane head refused: hub=$hub askedStack=$askedStack " +
                      s"ceilingStack=$ceilingStack"
                )
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
