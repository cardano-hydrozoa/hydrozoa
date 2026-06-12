package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.EventSequencerEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[EventSequencerEvent]] to [[LogEvent]] for various back-end sinks. */
object EventSequencerEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: EventSequencerEvent): LogEvent = {
        val ev = LogEvent.From.forPeer("EventSequencer", peerNum)
        import ev.*
        e match {
            case RequestIdAssigned(peer, requestNum) =>
                debug(s"Assigned request ID ($peer:$requestNum)")
        }
    }
