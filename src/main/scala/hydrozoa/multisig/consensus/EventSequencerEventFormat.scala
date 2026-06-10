package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.consensus.EventSequencerEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[EventSequencerEvent]] to [[LogEvent]] for various back-end sinks. */
object EventSequencerEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"EventSequencer.$peerNum"

    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    def humanFormat(peerNum: HeadPeerNumber)(e: EventSequencerEvent): LogEvent = {
        val rk = Some(routingKey(peerNum))
        val ctx0 = baseCtx(peerNum)
        def debug(msg: String) = LogEvent(Level.Debug, msg, ctx0, routingKey = rk)
        e match {
            case RequestIdAssigned(peer, requestNum) =>
                debug(s"Assigned request ID ($peer:$requestNum)")
        }
    }
