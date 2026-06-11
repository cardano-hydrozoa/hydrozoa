package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.FastConsensusActorEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[FastConsensusActorEvent]] to [[LogEvent]]. Lives separately from the event ADT
  * so the type itself stays pure data.
  */
object FastConsensusActorEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"FastConsensusActor.$peerNum"

    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    /** Routes every event to SLF4J text. */
    def humanFormat(peerNum: HeadPeerNumber)(e: FastConsensusActorEvent): LogEvent = {
        val ev = LogEvent.From(baseCtx(peerNum), routingKey(peerNum))
        import ev.*
        e match {
            case AckReceived(bn, p, ackType, isOwn) =>
                debug(
                  s"handleAck: block=$bn peer=$p" + (if isOwn then " (own)" else ""),
                  "blockNum" -> s"${bn: Int}",
                  "ackPeer" -> p.toString,
                  "ackType" -> ackType
                )
            case BlockSoftConfirmed(bn, bt, vMaj, vMin) =>
                info(
                  s"block soft-confirmed: block=$bn type=$bt v$vMaj.$vMin",
                  "blockNum" -> s"${bn: Int}",
                  "blockType" -> bt
                )
            case LeaderStarted(bn, p) =>
                info(s"leader started: block=$bn peer=$p", "blockNum" -> s"${bn: Int}")
        }
    }
