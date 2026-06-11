package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.FastConsensusActorEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[FastConsensusActorEvent]] to [[LogEvent]] for various back-end sinks. Lives
  * separately from the event ADT so the type itself stays pure data, and callers (Main / harness)
  * compose
  * `Slf4jTracer.sink.contramap(humanFormat(peer)) |+| Slf4jTracer.sink.traceMaybe(jsonlFormat(nodeId))`.
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

    /** Routes only protocol-trace-worthy events to the `hydrozoa.trace` JSONL logger; returns
      * `None` for everything else (passed to `traceMaybe`).
      */
    def jsonlFormat(nodeId: HeadPeerNumber)(e: FastConsensusActorEvent): Option[LogEvent] = {
        val ts = System.currentTimeMillis()
        val ev = LogEvent.From(Map.empty, "hydrozoa.trace")
        import ev.*
        def htrace(json: String) = info(s"HTRACE|$json")
        e match {
            case AckReceived(bn, p, ackType, _) =>
                Some(
                  htrace(
                    s"""{"ts":$ts,"node":"$nodeId","event":"ack","block_num":${bn: Int},"peer":${p: Int},"ack_type":"$ackType"}"""
                  )
                )
            case BlockSoftConfirmed(bn, bt, vMaj, vMin) =>
                Some(
                  htrace(
                    s"""{"ts":$ts,"node":"$nodeId","event":"block_confirmed","block_num":${bn: Int},"block_type":"$bt","v_major":$vMaj,"v_minor":$vMin}"""
                  )
                )
            case LeaderStarted(bn, p) =>
                Some(
                  htrace(
                    s"""{"ts":$ts,"node":"$nodeId","event":"leader_started","block_num":${bn: Int},"peer":${p: Int}}"""
                  )
                )
        }
    }
