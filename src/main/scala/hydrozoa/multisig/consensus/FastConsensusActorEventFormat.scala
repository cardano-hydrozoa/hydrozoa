package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.consensus.FastConsensusActorEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[FastConsensusActorEvent]] to [[LogEvent]] for various back-end sinks. Lives
  * separately from the event ADT so the type itself stays pure data, and callers (Main / harness)
  * compose
  * `Tracer.sink.contramap(humanFormat(peer)) |+| Tracer.sink.traceMaybe(jsonlFormat(nodeId))`.
  */
object FastConsensusActorEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"FastConsensusActor.$peerNum"

    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    /** Routes every event to SLF4J text. */
    def humanFormat(peerNum: HeadPeerNumber)(e: FastConsensusActorEvent): LogEvent = {
        val rk = Some(routingKey(peerNum))
        val ctx0 = baseCtx(peerNum)
        e match {
            case AckReceived(bn, p, ackType, isOwn) =>
                LogEvent(
                  Level.Debug,
                  s"handleAck: block=$bn peer=$p" + (if isOwn then " (own)" else ""),
                  ctx0 ++ Map(
                    "blockNum" -> s"${bn: Int}",
                    "ackPeer" -> p.toString,
                    "ackType" -> ackType
                  ),
                  routingKey = rk
                )
            case BlockSoftConfirmed(bn, bt, vMaj, vMin) =>
                LogEvent(
                  Level.Info,
                  s"block soft-confirmed: block=$bn type=$bt v$vMaj.$vMin",
                  ctx0 ++ Map(
                    "blockNum" -> s"${bn: Int}",
                    "blockType" -> bt
                  ),
                  routingKey = rk
                )
            case LeaderStarted(bn, p) =>
                LogEvent(
                  Level.Info,
                  s"leader started: block=$bn peer=$p",
                  ctx0 ++ Map("blockNum" -> s"${bn: Int}"),
                  routingKey = rk
                )
        }
    }

    /** Routes only protocol-trace-worthy events to the `hydrozoa.trace` JSONL logger; returns
      * `None` for everything else (passed to `traceMaybe`).
      */
    def jsonlFormat(nodeId: HeadPeerNumber)(e: FastConsensusActorEvent): Option[LogEvent] = {
        val ts = System.currentTimeMillis()
        val rk = Some("hydrozoa.trace")
        e match {
            case AckReceived(bn, p, ackType, _) =>
                Some(
                  LogEvent(
                    Level.Info,
                    s"""HTRACE|{"ts":$ts,"node":"$nodeId","event":"ack","block_num":${bn: Int},"peer":${p: Int},"ack_type":"$ackType"}""",
                    routingKey = rk
                  )
                )
            case BlockSoftConfirmed(bn, bt, vMaj, vMin) =>
                Some(
                  LogEvent(
                    Level.Info,
                    s"""HTRACE|{"ts":$ts,"node":"$nodeId","event":"block_confirmed","block_num":${bn: Int},"block_type":"$bt","v_major":$vMaj,"v_minor":$vMin}""",
                    routingKey = rk
                  )
                )
            case LeaderStarted(bn, p) =>
                Some(
                  LogEvent(
                    Level.Info,
                    s"""HTRACE|{"ts":$ts,"node":"$nodeId","event":"leader_started","block_num":${bn: Int},"peer":${p: Int}}""",
                    routingKey = rk
                  )
                )
        }
    }
