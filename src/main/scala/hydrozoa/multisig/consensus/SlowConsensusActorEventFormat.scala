package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.consensus.SlowConsensusActorEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[SlowConsensusActorEvent]] to [[LogEvent]] for various back-end sinks. */
object SlowConsensusActorEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"SlowConsensusActor.$peerNum"

    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    def humanFormat(peerNum: HeadPeerNumber)(e: SlowConsensusActorEvent): LogEvent = {
        val rk = Some(routingKey(peerNum))
        val ctx0 = baseCtx(peerNum)
        e match {
            case StackHandedOff(sn, phase) =>
                LogEvent(
                  Level.Info,
                  s"stack $sn handed off ($phase); broadcasting own acks",
                  ctx0 ++ Map("stackNum" -> s"${sn: Int}"),
                  routingKey = rk
                )
            case Round1Confirmed(sn) =>
                LogEvent(
                  Level.Info,
                  s"stack $sn round-1 confirmed; releasing own round-2",
                  ctx0 ++ Map("stackNum" -> s"${sn: Int}"),
                  routingKey = rk
                )
            case StackHardConfirmed(stack) =>
                val sn = stack.brief.stackNum
                LogEvent(
                  Level.Info,
                  s"stack $sn HARD-CONFIRMED",
                  ctx0 ++ Map("stackNum" -> s"${sn: Int}"),
                  routingKey = rk
                )
        }
    }

    def jsonlFormat(peerNum: HeadPeerNumber)(e: SlowConsensusActorEvent): Option[LogEvent] = {
        val ts = System.currentTimeMillis()
        val rk = Some("hydrozoa.trace")
        e match {
            case StackHandedOff(sn, phase) =>
                Some(
                  LogEvent(
                    Level.Info,
                    s"""HTRACE|{"ts":$ts,"node":"$peerNum","event":"stack_handed_off","stack_num":${sn: Int},"phase":"$phase"}""",
                    routingKey = rk
                  )
                )
            case Round1Confirmed(sn) =>
                Some(
                  LogEvent(
                    Level.Info,
                    s"""HTRACE|{"ts":$ts,"node":"$peerNum","event":"stack_round1_confirmed","stack_num":${sn: Int}}""",
                    routingKey = rk
                  )
                )
            case StackHardConfirmed(stack) =>
                val sn = stack.brief.stackNum
                Some(
                  LogEvent(
                    Level.Info,
                    s"""HTRACE|{"ts":$ts,"node":"$peerNum","event":"stack_hard_confirmed","stack_num":${sn: Int}}""",
                    routingKey = rk
                  )
                )
        }
    }
