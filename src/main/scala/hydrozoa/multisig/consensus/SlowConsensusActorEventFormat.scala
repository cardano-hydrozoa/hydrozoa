package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.SlowConsensusActorEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[SlowConsensusActorEvent]] to [[LogEvent]] for various back-end sinks. */
object SlowConsensusActorEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"SlowConsensusActor.$peerNum"

    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    def humanFormat(peerNum: HeadPeerNumber)(e: SlowConsensusActorEvent): LogEvent = {
        val ev = LogEvent.From(baseCtx(peerNum), routingKey(peerNum))
        import ev.*
        e match {
            case StackHandedOff(sn, phase) =>
                info(
                  s"stack $sn handed off ($phase); broadcasting own acks",
                  "stackNum" -> s"${sn: Int}"
                )
            case Round1Confirmed(sn) =>
                info(
                  s"stack $sn round-1 confirmed; releasing own round-2",
                  "stackNum" -> s"${sn: Int}"
                )
            case StackHardConfirmed(stack) =>
                val sn = stack.brief.stackNum
                info(s"stack $sn HARD-CONFIRMED", "stackNum" -> s"${sn: Int}")
        }
    }

    def jsonlFormat(peerNum: HeadPeerNumber)(e: SlowConsensusActorEvent): Option[LogEvent] = {
        val ts = System.currentTimeMillis()
        val ev = LogEvent.From(Map.empty, "hydrozoa.trace")
        import ev.*
        def htrace(json: String) = info(s"HTRACE|$json")
        e match {
            case StackHandedOff(sn, phase) =>
                Some(
                  htrace(
                    s"""{"ts":$ts,"node":"$peerNum","event":"stack_handed_off","stack_num":${sn: Int},"phase":"$phase"}"""
                  )
                )
            case Round1Confirmed(sn) =>
                Some(
                  htrace(
                    s"""{"ts":$ts,"node":"$peerNum","event":"stack_round1_confirmed","stack_num":${sn: Int}}"""
                  )
                )
            case StackHardConfirmed(stack) =>
                val sn = stack.brief.stackNum
                Some(
                  htrace(
                    s"""{"ts":$ts,"node":"$peerNum","event":"stack_hard_confirmed","stack_num":${sn: Int}}"""
                  )
                )
        }
    }
