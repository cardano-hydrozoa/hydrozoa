package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.SlowConsensusActorEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[SlowConsensusActorEvent]] to [[LogEvent]] for various back-end sinks. */
object SlowConsensusActorEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: SlowConsensusActorEvent): LogEvent = {
        val ev = LogEvent.From.forPeer("SlowConsensusActor", peerNum)
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
