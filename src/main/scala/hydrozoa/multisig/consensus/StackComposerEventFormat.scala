package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.StackComposerEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[StackComposerEvent]] to [[LogEvent]] for various back-end sinks. */
object StackComposerEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: StackComposerEvent): LogEvent = {
        val ev = LogEvent.From.forPeer("StackComposer", peerNum)
        import ev.*
        e match {
            case InitialStackBootstrapped =>
                info("bootstrapping initial stack 0 (init + fallback)")
            case StackClosed(sn, first, last, isLeader) =>
                val role = if isLeader then "Leader" else "Follower"
                info(
                  s"$role closing stack $sn with blocks $first..$last",
                  "stackNum" -> s"${sn: Int}"
                )
            case StructuralDivergence(sn, lFirst, lLast, expected) =>
                warn(
                  s"Follower stack $sn structural divergence: leader brief [$lFirst..$lLast] but expected to start at $expected",
                  "stackNum" -> s"${sn: Int}"
                )
        }
    }
