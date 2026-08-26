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
            case CommittedMap(version, size) =>
                debug(s"committed evacuation map at $version holds $size obligation(s)")
            case StackClosed(sn, first, last, isLeader) =>
                val role = if isLeader then "Leader" else "Follower"
                info(
                  s"$role closing stack $sn with blocks $first..$last",
                  "stackNum" -> s"${sn: Int}"
                )
            case PreviousStackHardConfirmed(sn) =>
                info(
                  s"single-flight gate OPEN: stack $sn hard-confirmed",
                  "stackNum" -> s"${sn: Int}"
                )
            case SingleFlightGateClosed(sn) =>
                info(
                  s"single-flight gate CLOSED on stack $sn; awaiting its hard confirmation",
                  "stackNum" -> s"${sn: Int}"
                )
            case HardConfirmationReconciled(awaited, persisted) =>
                warn(
                  s"single-flight gate OPEN by reconciliation: awaited stack $awaited, " +
                      s"persisted hard confirmation is $persisted — the HardConfirmed message " +
                      s"for $awaited never arrived",
                  "stackNum" -> s"${awaited: Int}"
                )
            case StructuralDivergence(sn, lFirst, lLast, expected) =>
                warn(
                  s"Follower stack $sn structural divergence: leader brief [$lFirst..$lLast] but expected to start at $expected",
                  "stackNum" -> s"${sn: Int}"
                )
        }
    }
