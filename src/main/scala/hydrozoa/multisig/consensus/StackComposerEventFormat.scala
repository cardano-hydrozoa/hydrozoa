package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.StackComposerEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[StackComposerEvent]] to [[LogEvent]] for various back-end sinks. */
object StackComposerEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"StackComposer.$peerNum"

    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    def humanFormat(peerNum: HeadPeerNumber)(e: StackComposerEvent): LogEvent = {
        val ev = LogEvent.From(baseCtx(peerNum), routingKey(peerNum))
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

    def jsonlFormat(peerNum: HeadPeerNumber)(e: StackComposerEvent): Option[LogEvent] = {
        val ts = System.currentTimeMillis()
        val ev = LogEvent.From(Map.empty, "hydrozoa.trace")
        import ev.*
        e match {
            case InitialStackBootstrapped => None
            case StackClosed(sn, first, last, isLeader) =>
                Some(
                  info(
                    s"""HTRACE|{"ts":$ts,"node":"$peerNum","event":"stack_closed","stack_num":${sn: Int},"first_block":${first: Int},"last_block":${last: Int},"is_leader":$isLeader}"""
                  )
                )
            case StructuralDivergence(sn, lFirst, lLast, expected) =>
                Some(
                  warn(
                    s"""HTRACE|{"ts":$ts,"node":"$peerNum","event":"stack_structural_divergence","stack_num":${sn: Int},"leader_first":${lFirst: Int},"leader_last":${lLast: Int},"expected_first":${expected: Int}}"""
                  )
                )
        }
    }
