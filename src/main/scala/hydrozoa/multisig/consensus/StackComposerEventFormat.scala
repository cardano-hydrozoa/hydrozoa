package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.consensus.StackComposerEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[StackComposerEvent]] to [[LogEvent]] for various back-end sinks. */
object StackComposerEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"StackComposer.$peerNum"

    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    def humanFormat(peerNum: HeadPeerNumber)(e: StackComposerEvent): LogEvent = {
        val rk = Some(routingKey(peerNum))
        val ctx0 = baseCtx(peerNum)
        e match {
            case InitialStackBootstrapped =>
                LogEvent(
                  Level.Info,
                  "bootstrapping initial stack 0 (init + fallback)",
                  ctx0,
                  routingKey = rk
                )
            case StackClosed(sn, first, last, isLeader) =>
                val role = if isLeader then "Leader" else "Follower"
                LogEvent(
                  Level.Info,
                  s"$role closing stack $sn with blocks $first..$last",
                  ctx0 ++ Map("stackNum" -> s"${sn: Int}"),
                  routingKey = rk
                )
            case StructuralDivergence(sn, lFirst, lLast, expected) =>
                LogEvent(
                  Level.Warn,
                  s"Follower stack $sn structural divergence: leader brief [$lFirst..$lLast] but expected to start at $expected",
                  ctx0 ++ Map("stackNum" -> s"${sn: Int}"),
                  routingKey = rk
                )
        }
    }

    def jsonlFormat(peerNum: HeadPeerNumber)(e: StackComposerEvent): Option[LogEvent] = {
        val ts = System.currentTimeMillis()
        val rk = Some("hydrozoa.trace")
        e match {
            case InitialStackBootstrapped => None
            case StackClosed(sn, first, last, isLeader) =>
                Some(
                  LogEvent(
                    Level.Info,
                    s"""HTRACE|{"ts":$ts,"node":"$peerNum","event":"stack_closed","stack_num":${sn: Int},"first_block":${first: Int},"last_block":${last: Int},"is_leader":$isLeader}""",
                    routingKey = rk
                  )
                )
            case StructuralDivergence(sn, lFirst, lLast, expected) =>
                Some(
                  LogEvent(
                    Level.Warn,
                    s"""HTRACE|{"ts":$ts,"node":"$peerNum","event":"stack_structural_divergence","stack_num":${sn: Int},"leader_first":${lFirst: Int},"leader_last":${lLast: Int},"expected_first":${expected: Int}}""",
                    routingKey = rk
                  )
                )
        }
    }
