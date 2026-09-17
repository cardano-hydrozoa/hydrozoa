package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.StoreCleanupActorEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[StoreCleanupActorEvent]] to [[LogEvent]] for various back-end sinks. */
object StoreCleanupActorEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: StoreCleanupActorEvent): LogEvent = {
        val ev = LogEvent.From.forPeer("StoreCleanupActor", peerNum)
        import ev.*
        e match {
            case AcksPruned(sn, bn, soft, hard) =>
                info(
                  s"stack $sn: pruned $soft soft-acks and $hard hard-acks up to block $bn",
                  "stackNum" -> s"${sn: Int}",
                  "blockNum" -> s"${bn: Int}"
                )
            // Routine once a pass has caught up — but also what a stalled archiver looks like,
            // since an ack survives until the archive holds it. Debug, because on a quiet head
            // this is the common case and an operator watches the retention metrics instead.
            case NothingToPrune(sn) =>
                debug(s"stack $sn: nothing to prune", "stackNum" -> s"${sn: Int}")
            // Deletion is best-effort: retaining more than necessary is safe, so a failed pass is
            // reported and the next one retries from the same floor.
            case PruneFailed(sn, cause) =>
                warn(
                  s"stack $sn: cleanup pass failed; retrying on the next confirmation",
                  "stackNum" -> s"${sn: Int}"
                )
        }
    }
