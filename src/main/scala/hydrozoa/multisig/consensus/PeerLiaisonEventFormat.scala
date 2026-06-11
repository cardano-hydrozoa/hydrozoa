package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.PeerLiaisonEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[PeerLiaisonEvent]] to [[LogEvent]]. */
object PeerLiaisonEventFormat:

    def humanFormat(own: HeadPeerNumber, remote: HeadPeerNumber)(e: PeerLiaisonEvent): LogEvent = {
        val ev = LogEvent.From(
          Map("peer" -> own.toString, "remote" -> remote.toString),
          s"PeerLiaison.$own->$remote"
        )
        import ev.*
        e match {
            case Started(remotePeerNum) =>
                info(s"starting, remote peer: $remotePeerNum")
            case ResendTick(b, ack, block, stack, hard, req) =>
                debug(
                  s"resend tick: GetMsgBatch batch=$b, ack=$ack, block=$block, " +
                      s"stackBrief=$stack, hardAck=$hard, req=$req"
                )
            case OutboxRequest(peerNum, requestNum) =>
                debug(s"outbox: request ($peerNum:$requestNum)")
            case OutboxSoftAck(blockNum, peerNum) =>
                debug(s"outbox: soft ack block=$blockNum peer=$peerNum")
            case OutboxBlock(blockNum) =>
                debug(s"outbox: block block=$blockNum")
            case OutboxStackBrief(stackNum) =>
                debug(s"outbox: stack brief stack=$stackNum")
            case OutboxHardAck(stackNum, peerNum, round) =>
                debug(s"outbox: hard ack stack=$stackNum peer=$peerNum round=$round")
            case GetMsgBatchReceived(b, ack, block, stack, hard, req) =>
                debug(
                  s"Got GetMsgBatch: batch=$b, ack=$ack, block=$block, " +
                      s"stackBrief=$stack, hardAck=$hard, req=$req"
                )
            case NewMsgBatchSent(b, ack, block, stack, hard, reqs) =>
                debug(
                  s"sending NewMsgBatch: batch=$b, ack=$ack, block=$block, " +
                      s"stackBrief=$stack, hardAck=$hard, reqs=$reqs"
                )
            case NewMsgBatchReceived(b, ack, block, stack, hard, reqCount) =>
                debug(
                  s"got NewMsgBatch: batch=$b, ack=$ack, block=$block, " +
                      s"stackBrief=$stack, hardAck=$hard, reqs=$reqCount"
                )
            case BatchAdvanced(b, ack, block, stack, hard, req) =>
                debug(
                  s"advancing GetMsgBatch: batch=$b, ack=$ack, block=$block, " +
                      s"stackBrief=$stack, hardAck=$hard, req=$req"
                )
            case BatchRejected(batchNum, reason) =>
                warn(s"dropping NewMsgBatch batch=$batchNum: $reason")
            case BlockConfirmedNonFinal(blockNum) =>
                debug(s"Got BlockConfirmed (non-final): block=$blockNum")
            case BlockConfirmedFinal(blockNum) =>
                debug(s"Got BlockConfirmed (final): block=$blockNum")
        }
    }
