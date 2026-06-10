package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.consensus.PeerLiaisonEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[PeerLiaisonEvent]] to [[LogEvent]] for various back-end sinks.
  *
  * Three format functions are provided:
  *   - [[humanFormat]] — one structured line per event to the per-link SLF4J logger.
  *   - [[mermaidFormat]] — `Some` for events that also emit a Mermaid sequence-diagram line; `None`
  *     for the rest. Wire as `Slf4jTracer.sink.traceMaybe(mermaidFormat(own, remote))`.
  */
object PeerLiaisonEventFormat:

    private def routingKey(own: HeadPeerNumber, remote: HeadPeerNumber): String =
        s"PeerLiaison.$own->$remote"

    private def baseCtx(own: HeadPeerNumber, remote: HeadPeerNumber): Map[String, String] =
        Map("peer" -> own.toString, "remote" -> remote.toString)

    def humanFormat(own: HeadPeerNumber, remote: HeadPeerNumber)(e: PeerLiaisonEvent): LogEvent = {
        val rk = Some(routingKey(own, remote))
        val ctx0 = baseCtx(own, remote)
        def debug(msg: String) = LogEvent(Level.Debug, msg, ctx0, routingKey = rk)
        def info(msg: String) = LogEvent(Level.Info, msg, ctx0, routingKey = rk)
        def warn(msg: String) = LogEvent(Level.Warn, msg, ctx0, routingKey = rk)
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

    /** Returns a Mermaid sequence-diagram line for events that are part of the `GetMsgBatch` /
      * `NewMsgBatch` exchange, `None` for all others.
      */
    def mermaidFormat(own: HeadPeerNumber, remote: HeadPeerNumber)(
        e: PeerLiaisonEvent
    ): Option[LogEvent] = {
        val rk = Some("Mermaid.PeerLiaison")
        def mmd(s: String) =
            LogEvent(Level.Info, s"\t$own->>$remote: $s", routingKey = rk)
        e match {
            case Started(_) =>
                Some(mmd("GetMsgBatch.initial"))
            case NewMsgBatchSent(b, ack, block, stack, hard, reqs) =>
                Some(
                  mmd(
                    s"NewMsgBatch: batch=$b, ack=$ack, block=$block, " +
                        s"stackBrief=$stack, hardAck=$hard, reqs=$reqs"
                  )
                )
            case BatchAdvanced(b, ack, block, stack, hard, req) =>
                Some(
                  mmd(
                    s"GetMsgBatch: batch=$b, ack=$ack, block=$block, " +
                        s"stackBrief=$stack, hardAck=$hard, req=$req"
                  )
                )
            case _ => None
        }
    }
