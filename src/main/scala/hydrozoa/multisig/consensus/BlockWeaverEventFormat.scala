package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.consensus.BlockWeaverEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[BlockWeaverEvent]] to [[LogEvent]] for various back-end sinks. */
object BlockWeaverEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"BlockWeaver.$peerNum"

    def humanFormat(peerNum: HeadPeerNumber)(e: BlockWeaverEvent): LogEvent = {
        val rk = Some(routingKey(peerNum))
        val ctx0 = Map("peer" -> peerNum.toString)
        def trace(msg: String) = LogEvent(Level.Trace, msg, ctx0, routingKey = rk)
        def info(msg: String) = LogEvent(Level.Info, msg, ctx0, routingKey = rk)
        def infoCtx(msg: String, extra: Map[String, String]) =
            LogEvent(Level.Info, msg, ctx0 ++ extra, routingKey = rk)
        def warn(msg: String) = LogEvent(Level.Warn, msg, ctx0, routingKey = rk)
        e match {
            case Stopped           => info("stopping")
            case BecameState(name) => info(s"becoming $name")
            case BlockBriefReceived(blockNum) =>
                infoCtx(s"new block brief ${blockNum: Int}", Map("blockNum" -> s"${blockNum: Int}"))
            case FinalizationTriggered => info("finalization locally triggered")
            case PollResultsUpdated    => trace("poll results updated")
            case WakeupIgnored(received, current, isFuture) =>
                val msg =
                    s"ignoring wakeup for block ${received: Int}, current: ${current: Int}"
                if isFuture then warn(msg) else info(msg)
            case WakeupDropped(blockNum) =>
                trace(s"unexpected wakeup for block ${blockNum: Int}, ignoring")
            case SoftConfirmationIgnored(blockNum) =>
                trace(s"ignoring soft block confirmation ${blockNum: Int}")
            case RequestAddedToMempool(requestId) =>
                info(s"request $requestId added to mempool")
            case AwaitedRequestReceived(requestId) =>
                info(s"awaited request $requestId received")
            case WaitingForRequest(received, awaiting) =>
                info(s"request $received received; still waiting for $awaiting")
            case MempoolExtracted(requestIds) =>
                trace(s"extracted ${requestIds.size} requests from mempool")
            case RequestSentToJointLedger(requestId) =>
                trace(s"sending request $requestId to joint ledger")
            case PreviousBlockConfirmation(blockNum) =>
                infoCtx(
                  s"handling confirmation for previous block ${blockNum: Int}",
                  Map("blockNum" -> s"${blockNum: Int}")
                )
            case BelatedConfirmation(confirmed, producing) =>
                info(
                  s"belated confirmation ${confirmed: Int} while producing ${producing: Int}, ignoring"
                )
            case ForcedBlockCompletion(blockNum) =>
                infoCtx(
                  s"wakeup for current block ${blockNum: Int}: force start/complete",
                  Map("blockNum" -> s"${blockNum: Int}")
                )
            case NonPositiveWakeupDelay(blockNum) =>
                infoCtx(
                  s"non-positive wakeup delay for block ${blockNum: Int}, firing immediately",
                  Map("blockNum" -> s"${blockNum: Int}")
                )
            case WakeupFiberStarted(blockNum) =>
                trace(s"wakeup fiber scheduled for block ${blockNum: Int}")
        }
    }

    def jsonlFormat(peerNum: HeadPeerNumber)(e: BlockWeaverEvent): Option[LogEvent] = {
        val ts = System.currentTimeMillis()
        val rk = Some("hydrozoa.trace")
        def htrace(json: String) = LogEvent(Level.Info, s"HTRACE|$json", routingKey = rk)
        e match {
            case BlockBriefReceived(blockNum) =>
                Some(
                  htrace(
                    s"""{"ts":$ts,"node":"$peerNum","event":"block_brief_received","block_num":${blockNum: Int}}"""
                  )
                )
            case FinalizationTriggered =>
                Some(
                  htrace(s"""{"ts":$ts,"node":"$peerNum","event":"finalization_triggered"}""")
                )
            case _ => None
        }
    }
