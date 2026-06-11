package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.BlockWeaverEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[BlockWeaverEvent]] to [[LogEvent]] for various back-end sinks. */
object BlockWeaverEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"BlockWeaver.$peerNum"

    def humanFormat(peerNum: HeadPeerNumber)(e: BlockWeaverEvent): LogEvent = {
        val ev = LogEvent.From(Map("peer" -> peerNum.toString), routingKey(peerNum))
        import ev.*
        e match {
            case Stopped           => info("stopping")
            case BecameState(name) => info(s"becoming $name")
            case BlockBriefReceived(blockNum) =>
                info(s"new block brief ${blockNum: Int}", "blockNum" -> s"${blockNum: Int}")
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
                info(
                  s"handling confirmation for previous block ${blockNum: Int}",
                  "blockNum" -> s"${blockNum: Int}"
                )
            case BelatedConfirmation(confirmed, producing) =>
                info(
                  s"belated confirmation ${confirmed: Int} while producing ${producing: Int}, ignoring"
                )
            case ForcedBlockCompletion(blockNum) =>
                info(
                  s"wakeup for current block ${blockNum: Int}: force start/complete",
                  "blockNum" -> s"${blockNum: Int}"
                )
            case NonPositiveWakeupDelay(blockNum) =>
                info(
                  s"non-positive wakeup delay for block ${blockNum: Int}, firing immediately",
                  "blockNum" -> s"${blockNum: Int}"
                )
            case WakeupFiberStarted(blockNum) =>
                trace(s"wakeup fiber scheduled for block ${blockNum: Int}")
        }
    }

    def jsonlFormat(peerNum: HeadPeerNumber)(e: BlockWeaverEvent): Option[LogEvent] = {
        val ts = System.currentTimeMillis()
        val ev = LogEvent.From(Map.empty, "hydrozoa.trace")
        import ev.*
        def htrace(json: String) = info(s"HTRACE|$json")
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
