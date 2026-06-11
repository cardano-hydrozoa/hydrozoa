package hydrozoa.multisig.ledger.joint

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.joint.JointLedgerEvent.*

/** Renderers from [[JointLedgerEvent]] to [[LogEvent]]. Lives separately from the event ADT so the
  * type itself stays pure data.
  */
object JointLedgerEventFormat:

    private def briefTypeName(b: BlockBrief.Intermediate): String = b match {
        case _: BlockBrief.Minor => "minor"
        case _: BlockBrief.Major => "major"
    }

    private def routingKey(peerNum: Int): String = s"JointLedger.$peerNum"

    private def baseCtx(peerNum: Int): Map[String, String] =
        Map("peer" -> peerNum.toString)

    /** Routes every event to SLF4J text. */
    def humanFormat(peerNum: Int)(e: JointLedgerEvent): LogEvent = {
        val ev = LogEvent.From(baseCtx(peerNum), routingKey(peerNum))
        import ev.*
        e match {
            case BriefProduced(b) =>
                val v = b.header.blockVersion
                info(
                  s"brief produced: block=${b.blockNum: Int} type=${briefTypeName(b)} v${v.major: Int}.${v.minor: Int} events=${b.body.events.size}",
                  "blockNum" -> s"${b.blockNum: Int}"
                )
            case L2CommandFailed(err) =>
                error(s"L2 command failed: $err")
            case L2ProxyCommandFailed(err) =>
                error(s"L2 proxy command failed: $err")
            case InvalidStateExpectedProducing =>
                error(
                  "Expected a `Producing` State, but got `Done`. This indicates that a request was issued to the JointLedger that is only valid when the hydrozoa node is producing a block."
                )
            case DepositRegistrationStarted(rid) =>
                info(s"register new deposit, request id: $rid", "requestId" -> rid.toString)
            case DepositRegistrationCompleted(rid, bn) =>
                debug(
                  s"Request processed ($rid)",
                  "requestId" -> rid.toString,
                  "blockNum" -> s"${bn: Int}"
                )
            case TransactionApplicationStarted(rid) =>
                info(s"applying transaction, request id: $rid", "requestId" -> rid.toString)
            case TransactionApplicationCompleted(rid, bn) =>
                debug(
                  s"transaction applied ($rid)",
                  "requestId" -> rid.toString,
                  "blockNum" -> s"${bn: Int}"
                )
            case RequestRejected(rid, bn, reason) =>
                warn(
                  s"Request rejected ($rid): $reason",
                  "requestId" -> rid.toString,
                  "blockNum" -> s"${bn: Int}"
                )
            case BlockStarted(bn, startTime) =>
                info(
                  s"start block: $bn (blockCreationStartTime=$startTime)",
                  "blockNum" -> s"${bn: Int}"
                )
            case BlockCompleting(bn, endTime, fallbackTime, split) =>
                info(
                  s"completing block $bn (blockCreationEndTime=$endTime, competingFallbackTxTime=$fallbackTime, split=$split)",
                  "blockNum" -> s"${bn: Int}"
                )
            case BlockBriefBuilding(prev, start, fallback, events, absorbed, refunded) =>
                trace(
                  s"mkBlockBrief: previousHeader=$prev\n" +
                      s"mkBlockBrief: blockStartTime=$start\n" +
                      s"mkBlockBrief: competingFallbackValidityStart=$fallback\n" +
                      s"mkBlockBrief: events=$events\n" +
                      s"mkBlockBrief: decisions.absorbed=$absorbed\n" +
                      s"mkBlockBrief: decisions.refunded=$refunded"
                )
            case BlockBriefBuilt(brief) =>
                trace(
                  "mkBlockBriefIntermediate result:\n" +
                      s"  Block type: ${briefTypeName(brief)}\n" +
                      s"  Block number: ${brief.blockNum: Int}\n" +
                      s"  Block brief: $brief",
                  "blockNum" -> s"${brief.blockNum: Int}"
                )
            case HeaderLog(level, msg, ctx) =>
                LogEvent(level, msg, ev.ctx ++ ctx, routingKey = ev.routingKey)
        }
    }
