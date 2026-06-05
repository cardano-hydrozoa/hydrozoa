package hydrozoa.multisig.ledger.joint

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedgerEvent.*

/** Renderers from [[JointLedgerEvent]] to [[LogEvent]] for various back-end sinks. Lives separately
  * from the event ADT so the type itself stays pure data, and callers (Main / harness) compose
  * `Tracer.sink.contramap(humanFormat(peer)) |+| Tracer.sink.traceMaybe(jsonlFormat(nodeId))`.
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
        val rk = Some(routingKey(peerNum))
        val ctx0 = baseCtx(peerNum)
        e match {
            case BriefProduced(b) =>
                val v = b.header.blockVersion
                LogEvent(
                  Level.Info,
                  s"brief produced: block=${b.blockNum: Int} type=${briefTypeName(b)} v${v.major: Int}.${v.minor: Int} events=${b.body.events.size}",
                  ctx0 + ("blockNum" -> s"${b.blockNum: Int}"),
                  routingKey = rk
                )
            case L2CommandFailed(err) =>
                LogEvent(Level.Error, s"L2 command failed: $err", ctx0, routingKey = rk)
            case L2ProxyCommandFailed(err) =>
                LogEvent(Level.Error, s"L2 proxy command failed: $err", ctx0, routingKey = rk)
            case InvalidStateExpectedProducing =>
                LogEvent(
                  Level.Error,
                  "Expected a `Producing` State, but got `Done`. This indicates that a request was issued to the JointLedger that is only valid when the hydrozoa node is producing a block.",
                  ctx0,
                  routingKey = rk
                )
            case DepositRegistrationStarted(rid) =>
                LogEvent(
                  Level.Info,
                  s"register new deposit, request id: $rid",
                  ctx0 + ("requestId" -> rid.toString),
                  routingKey = rk
                )
            case DepositRegistrationCompleted(rid, bn) =>
                LogEvent(
                  Level.Debug,
                  s"Request processed ($rid)",
                  ctx0 ++ Map("requestId" -> rid.toString, "blockNum" -> s"${bn: Int}"),
                  routingKey = rk
                )
            case TransactionApplicationStarted(rid) =>
                LogEvent(
                  Level.Info,
                  s"applying transaction, request id: $rid",
                  ctx0 + ("requestId" -> rid.toString),
                  routingKey = rk
                )
            case TransactionApplicationCompleted(rid, bn) =>
                LogEvent(
                  Level.Debug,
                  s"transaction applied ($rid)",
                  ctx0 ++ Map("requestId" -> rid.toString, "blockNum" -> s"${bn: Int}"),
                  routingKey = rk
                )
            case RequestRejected(rid, bn, reason) =>
                LogEvent(
                  Level.Warn,
                  s"Request rejected ($rid): $reason",
                  ctx0 ++ Map("requestId" -> rid.toString, "blockNum" -> s"${bn: Int}"),
                  routingKey = rk
                )
            case BlockStarted(bn, startTime) =>
                LogEvent(
                  Level.Info,
                  s"start block: $bn (blockCreationStartTime=$startTime)",
                  ctx0 + ("blockNum" -> s"${bn: Int}"),
                  routingKey = rk
                )
            case BlockCompleting(bn, endTime, fallbackTime, split) =>
                LogEvent(
                  Level.Info,
                  s"completing block $bn (blockCreationEndTime=$endTime, competingFallbackTxTime=$fallbackTime, split=$split)",
                  ctx0 + ("blockNum" -> s"${bn: Int}"),
                  routingKey = rk
                )
            case BlockBriefBuilding(prev, start, fallback, events, absorbed, refunded) =>
                LogEvent(
                  Level.Trace,
                  s"mkBlockBrief: previousHeader=$prev\n" +
                      s"mkBlockBrief: blockStartTime=$start\n" +
                      s"mkBlockBrief: competingFallbackValidityStart=$fallback\n" +
                      s"mkBlockBrief: events=$events\n" +
                      s"mkBlockBrief: decisions.absorbed=$absorbed\n" +
                      s"mkBlockBrief: decisions.refunded=$refunded",
                  ctx0,
                  routingKey = rk
                )
            case BlockBriefBuilt(brief) =>
                LogEvent(
                  Level.Trace,
                  "mkBlockBriefIntermediate result:\n" +
                      s"  Block type: ${briefTypeName(brief)}\n" +
                      s"  Block number: ${brief.blockNum: Int}\n" +
                      s"  Block brief: $brief",
                  ctx0 + ("blockNum" -> s"${brief.blockNum: Int}"),
                  routingKey = rk
                )
            case HeaderLog(level, msg, ctx) =>
                LogEvent(level, msg, ctx0 ++ ctx, routingKey = rk)
        }
    }

    /** Routes only protocol-trace-worthy events to the `hydrozoa.trace` JSONL logger; returns
      * `None` for everything else (passed to `traceMaybe`).
      */
    def jsonlFormat(nodeId: String)(e: JointLedgerEvent): Option[LogEvent] = {
        val ts = System.currentTimeMillis()
        val rk = Some("hydrozoa.trace")
        e match {
            case BriefProduced(b) =>
                val v = b.header.blockVersion
                Some(
                  LogEvent(
                    Level.Info,
                    s"""HTRACE|{"ts":$ts,"node":"$nodeId","event":"brief_produced","block_num":${b.blockNum: Int},"block_type":"${briefTypeName(
                          b
                        )}","v_major":${v.major: Int},"v_minor":${v.minor: Int},"event_count":${b.body.events.size}}""",
                    routingKey = rk
                  )
                )
            case DepositRegistrationCompleted(rid, bn) =>
                Some(eventProcessedLine(ts, nodeId, rid, bn, valid = true, rk))
            case TransactionApplicationCompleted(rid, bn) =>
                Some(eventProcessedLine(ts, nodeId, rid, bn, valid = true, rk))
            case RequestRejected(rid, bn, _) =>
                Some(eventProcessedLine(ts, nodeId, rid, bn, valid = false, rk))
            case _ => None
        }
    }

    private def eventProcessedLine(
        ts: Long,
        nodeId: String,
        rid: RequestId,
        bn: BlockNumberInt,
        valid: Boolean,
        rk: Option[String]
    ): LogEvent = {
        val eventId = s"${rid.peerNum}:${rid.requestNum}"
        LogEvent(
          Level.Info,
          s"""HTRACE|{"ts":$ts,"node":"$nodeId","event":"event_processed","event_id":"$eventId","block_num":${bn: Int},"valid":$valid}""",
          routingKey = rk
        )
    }

    // Alias for readability — keep BlockNumber's opaque-Int erasure local to this file.
    private type BlockNumberInt = hydrozoa.multisig.ledger.block.BlockNumber
