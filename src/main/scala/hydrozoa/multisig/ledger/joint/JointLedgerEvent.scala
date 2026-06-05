package hydrozoa.multisig.ledger.joint

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime, FallbackTxStartTime}
import hydrozoa.lib.logging.Level
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.l2.L2LedgerError

/** Typed events emitted by [[JointLedger]]. Pure data: every former `Tracer.xxx` / `ProtocolTracer`
  * call in JL maps to one variant here. Formatters (see [[JointLedgerEventFormat]]) decide how each
  * variant is rendered to a particular sink (SLF4J text, JSONL protocol trace, in-memory capture).
  */
sealed trait JointLedgerEvent

object JointLedgerEvent:

    // ===== Brief production =====

    /** A new soft-block brief was produced/reproduced by `JointLedger.handleBlock`. */
    final case class BriefProduced(brief: BlockBrief.Intermediate) extends JointLedgerEvent

    // ===== L2 ledger failures =====

    final case class L2CommandFailed(err: L2LedgerError) extends JointLedgerEvent
    final case class L2ProxyCommandFailed(err: Throwable) extends JointLedgerEvent

    // ===== Internal-state errors =====

    /** A request demanded a `Producing` state but the JL was `Done`. */
    case object InvalidStateExpectedProducing extends JointLedgerEvent

    // ===== Request lifecycle =====

    final case class DepositRegistrationStarted(requestId: RequestId) extends JointLedgerEvent
    final case class DepositRegistrationCompleted(requestId: RequestId, blockNum: BlockNumber)
        extends JointLedgerEvent
    final case class TransactionApplicationStarted(requestId: RequestId) extends JointLedgerEvent
    final case class TransactionApplicationCompleted(requestId: RequestId, blockNum: BlockNumber)
        extends JointLedgerEvent
    final case class RequestRejected(
        requestId: RequestId,
        blockNum: BlockNumber,
        reason: String
    ) extends JointLedgerEvent

    // ===== Block lifecycle =====

    final case class BlockStarted(blockNum: BlockNumber, startTime: BlockCreationStartTime)
        extends JointLedgerEvent
    final case class BlockCompleting(
        blockNum: BlockNumber,
        endTime: BlockCreationEndTime,
        competingFallbackTxTime: FallbackTxStartTime,
        splitDescription: String
    ) extends JointLedgerEvent

    /** Verbose debug dump of the inputs to brief construction. */
    final case class BlockBriefBuilding(
        previousHeader: BlockHeader,
        blockCreationStartTime: BlockCreationStartTime,
        competingFallbackTxTime: FallbackTxStartTime,
        events: List[(RequestId, ValidityFlag)],
        decisionsAbsorbed: List[RequestId],
        decisionsRefunded: List[RequestId]
    ) extends JointLedgerEvent

    final case class BlockBriefBuilt(brief: BlockBrief.Intermediate) extends JointLedgerEvent

    // ===== Pure-function log pass-through =====

    /** Wraps a [[hydrozoa.lib.logging.LogEvent]] emitted by a pure function returning `Traced[A]`
      * (e.g. `BlockHeader.nextHeader*`), so it can flow through JL's typed tracer instead of an
      * ambient `IOLocal[Tracer]`.
      */
    final case class HeaderLog(level: Level, msg: String, ctx: Map[String, String])
        extends JointLedgerEvent
