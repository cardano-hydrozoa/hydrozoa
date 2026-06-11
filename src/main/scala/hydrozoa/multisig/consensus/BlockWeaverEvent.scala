package hydrozoa.multisig.consensus

import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestId

/** Typed events emitted by [[BlockWeaver]]. Pure data; formatters in [[BlockWeaverEventFormat]]
  * decide how each variant is rendered to a particular sink.
  */
sealed trait BlockWeaverEvent

object BlockWeaverEvent:
    case object Stopped extends BlockWeaverEvent

    final case class BecameState(name: String) extends BlockWeaverEvent

    final case class BlockBriefReceived(blockNum: BlockNumber) extends BlockWeaverEvent

    case object FinalizationTriggered extends BlockWeaverEvent

    case object PollResultsUpdated extends BlockWeaverEvent

    /** Wakeup for a block that does not match the current leading block (in states with a leading
      * block number). `isFuture` is true when the wakeup is for a later block (warn-level).
      */
    final case class WakeupIgnored(received: BlockNumber, current: BlockNumber, isFuture: Boolean)
        extends BlockWeaverEvent

    /** Wakeup dropped in a state that has no current leading block number (e.g. AwaitingBlockBrief,
      * AwaitingRequest).
      */
    final case class WakeupDropped(blockNum: BlockNumber) extends BlockWeaverEvent

    final case class SoftConfirmationIgnored(blockNum: BlockNumber) extends BlockWeaverEvent

    final case class RequestAddedToMempool(requestId: RequestId) extends BlockWeaverEvent

    final case class AwaitedRequestReceived(requestId: RequestId) extends BlockWeaverEvent

    final case class WaitingForRequest(received: RequestId, awaiting: RequestId)
        extends BlockWeaverEvent

    final case class MempoolExtracted(requestIds: List[RequestId]) extends BlockWeaverEvent

    final case class RequestSentToJointLedger(requestId: RequestId) extends BlockWeaverEvent

    final case class PreviousBlockConfirmation(blockNum: BlockNumber) extends BlockWeaverEvent

    final case class BelatedConfirmation(confirmed: BlockNumber, producing: BlockNumber)
        extends BlockWeaverEvent

    final case class ForcedBlockCompletion(blockNum: BlockNumber) extends BlockWeaverEvent

    final case class NonPositiveWakeupDelay(blockNum: BlockNumber) extends BlockWeaverEvent

    final case class WakeupFiberStarted(blockNum: BlockNumber) extends BlockWeaverEvent
