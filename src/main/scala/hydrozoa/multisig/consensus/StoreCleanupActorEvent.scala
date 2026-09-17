package hydrozoa.multisig.consensus

import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber

/** Typed events emitted by [[StoreCleanupActor]]. Pure data; formatters in
  * [[StoreCleanupActorEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait StoreCleanupActorEvent

object StoreCleanupActorEvent:

    /** One cleanup pass deleted ack signatures a confirmation had already subsumed. */
    final case class AcksPruned(
        stackNum: StackNumber,
        lastBlockNum: BlockNumber,
        softAcks: Int,
        hardAcks: Int
    ) extends StoreCleanupActorEvent

    /** A pass found nothing to delete.
      *
      * Routine when the previous pass already caught up — but it is also what a stalled archiver
      * looks like, since an ack survives until the archive holds it. Distinguishing the two is what
      * the retention metrics are for; this only records that the pass ran and did nothing.
      */
    final case class NothingToPrune(stackNum: StackNumber) extends StoreCleanupActorEvent

    /** A pass failed. Deletion is best-effort by design: the store keeping more than it must is
      * always safe, so a failed pass is reported and the next one retries from the same floor.
      */
    final case class PruneFailed(stackNum: StackNumber, cause: Throwable)
        extends StoreCleanupActorEvent
