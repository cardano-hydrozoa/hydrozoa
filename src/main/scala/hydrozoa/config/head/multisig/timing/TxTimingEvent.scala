package hydrozoa.config.head.multisig.timing

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, FallbackTxStartTime, ForcedMajorBlockWakeupTime}

/** Typed events emitted by the polymorphic `TxTiming.blockCanStayMinor` predicate. Pure data;
  * formatters in [[TxTimingEventFormat]] decide how each variant is rendered.
  */
sealed trait TxTimingEvent

object TxTimingEvent:

    /** A "can this block stay minor?" decision was evaluated for one block. */
    final case class CanStayMinor(
        competingFallbackStartTime: FallbackTxStartTime,
        forcedMajorBlockWakeupTime: ForcedMajorBlockWakeupTime,
        blockCreationEndTime: BlockCreationEndTime
    ) extends TxTimingEvent
