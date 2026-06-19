package hydrozoa.multisig.ledger.block

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{DepositDecisionWakeupTime, ForcedMajorBlockWakeupTime}

/** Typed events emitted by the polymorphic `BlockHeader.nextHeader*` builders. Pure data;
  * formatters in [[BlockHeaderEventFormat]] decide how each variant is rendered.
  *
  * The builders take a `ContraTracer[F, BlockHeaderEvent]`, so a pure caller can pass
  * `ContraTracer.nullTracer[cats.Id, BlockHeaderEvent]` and an IO caller can pass a real tracer.
  */
sealed trait BlockHeaderEvent

object BlockHeaderEvent:

    /** A minor header was computed. */
    final case class NextMinor(
        forcedMajorBlockWakeupTime: ForcedMajorBlockWakeupTime,
        mDepositDecisionWakeupTime: Option[DepositDecisionWakeupTime]
    ) extends BlockHeaderEvent

    /** A major header was computed. */
    final case class NextMajor(
        forcedMajorBlockWakeupTime: ForcedMajorBlockWakeupTime,
        mDepositDecisionWakeupTime: Option[DepositDecisionWakeupTime]
    ) extends BlockHeaderEvent
