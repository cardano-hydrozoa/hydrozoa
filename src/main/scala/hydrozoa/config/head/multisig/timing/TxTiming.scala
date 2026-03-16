package hydrozoa.config.head.multisig.timing

import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant, quantize}
import hydrozoa.multisig.consensus.{RequestValidityEndTimeRaw, RequestValidityStartTimeRaw}
import scala.concurrent.duration.DurationInt
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.SlotConfig

import TxTiming.*
import Durations.*
import BlockTimes.*
import RequestTimes.*

/** The reason we measure time duration in real units is that slot length is different for different
  * networks.
  *
  * @param minSettlementDuration
  *   see spec
  *
  * @param inactivityMarginDuration
  *   After every major block, a consecutive sequence of minor blocks can be made before this
  *   duration elapses. After it elapses, the next block must be major.
  *
  * @param silenceDuration
  *   A fixed-time gap between concurrent txs to prevent contention, typically a small value like 5
  *   min:
  *   - between fallback N and settlement/finalization N+1
  *   - settlement tx and refund tx that tries to absorb/refund the same deposit
  *
  * @param depositSubmissionDuration
  *   The fixed amount of time reserved for submitting the deposit txs by users. It's materialized
  *   as the ttl for deposit txs, which SHOULD be exactly [[UserRequestHeader.validityEnd]] +
  *   [[depositSubmissionDurationdefines]].
  *
  * @param depositMaturityDuration
  *   The head waits for this duration after a deposit tx's validity end time to check whether a
  *   deposit utxo exists on L1. The deposit utxo should be mostly settled on L1 at this time (i.e.
  *   unlikely to be rolled back). Defines _depositAbsorptionStart_ point.
  *
  * @param depositAbsorptionDuration
  *   After a deposit utxo is mature, the head has until this duration elapses to attempt to absorb
  *   it. Defines _depositAbsorptionEnd_ point.
  */
final case class TxTiming private (
    override val minSettlementDuration: MinSettlementDuration,
    override val inactivityMarginDuration: InactivityMarginDuration,
    override val silenceDuration: SilenceDuration,
    override val depositSubmissionDuration: DepositSubmissionDuration,
    override val depositMaturityDuration: DepositMaturityDuration,
    override val depositAbsorptionDuration: DepositAbsorptionDuration,
) extends TxTiming.Section {
    override transparent inline def txTiming: TxTiming = this

    val absorptionStartOffsetDuration: AbsorptionStartOffsetDuration =
        AbsorptionStartOffsetDuration(
          depositSubmissionDuration + depositMaturityDuration
        )

    val refundStartOffsetDuration: RefundStartOffsetDuration = RefundStartOffsetDuration(
      absorptionStartOffsetDuration + depositAbsorptionDuration + silenceDuration
    )

    def initializationEndTime(blockCreationEndTime: BlockCreationEndTime): InitializationTxEndTime =
        InitializationTxEndTime(
          blockCreationEndTime + minSettlementDuration + inactivityMarginDuration
        )

    def newSettlementEndTime(competingFallbackStartTime: FallbackTxStartTime): SettlementTxEndTime =
        SettlementTxEndTime(competingFallbackStartTime - silenceDuration)

    def finalizationEndTime(
        competingFallbackStartTime: FallbackTxStartTime
    ): FinalizationTxEndTime =
        FinalizationTxEndTime(newSettlementEndTime(competingFallbackStartTime))

    /** A major/initial block's fallback tx's start time should be set to this time relative to the
      * block's start time.
      */
    def newFallbackStartTime(blockCreationEndTime: BlockCreationEndTime): FallbackTxStartTime =
        FallbackTxStartTime(
          blockCreationEndTime + minSettlementDuration + inactivityMarginDuration + silenceDuration
        )

    def nextBlockTimeout(competingFallbackStartTime: FallbackTxStartTime): NextBlockTimeout =
        NextBlockTimeout(competingFallbackStartTime - minSettlementDuration - silenceDuration)

    /** A block can stay minor if this predicate is true for its start time, relative to the
      * previous major block's fallback tx start time. Otherwise, it must be upgraded to a major
      * block so that the competing fallback start time is pushed forward for future blocks.
      */
    def blockCanStayMinor(
        blockCreationEndTime: BlockCreationEndTime,
        competingFallbackStartTime: FallbackTxStartTime
    ): Boolean = {
        nextBlockTimeout(competingFallbackStartTime).convert > blockCreationEndTime.convert
    }

    def depositSubmissionDeadline(
        requestValidityEndTime: RequestValidityEndTime
    ): DepositSubmissionDeadline =
        DepositSubmissionDeadline(requestValidityEndTime + depositSubmissionDuration)

    def depositAbsorptionStartTime(
        requestValidityEndTime: RequestValidityEndTime
    ): DepositAbsorptionStartTime =
        DepositAbsorptionStartTime(
          depositSubmissionDeadline(requestValidityEndTime) + depositMaturityDuration
        )

    def depositAbsorptionEndTime(
        requestValidityEndTime: RequestValidityEndTime
    ): DepositAbsorptionEndTime =
        DepositAbsorptionEndTime(
          depositAbsorptionStartTime(requestValidityEndTime) + depositAbsorptionDuration
        )

    def refundValidityStart(requestValidityEndTime: RequestValidityEndTime): RefundStartTime =
        RefundStartTime(depositAbsorptionEndTime(requestValidityEndTime) + silenceDuration)
}

/** TODO: Update/fix comment
  *
  * Timing is hard. The precision we have to use is going to be dependent on the slot config.
  *
  * For example, when we're parsing a PostDated refund tx, we need to extract a start time. That
  * start time right now is represented as an Instant, and without additional mitigations, we'll get
  * a parse failure because the expected start time is generated in the test suite from
  * IO.realTimeInstant in nanosecond precision.
  *
  * But when we convert to a slot, we're necessarily doing to things: (1) truncation due to integer
  * division, (2) adopting a precision dictated by the SlotConfig.slotLength field
  *
  * So using the current slot length of 1000 that appears for Mainnet, Preview, and Preprod we can
  * use millisecond precision, but that goes away if that changes
  *
  * We can use milliseconds for now, but the right way to handle this would probably be some sort of
  * opaque time object that will only spit out values of Instant, Slot, FiniteDuration, etc that are
  * exactly at the precision of some given slot config
  *
  * For now, we just have to be careful to ensure that we're using millisecond precision everywhere
  */
object TxTiming {
    def checkRequestValidityInterval(
        blockCreationStartTime: BlockCreationStartTime,
        requestValidityStartTime: RequestValidityStartTime,
        requestValidityEndTime: RequestValidityEndTime
    ): Boolean =
        //        requestValidityStartTime.convert <= blockCreationStartTime.convert &&
        blockCreationStartTime.convert < requestValidityEndTime.convert

    /** Maturity. The deposit is immature if its absorption start time is later than the block
      * brief’s creation end time.
      */
    def depositIsImmature(
        depositAbsorptionStartTime: DepositAbsorptionStartTime,
        blockCreationEndTime: BlockCreationEndTime
    ): Boolean =
        blockCreationEndTime.convert < depositAbsorptionStartTime.convert

    /** The deposit is expired if its absorption end time is earlier than the notional settlement
      * effect's end time.
      */
    def depositIsExpired(
        settlementTxEndTime: SettlementTxEndTime,
        depositAbsorptionEndTime: DepositAbsorptionEndTime
    ): Boolean =
        depositAbsorptionEndTime.convert < settlementTxEndTime.convert

    def default(slotConfig: SlotConfig): TxTiming = TxTiming(
      MinSettlementDuration(12.hours.quantize(slotConfig)),
      InactivityMarginDuration(24.hours.quantize(slotConfig)),
      SilenceDuration(5.minutes.quantize(slotConfig)),
      DepositSubmissionDuration(5.minutes.quantize(slotConfig)),
      DepositMaturityDuration(1.hour.quantize(slotConfig)),
      DepositAbsorptionDuration(48.hours.quantize(slotConfig)),
    )

    // TODO: move to integration
    def yaci(slotConfig: SlotConfig) = TxTiming(
      MinSettlementDuration(30.seconds.quantize(slotConfig)),
      InactivityMarginDuration(60.seconds.quantize(slotConfig)),
      SilenceDuration(1.minute.quantize(slotConfig)),
      DepositSubmissionDuration(1.second.quantize(slotConfig)),
      DepositMaturityDuration(1.second.quantize(slotConfig)),
      DepositAbsorptionDuration(2.minutes.quantize(slotConfig)),
    )

    def demo(slotConfig: SlotConfig) = TxTiming(
      MinSettlementDuration(1.hour.quantize(slotConfig)),
      InactivityMarginDuration(2.hours.quantize(slotConfig)),
      SilenceDuration(2.minutes.quantize(slotConfig)),
      DepositSubmissionDuration(2.minutes.quantize(slotConfig)),
      DepositMaturityDuration(3.minutes.quantize(slotConfig)),
      DepositAbsorptionDuration(4.hours.quantize(slotConfig)),
    )

    // TODO: move to integration
    def testnet(slotConfig: SlotConfig) = TxTiming(
      MinSettlementDuration(1.hour.quantize(slotConfig)),
      InactivityMarginDuration(2.hours.quantize(slotConfig)),
      SilenceDuration(2.minutes.quantize(slotConfig)),
      DepositSubmissionDuration(30.seconds.quantize(slotConfig)),
      DepositMaturityDuration(30.seconds.quantize(slotConfig)),
      DepositAbsorptionDuration(4.hours.quantize(slotConfig)),
    )

    trait Section {
        def txTiming: TxTiming

        def minSettlementDuration: MinSettlementDuration
        def inactivityMarginDuration: InactivityMarginDuration
        def silenceDuration: SilenceDuration
        def depositSubmissionDuration: DepositSubmissionDuration
        def depositMaturityDuration: DepositMaturityDuration
        def depositAbsorptionDuration: DepositAbsorptionDuration
    }

    object Durations {
        opaque type MinSettlementDuration = QuantizedFiniteDuration
        def MinSettlementDuration(x: QuantizedFiniteDuration): MinSettlementDuration = x
        given Conversion[MinSettlementDuration, QuantizedFiniteDuration] = identity

        opaque type InactivityMarginDuration = QuantizedFiniteDuration
        def InactivityMarginDuration(
            x: QuantizedFiniteDuration
        ): InactivityMarginDuration = x
        given Conversion[InactivityMarginDuration, QuantizedFiniteDuration] = identity

        opaque type SilenceDuration = QuantizedFiniteDuration
        def SilenceDuration(x: QuantizedFiniteDuration): SilenceDuration = x
        given Conversion[SilenceDuration, QuantizedFiniteDuration] = identity

        opaque type DepositSubmissionDuration = QuantizedFiniteDuration
        def DepositSubmissionDuration(
            x: QuantizedFiniteDuration
        ): DepositSubmissionDuration = x
        given Conversion[DepositSubmissionDuration, QuantizedFiniteDuration] = identity

        opaque type DepositMaturityDuration = QuantizedFiniteDuration
        def DepositMaturityDuration(x: QuantizedFiniteDuration): DepositMaturityDuration = x
        given Conversion[DepositMaturityDuration, QuantizedFiniteDuration] = identity

        opaque type DepositAbsorptionDuration = QuantizedFiniteDuration
        def DepositAbsorptionDuration(x: QuantizedFiniteDuration): DepositAbsorptionDuration = x
        given Conversion[DepositAbsorptionDuration, QuantizedFiniteDuration] = identity

        opaque type AbsorptionStartOffsetDuration = QuantizedFiniteDuration
        def AbsorptionStartOffsetDuration(
            x: QuantizedFiniteDuration
        ): AbsorptionStartOffsetDuration = x
        given Conversion[AbsorptionStartOffsetDuration, QuantizedFiniteDuration] = identity

        opaque type RefundStartOffsetDuration = QuantizedFiniteDuration
        def RefundStartOffsetDuration(x: QuantizedFiniteDuration): RefundStartOffsetDuration = x
        given Conversion[RefundStartOffsetDuration, QuantizedFiniteDuration] = identity
    }

    object BlockTimes {
        opaque type BlockCreationStartTime = QuantizedInstant
        def BlockCreationStartTime(x: QuantizedInstant): BlockCreationStartTime = x
        given Conversion[BlockCreationStartTime, QuantizedInstant] = identity

        opaque type BlockCreationEndTime = QuantizedInstant
        def BlockCreationEndTime(x: QuantizedInstant): BlockCreationEndTime = x
        given Conversion[BlockCreationEndTime, QuantizedInstant] = identity

        opaque type InitializationTxEndTime = QuantizedInstant
        private[timing] def InitializationTxEndTime(x: QuantizedInstant): InitializationTxEndTime =
            x
        given Conversion[InitializationTxEndTime, QuantizedInstant] = identity

        opaque type SettlementTxEndTime = QuantizedInstant
        private[timing] def SettlementTxEndTime(x: QuantizedInstant): SettlementTxEndTime = x
        given Conversion[SettlementTxEndTime, QuantizedInstant] = identity

        opaque type FinalizationTxEndTime = QuantizedInstant
        private[timing] def FinalizationTxEndTime(x: QuantizedInstant): FinalizationTxEndTime = x
        given Conversion[FinalizationTxEndTime, QuantizedInstant] = identity

        opaque type FallbackTxStartTime = QuantizedInstant
        private[timing] def FallbackTxStartTime(x: QuantizedInstant): FallbackTxStartTime = x
        given Conversion[FallbackTxStartTime, QuantizedInstant] = identity

        opaque type NextBlockTimeout = QuantizedInstant
        private[timing] def NextBlockTimeout(x: QuantizedInstant): NextBlockTimeout = x
        given Conversion[NextBlockTimeout, QuantizedInstant] = identity
    }

    object RequestTimes {
        opaque type RequestValidityStartTime = QuantizedInstant
        def RequestValidityStartTime(
            slotConfig: SlotConfig,
            x: RequestValidityStartTimeRaw
        ): RequestValidityStartTime =
            QuantizedInstant.ofEpochSeconds(slotConfig, x.toLong)
        given Conversion[RequestValidityStartTime, QuantizedInstant] = identity

        opaque type RequestValidityEndTime = QuantizedInstant
        def RequestValidityEndTime(
            slotConfig: SlotConfig,
            x: RequestValidityEndTimeRaw
        ): RequestValidityEndTime =
            QuantizedInstant.ofEpochSeconds(slotConfig, x.toLong)
        // Used in tests
        def unsafeRequestValidityEndTime(x: QuantizedInstant): RequestValidityEndTime = x
        given Conversion[RequestValidityEndTime, QuantizedInstant] = identity

        opaque type DepositSubmissionDeadline = QuantizedInstant
        private[timing] def DepositSubmissionDeadline(
            x: QuantizedInstant
        ): DepositSubmissionDeadline =
            x
        given Conversion[DepositSubmissionDeadline, QuantizedInstant] = identity

        opaque type DepositAbsorptionStartTime = QuantizedInstant
        private[timing] def DepositAbsorptionStartTime(
            x: QuantizedInstant
        ): DepositAbsorptionStartTime = x
        given Conversion[DepositAbsorptionStartTime, QuantizedInstant] = identity

        opaque type DepositAbsorptionEndTime = QuantizedInstant
        private[timing] def DepositAbsorptionEndTime(
            x: QuantizedInstant
        ): DepositAbsorptionEndTime = x
        given Conversion[DepositAbsorptionEndTime, QuantizedInstant] = identity

        opaque type RefundStartTime = QuantizedInstant
        private[timing] def RefundStartTime(x: QuantizedInstant): RefundStartTime = x
        given Conversion[RefundStartTime, QuantizedInstant] = identity
    }
}

given Ordering[DepositAbsorptionStartTime] with {
    override def compare(
        self: DepositAbsorptionStartTime,
        other: DepositAbsorptionStartTime
    ): Int = {
        self.convert.compare(other.convert)
    }
}
