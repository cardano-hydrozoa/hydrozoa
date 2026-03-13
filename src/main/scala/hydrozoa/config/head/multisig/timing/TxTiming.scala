package hydrozoa.config.head.multisig.timing

import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant, quantize}
import scala.concurrent.duration.DurationInt
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.SlotConfig

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
final case class TxTiming(
    override val minSettlementDuration: QuantizedFiniteDuration,
    override val inactivityMarginDuration: QuantizedFiniteDuration,
    override val silenceDuration: QuantizedFiniteDuration,
    override val depositSubmissionDuration: QuantizedFiniteDuration,
    override val depositMaturityDuration: QuantizedFiniteDuration,
    override val depositAbsorptionDuration: QuantizedFiniteDuration,
) extends TxTiming.Section {
    override transparent inline def txTiming: TxTiming = this

    val refundValidityStartOffset: QuantizedFiniteDuration =
        depositSubmissionDuration + depositMaturityDuration + depositAbsorptionDuration + silenceDuration

    /** A block can stay minor if this predicate is true for its start time, relative to the
      * previous major block's fallback tx start time. Otherwise, it must be upgraded to a major
      * block so that the competing fallback start time is pushed forward for future blocks.
      */
    def blockCanStayMinor(
        blockCreationEndTime: QuantizedInstant,
        competingFallbackEndTime: QuantizedInstant
    ): Boolean =
        competingFallbackEndTime - blockCreationEndTime > minSettlementDuration + silenceDuration

    def initializationEndTime(initialBlockCreationEndTime: QuantizedInstant): QuantizedInstant =
        initialBlockCreationEndTime + minSettlementDuration + inactivityMarginDuration

    def newSettlementEndTime(competingFallbackStartTime: QuantizedInstant): QuantizedInstant =
        competingFallbackStartTime - silenceDuration

    def finalizationEndTime(competingFallbackStartTime: QuantizedInstant): QuantizedInstant =
        newSettlementEndTime(competingFallbackStartTime)

    /** A major/initial block's fallback tx's start time should be set to this time relative to the
      * block's start time.
      */
    def newFallbackStartTime(blockCreationEndTime: QuantizedInstant): QuantizedInstant =
        blockCreationEndTime + minSettlementDuration + inactivityMarginDuration + silenceDuration

    def depositSubmissionEndTime(requestValidityEndTime: QuantizedInstant) =
        requestValidityEndTime + depositSubmissionDuration

    def depositAbsorptionStartTime(requestValidityEndTime: QuantizedInstant): QuantizedInstant =
        depositSubmissionEndTime(requestValidityEndTime) + depositMaturityDuration

    def depositAbsorptionEndTime(requestValidityEndTime: QuantizedInstant): QuantizedInstant =
        depositAbsorptionStartTime(requestValidityEndTime) + depositAbsorptionDuration

    def refundValidityStart(requestValidityEndTime: QuantizedInstant): QuantizedInstant =
        depositAbsorptionEndTime(requestValidityEndTime) + silenceDuration

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
    def default(slotConfig: SlotConfig): TxTiming = TxTiming(
      minSettlementDuration = 12.hours.quantize(slotConfig),
      inactivityMarginDuration = 24.hours.quantize(slotConfig),
      silenceDuration = 5.minutes.quantize(slotConfig),
      depositSubmissionDuration = 5.minutes.quantize(slotConfig),
      depositMaturityDuration = 1.hours.quantize(slotConfig),
      depositAbsorptionDuration = 48.hours.quantize(slotConfig),
    )

    // TODO: move to integration
    def yaci(slotConfig: SlotConfig) = TxTiming(
      minSettlementDuration = 60.seconds.quantize(slotConfig),
      inactivityMarginDuration = 20.seconds.quantize(slotConfig),
      silenceDuration = 1.minute.quantize(slotConfig),
      depositSubmissionDuration = 5.minutes.quantize(slotConfig),
      depositMaturityDuration = 1.second.quantize(slotConfig),
      depositAbsorptionDuration = 1.hours.quantize(slotConfig),
    )

    def demo(slotConfig: SlotConfig) = TxTiming(
      minSettlementDuration = 1.hour.quantize(slotConfig),
      inactivityMarginDuration = 1.minute.quantize(slotConfig),
      silenceDuration = 5.minute.quantize(slotConfig),
      depositSubmissionDuration = 5.minutes.quantize(slotConfig),
      depositMaturityDuration = 5.minute.quantize(slotConfig),
      depositAbsorptionDuration = 2.hours.quantize(slotConfig),
    )

    // TODO: move to integration
    def testnet(slotConfig: SlotConfig) = TxTiming(
      minSettlementDuration = 1.hour.quantize(slotConfig),
      inactivityMarginDuration = 20.seconds.quantize(slotConfig),
      silenceDuration = 5.minute.quantize(slotConfig),
      depositSubmissionDuration = 5.minutes.quantize(slotConfig),
      depositMaturityDuration = 10.second.quantize(slotConfig),
      depositAbsorptionDuration = 2.hours.quantize(slotConfig),
    )

    trait Section {
        def txTiming: TxTiming

        def minSettlementDuration: QuantizedFiniteDuration
        def inactivityMarginDuration: QuantizedFiniteDuration
        def silenceDuration: QuantizedFiniteDuration
        def depositSubmissionDuration: QuantizedFiniteDuration
        def depositMaturityDuration: QuantizedFiniteDuration
        def depositAbsorptionDuration: QuantizedFiniteDuration
    }
}
