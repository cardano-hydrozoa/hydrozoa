package hydrozoa.config.head.multisig.timing

import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant, quantize}
import scala.concurrent.duration.DurationInt
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.SlotConfig

// TODO: Update/fix comment
/** TODO: This should be derived from Hydrozoa parameters.
  *
  * TODO: move around?
  *
  * Peter and I determined that the settlement tx duration should be:
  *   - Long enough that the settlement tx is still unexpired whenever we need to resubmit it due to
  *     rollbacks.
  *   - Short enough that: We don't need to push forward the fallback start too frequently with
  *     empty major blocks.
  *   - Whenever we have too many deposits and some get deferred for absorption in future blocks, we
  *     aren't forced to decide to never-absorb them because they've been deferred for too long.
  *
  * Therefore, I think a reasonable settlement tx duration should be approximately the Cardano
  * security parameter (~12h on average), and the deposit absorption window should be longer (~24h).
  *
  * Other parameter values:
  *   - Deposit maturity duration ~= 30min to 1h,
  *   - Deposit expiry margin ~= 5 min
  *   - (Fallback N start - Settlement N TTL) ~= 24h
  *
  * The reason we measure time duration in real units is that slot length is different for different
  * networks.
  *
  * @param minSettlementDuration
  *   Minimal duration of a settlement (finalization) tx's validity range.
  * @param inactivityMarginDuration
  *   After every major block, a consecutive sequence of minor blocks can be made before this
  *   duration elapses. After it elapses, the next block must be major.
  * @param silenceDuration
  *   A fixed-time gap between concurrent txs (i.e. fallback N and settlement/finalization N+1) to
  *   prevent contention, typically a small value like 5 min.
  * @param depositMaturityDuration
  *   The head waits for this duration after a deposit tx's validity end time to check whether a
  *   deposit utxo exists on L1. The deposit utxo should be mostly settled on L1 at this time (i.e.
  *   unlikely to be rolled back).
  * @param depositAbsorptionDuration
  *   After a deposit utxo is mature, the head has until this duration elapses to attempt to absorb
  *   it. In other words, the deposit utxo can be absorbed by a settlement tx only if the settlement
  *   tx's validity end time is earlier than the maturity time plus absorption duration.
  */
final case class TxTiming(
    override val minSettlementDuration: QuantizedFiniteDuration,
    override val inactivityMarginDuration: QuantizedFiniteDuration,
    override val silenceDuration: QuantizedFiniteDuration,
    override val depositMaturityDuration: QuantizedFiniteDuration,
    override val depositAbsorptionDuration: QuantizedFiniteDuration,
) extends TxTiming.Section {
    override transparent inline def txTiming: TxTiming = this

    /** A block can stay minor if this predicate is true for its start time, relative to the
      * previous major block's fallback tx start time. Otherwise, it must be upgraded to a major
      * block so that the competing fallback start time is pushed forward for future blocks.
      */
    def blockCanStayMinor(
        blockStartTime: QuantizedInstant,
        competingFallbackEndTime: QuantizedInstant
    ): Boolean =
        competingFallbackEndTime - blockStartTime > minSettlementDuration + silenceDuration

    /** A major/initial block's fallback tx's start time should be set to this time relative to the
      * block's start time.
      */
    def newFallbackStartTime(blockStartTime: QuantizedInstant): QuantizedInstant =
        blockStartTime + minSettlementDuration + inactivityMarginDuration + silenceDuration
}

// TODO: Update/fix comment
/** Timing is hard. The precision we have to use is going to be dependent on the slot config.
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
      depositMaturityDuration = 1.hours.quantize(slotConfig),
      depositAbsorptionDuration = 48.hours.quantize(slotConfig),
    )

    trait Section {
        def txTiming: TxTiming

        def minSettlementDuration: QuantizedFiniteDuration
        def inactivityMarginDuration: QuantizedFiniteDuration
        def silenceDuration: QuantizedFiniteDuration
        def depositMaturityDuration: QuantizedFiniteDuration
        def depositAbsorptionDuration: QuantizedFiniteDuration
    }
}
