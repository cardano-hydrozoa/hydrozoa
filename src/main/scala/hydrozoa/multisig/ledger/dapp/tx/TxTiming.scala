package hydrozoa.multisig.ledger.dapp.tx

import scala.concurrent.duration.{DurationInt, FiniteDuration, MILLISECONDS}
import scalus.cardano.ledger.{Slot, SlotConfig}

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
  *   Minimal length of a settlement (finalization) tx's validity range.
  * @param inactivityMarginDuration
  *   The minimal frequency of major blocks (in case no activity happens).
  * @param silenceDuration
  *   A fixed-time gap between concurrent txs (i.e. fallback N and settlement/finalization N+1) to
  *   prevent contention, typically a small value like 5 min.
  * @param initializationFallbackDeviation
  *   Since it doesn't make sense to ask users to specify exact the same TTL/validity start slot for
  *   txs in the initialization sequence that we may calculate based on the time the initialization
  *   request was received, we need to allow some deviation which is defined by that parameter. The
  *   rule is that the specified value in the txs should stay in the [calculatedTime -
  *   initializationFallbackDeviation; calculatedTime + initializationFallbackDeviation].
  */
final case class TxTiming(
    minSettlementDuration: FiniteDuration,
    inactivityMarginDuration: FiniteDuration,
    silenceDuration: FiniteDuration,
    depositMaturityDuration: FiniteDuration,
    depositAbsorptionDuration: FiniteDuration,
)

object TxTiming:
    val default = TxTiming(
      minSettlementDuration = 12.hours,
      inactivityMarginDuration = 24.hours,
      silenceDuration = 5.minutes,
      depositMaturityDuration = 1.hours,
      depositAbsorptionDuration = 48.hours,
    )

    extension (instant: java.time.Instant) {
        def +(duration: FiniteDuration): java.time.Instant =
            instant.plusNanos(duration.toNanos)
        def -(duration: FiniteDuration): java.time.Instant =
            instant.minusNanos(duration.toNanos)
        def toSlot(slotConfig: SlotConfig): Slot = {
            Slot(slotConfig.timeToSlot(instant.toEpochMilli))
        }

        /** Convert into a FiniteDuration from the unix epoch (jan 1st 1970)
          * @return
          */
        def toEpochFiniteDuration: FiniteDuration =
            FiniteDuration(instant.toEpochMilli, MILLISECONDS)
    }

    extension (slot: Slot) {
        def toInstant(slotConfig: SlotConfig): java.time.Instant =
            java.time.Instant.ofEpochMilli(slotConfig.slotToTime(slot.slot))
    }

    extension (fd: FiniteDuration) {

        /** Convert an finite duration to an Instant given as the number of milliseconds from Jan
          * 1st 1970
          * @return
          */
        def toEpochInstant: java.time.Instant =
            java.time.Instant.ofEpochMilli(fd.toMillis)
    }
