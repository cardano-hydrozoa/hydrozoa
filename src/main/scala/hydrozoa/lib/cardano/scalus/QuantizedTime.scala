package hydrozoa.lib.cardano.scalus

import cats.effect.*
import cats.effect.IO.*
import java.time.Instant
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.{Slot, SlotConfig}
import scalus.ledger.api.v3.PosixTime

/** In Cardano, our notion of time is constrained by:
  *
  *   - a so-called PosixTime type defined by plutus-ledger-api (and inherited by scalus) that
  *     measure _milliseconds_ since the Unix epoch
  *   - Slot, which measure in some multiple (`n`) of a fixed number of non-negative milliseconds
  *     ([[slotLength]] since some arbitrary starting posix time ([[zeroSlot]]/[[zeroTime]])
  *
  * What this mean is that converting from time to slots may be lossy if the `[[slotLength]]` and de
  * facto precision of PosixTime don't align. This type quantize instants and durations so that they
  * align precisely on these boundaries; i.e., each time, when represented as PosixTime, will be an
  * integer multiple of [[slotLength]. Note that we [[require]] addition or subtraction of a
  * QuantizedFiniteDuration and a QuantizedFiniteTime to have the same slot configuration
  */
/*
TODO:
 - Tests for this would include:
   - Ensuring round trips from Quantized -> NonQuantized -> Quantized
   - Ensuring group laws? Probably abelian group on (+)? Anything stronger?
 - The safety of these operations are dubious.
   - (1): There is an integer division in slot conversion, so there could be a divide by zero -- but this isn't
     really our concern
   - (2): The [[Slot]] type prohibits negative numbers, but the slot conversions just work on [[Long]]. So instead
     This means you can happily construct an invalid time value and then get a runtime exception when trying to
     construct a value of type Slot. This is probably actually useful, but still a sort of weird situation.
   - (3): These are all [[Long]]s internally, rather than [[BigInt]], so there is an overflow/underflow risk
     technically.
  - It should be possible to pass [SC <: SlotConfig] as a type-level literal in the parameters. But I tried it briefly,
    and couldn't quite figure out how to make things unify correctly.
    - This would potentially move the runtime [[require]] to a compile time check.
  - These could probably also be made sub-types of Instant/FiniteDuration, and potentially opaque type aliases?
  - Ensure soundnes of algebraic operations when combining quantized and unquantized values
    - i.e., is quantizedInstant + finiteDuration.quantize(quantizedInstant.slotConfig)
      always equal to QuantizedInstant(quantizedInstant.instant + finiteDuration, quantizedInstant.slotConfig) ?
 */
object QuantizedTime {

    case class QuantizedInstant private (instant: java.time.Instant, slotConfig: SlotConfig) {

        def isAfter(other: QuantizedInstant): Boolean = {
            // Whether or not this "require" is needed is up to semantic interpretation.
            // I'm choosing to include it because in our particular case such a comparison would almost certainly be a
            // programming error, and it is not a priori given what should happen if the instants being compared as "close"
            // within their respective quantization window.
            require(
              this.slotConfig == other.slotConfig,
              s"Tried to compare `isAfter` for $this and $other, but they have " +
                  "different slotConfigs"
            )
            this.instant.isAfter(other.instant)
        }

        def isBefore(other: QuantizedInstant): Boolean = {
            require(
              this.slotConfig == other.slotConfig,
              s"Tried to compare `isBefore` for $this and $other, but they have " +
                  "different slotConfigs"
            )
            this.instant.isBefore(other.instant)
        }

        /** WARNING: Will throw if the slot configuration is such that the instant is before the
          * zero slot
          *
          * @return
          */
        def toSlot: Slot =
            Slot(this.slotConfig.timeToSlot(this.instant.toEpochMilli))

        /** Add a finite duration to a quantized duration by first quantizing the finite duration.
          * WARNING: This can incur a loss of precision.
          *
          * @param duration
          * @return
          */
        def +(duration: FiniteDuration): QuantizedInstant =
            this + duration.quantize(this.slotConfig)

        /** Add a QuantizedFiniteDuration to a QuantizedInstant. Both must be quantized according to
          * the same slot configuration, or this will throw an exception.
          *
          * Because both times are quantized the same, this will not incur any loss of precision.
          */
        def +(duration: QuantizedFiniteDuration): QuantizedInstant = {
            require(
              this.slotConfig == duration.slotConfig,
              s"Tried to do ${this} + ${duration}, but they have different" +
                  " slot configurations"
            )
            new QuantizedInstant(
              slotConfig = slotConfig,
              instant = java.time.Instant.ofEpochMilli(
                instant.toEpochMilli + duration.finiteDuration.toMillis
              )
            )
        }

        /** Subtract a finite duration to a quantized duration by first quantizing the finite
          * duration. WARNING: This can incur a loss of precision.
          *
          * @param duration
          * @return
          */
        def -(finiteDuration: FiniteDuration): QuantizedInstant =
            this - finiteDuration.quantize(this.slotConfig)

        /** Subtract a QuantizedFiniteDuration to a QuantizedInstant. Both must be quantized
          * according to the same slot configuration, or this will throw an exception.
          *
          * Because both times are quantized the same, this will not incur any loss of precision.
          */
        def -(duration: QuantizedFiniteDuration): QuantizedInstant = {
            require(
              this.slotConfig == duration.slotConfig,
              s"Tried to do ${this} - ${duration}, but they have different" +
                  " slot configurations"
            )
            new QuantizedInstant(
              slotConfig = slotConfig,
              instant = java.time.Instant.ofEpochMilli(
                instant.toEpochMilli - duration.finiteDuration.toMillis
              )
            )
        }

        def <(other: QuantizedInstant): Boolean =
            this.instant < other.instant

        def -(other: QuantizedInstant): QuantizedFiniteDuration = {
            require(
              this.slotConfig == other.slotConfig,
              s"Tried to subtract $this and $other, but they have " +
                  "different slotConfigs"
            )
            QuantizedFiniteDuration(
              slotConfig = this.slotConfig,
              finiteDuration =
                  // The fundamental precision that the quantized times are measured in MUST be multiples of epoch milli,
                  // because this is what scalus PosixTime is measure in. Thus, this conversion should be safe
                  FiniteDuration(
                    this.instant.toEpochMilli - other.instant.toEpochMilli,
                    MILLISECONDS
                  )
            )
        }

    }

    case class QuantizedFiniteDuration private (
        finiteDuration: FiniteDuration,
        slotConfig: SlotConfig
    ) {
        def <=(other: QuantizedFiniteDuration): Boolean =
            this.finiteDuration <= other.finiteDuration

        def >=(other: QuantizedFiniteDuration): Boolean =
            this.finiteDuration >= other.finiteDuration
    }

    object QuantizedInstant {
        def apply(slotConfig: SlotConfig, instant: java.time.Instant): QuantizedInstant =
            new QuantizedInstant(
              slotConfig = slotConfig,
              instant = Instant.ofEpochMilli(
                slotConfig.slotToTime(slotConfig.timeToSlot(instant.toEpochMilli))
              )
            )

        def realTimeQuantizedInstant(slotConfig: SlotConfig): IO[QuantizedInstant] =
            IO.realTimeInstant.map(_.quantize(slotConfig))
    }

    object QuantizedFiniteDuration {
        def apply(slotConfig: SlotConfig, finiteDuration: FiniteDuration): QuantizedFiniteDuration =
            new QuantizedFiniteDuration(
              slotConfig = slotConfig,
              finiteDuration = FiniteDuration(
                slotConfig.slotToTime(slotConfig.timeToSlot(finiteDuration.toMillis)),
                MILLISECONDS
              )
            )
    }

    extension (instant: java.time.Instant) {

        def quantize(slotConfig: SlotConfig): QuantizedInstant =
            QuantizedInstant(slotConfig, instant)

        /** A quantization method that REQUIRES that the time comes pre-quantized, otherwise it
          * throws an exception.
          *
          * This is used in places where we MUST have a pre-quantized time, such as in the refund
          * validity start in the the deposit datum
          *
          * @param slotConfig
          * @return
          */
        def quantizeLosslessUnsafe(slotConfig: SlotConfig): QuantizedInstant = {
            val q = instant.quantize(slotConfig)
            require(q.instant == instant, s"Instant was not pre-quantized according to $slotConfig")
            q
        }

    }

    extension (s: Slot) {
        def toQuantizedInstant(slotConfig: SlotConfig): QuantizedInstant =
            java.time.Instant.ofEpochMilli(slotConfig.slotToTime(s.slot)).quantize(slotConfig)
    }

    extension (fd: FiniteDuration) {
        def quantize(slotConfig: SlotConfig): QuantizedFiniteDuration =
            QuantizedFiniteDuration(slotConfig, fd)

        def toEpochQuantizedInstant(slotConfig: SlotConfig): QuantizedInstant = {
            // See "java.time.instant.ofEpochMilli"
            val epochNano = fd.toNanos
            val secs = Math.floorDiv(epochNano, 1_000_000_000)
            val nanos = Math.floorMod(epochNano, 1_000_000_000)
            java.time.Instant.ofEpochSecond(secs, nanos).quantize(slotConfig)
        }
    }

    extension (p: PosixTime) {
        def toEpochQuantizedInstant(slotConfig: SlotConfig): QuantizedInstant = {
            java.time.Instant.ofEpochMilli(p.toLong).quantize(slotConfig)
        }
    }

}
