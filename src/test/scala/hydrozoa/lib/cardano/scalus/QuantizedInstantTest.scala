package hydrozoa.lib.cardano.scalus

import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import org.scalacheck.*
import scalus.cardano.ledger.SlotConfig
import scalus.cardano.ledger.ArbitraryInstances.given

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

object QuantizedInstantTest extends Properties("QuantizedInstant tests"){

    val genSlotConfig : Gen[SlotConfig] = {
        Gen.frequency(
            (10, Gen.const(SlotConfig.mainnet)),
            (10, Gen.const(SlotConfig.preprod)),
            (10, Gen.const(SlotConfig.preview)),
            (1, for {
                zeroTime <- Arbitrary.arbitrary[Long]
                zeroSlot <- Arbitrary.arbitrary[Long]
                slotLength <- Arbitrary.arbitrary[Long]
            } yield SlotConfig(zeroTime, zeroSlot, slotLength))
        )

    }

    val _ = property("Quantized instant constructor should never throw exception") =
        Prop.forAll(genSlotConfig, Arbitrary.arbitrary[Instant])((sc, instant) =>
            val _ = QuantizedInstant(slotConfig = sc, instant = instant)
            true
        )

    val _ = property("Quantized finite duration constructor should never throw exception") =
        Prop.forAll(genSlotConfig, Arbitrary.arbitrary[FiniteDuration])((sc, duration) =>
            val _ = QuantizedFiniteDuration(slotConfig = sc, finiteDuration = duration)
            true
        )

}
