package hydrozoa.lib.cardano.scalus

import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose

given Choose[QuantizedInstant] with {
    def choose(low: QuantizedInstant, high: QuantizedInstant): Gen[QuantizedInstant] = {
        assert(
          low.slotConfig == high.slotConfig,
          "slot config should be identical across low and high bounds"
        )
        Gen.choose(low.instant, high.instant)
            .map(QuantizedInstant(low.slotConfig, _))
    }
}
