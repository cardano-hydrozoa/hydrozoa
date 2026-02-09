package hydrozoa.config.head.rulebased

import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.ledger.ArbitraryInstances.{*, given}
import scalus.cardano.ledger.SlotConfig

import scala.concurrent.duration.DurationInt

// Not setting a default on purpose. 90% of the time you will want this to 
// be coherent with some other slot config, so it's better to force the user
// to pass None explicitly.
def disputeResolutionConfigGen(genSlotConfig : Option[Gen[SlotConfig]]) : Gen[DisputeResolutionConfig] =
    for {
        slotConfig <- genSlotConfig.getOrElse(
            Gen.oneOf(SlotConfig.mainnet, SlotConfig.preview, SlotConfig.preprod))
        // 1 hour to 5 days
        seconds <- Gen.choose(60*60, 60*60*24*5)
    } yield DisputeResolutionConfig(
      votingDuration = QuantizedFiniteDuration(slotConfig = slotConfig, 
          finiteDuration = seconds.seconds)
    )