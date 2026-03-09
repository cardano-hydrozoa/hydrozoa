package hydrozoa.config.head.rulebased.dispute

import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.SlotConfig

final case class DisputeResolutionConfig(
    override val votingDuration: QuantizedFiniteDuration
) extends DisputeResolutionConfig.Section {
    override transparent inline def disputeResolutionConfig: DisputeResolutionConfig = this
}

// TODO: Add utility functions for fallback tx builder and voting?
object DisputeResolutionConfig {
    trait Section {
        def disputeResolutionConfig: DisputeResolutionConfig

        def votingDuration: QuantizedFiniteDuration
    }

    def default(slotConfig: SlotConfig): DisputeResolutionConfig =
        DisputeResolutionConfig(
          QuantizedFiniteDuration(slotConfig = slotConfig, finiteDuration = 2.days)
        )
}
