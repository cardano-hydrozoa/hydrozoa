package hydrozoa.config.head.multisig.settlement

import hydrozoa.lib.number.PositiveInt

final case class SettlementConfig(
    override val maxDepositsAbsorbedPerBlock: PositiveInt
) extends SettlementConfig.Section {
    override transparent inline def settlementConfig: SettlementConfig = this
}

object SettlementConfig {
    trait Section {
        def settlementConfig: SettlementConfig

        def maxDepositsAbsorbedPerBlock: PositiveInt
    }
}
