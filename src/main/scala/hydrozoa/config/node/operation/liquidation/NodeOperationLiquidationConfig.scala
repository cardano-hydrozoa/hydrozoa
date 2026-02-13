package hydrozoa.config.node.operation.liquidation

import scala.concurrent.duration.FiniteDuration

final case class NodeOperationLiquidationConfig(
    override val liquidationBotPollingPeriod: FiniteDuration
) extends NodeOperationLiquidationConfig.Section {
    override transparent inline def nodeOperationLiquidationConfig: NodeOperationLiquidationConfig =
        this
}

object NodeOperationLiquidationConfig {
    trait Section {
        def nodeOperationLiquidationConfig: NodeOperationLiquidationConfig

        def liquidationBotPollingPeriod: FiniteDuration
    }
}
