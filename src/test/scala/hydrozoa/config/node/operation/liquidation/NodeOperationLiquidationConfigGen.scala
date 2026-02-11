package hydrozoa.config.node.operation.liquidation

import org.scalacheck.Gen
import scala.concurrent.duration.DurationInt

/** Generate a liquidation bot polling period between 1 and 10 minutes
  */
lazy val generateNodeOperationLiquidationConfig: Gen[NodeOperationLiquidationConfig] =
    for {
        seconds <- Gen.choose(1 * 60, 10 * 60)
    } yield NodeOperationLiquidationConfig(
      liquidationBotPollingPeriod = seconds.seconds
    )
