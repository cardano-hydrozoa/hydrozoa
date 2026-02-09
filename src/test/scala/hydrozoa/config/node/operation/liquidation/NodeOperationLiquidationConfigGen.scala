package hydrozoa.config.node.operation.liquidation

import org.scalacheck.Gen
import scala.concurrent.duration.DurationInt

lazy val nodeOperationLiquidationConfigGen: Gen[NodeOperationLiquidationConfig] =
    for {
        seconds <- Gen.choose(1 * 60, 10 * 60)
    } yield NodeOperationLiquidationConfig(
      liquidationBotPollingPeriod = seconds.seconds
    )
