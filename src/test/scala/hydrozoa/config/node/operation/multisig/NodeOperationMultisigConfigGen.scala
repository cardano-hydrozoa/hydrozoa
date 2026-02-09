package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.number.PositiveInt
import org.scalacheck.Gen
import scala.concurrent.duration.DurationInt

lazy val nodeOperationMultisigConfigGen: Gen[NodeOperationMultisigConfig] =
    for {
        posNum <- Gen.choose(1, 100)
        seconds <- Gen.choose(1, 1 * 60)
    } yield NodeOperationMultisigConfig(
      cardanoLiaisonPollingPeriod = seconds.seconds,
      peerLiaisonMaxEventsPerBatch = PositiveInt(posNum).get
    )
