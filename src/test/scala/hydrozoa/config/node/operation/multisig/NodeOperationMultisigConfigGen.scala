package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.number.PositiveInt
import org.scalacheck.Gen
import scala.concurrent.duration.DurationInt

/** Generates a cardano liaison polling period between 1 and 60 seconds, and between 1 and 100 max
  * events per batch.
  */
lazy val generateNodeOperationMultisigConfig: Gen[NodeOperationMultisigConfig] =
    for {
        posNum <- Gen.choose(1, 100)
        seconds <- Gen.choose(1, 1 * 60)
    } yield NodeOperationMultisigConfig(
      cardanoLiaisonPollingPeriod = seconds.seconds,
      peerLiaisonMaxEventsPerBatch = PositiveInt(posNum).get
    )
