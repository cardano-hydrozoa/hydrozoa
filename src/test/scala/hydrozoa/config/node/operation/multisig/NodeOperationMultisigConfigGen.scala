package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.number.PositiveInt
import org.scalacheck.Gen
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}

/** Generates a [[NodeOperationMultisigConfig]] with a `cardanoLiaisonPollingPeriod` uniformly
  * sampled from `[1ms, maxPollingPeriod]` and a `peerLiaisonMaxRequestsPerBatch` between 1 and 100.
  *
  * The polling period must respect the head's
  * [[hydrozoa.config.head.multisig.timing.TxTiming.Section.maxCardanoLiaisonPollingPeriod]]
  * invariant, so callers should pass `headConfig.maxCardanoLiaisonPollingPeriod`.
  */
def generateNodeOperationMultisigConfig(
    maxPollingPeriod: FiniteDuration = 60.seconds
): Gen[NodeOperationMultisigConfig] =
    for {
        maxRequestsPerBatch <- Gen.choose(1, 100)
        millis <- Gen.choose(1L, maxPollingPeriod.toMillis)
    } yield NodeOperationMultisigConfig(
      cardanoLiaisonPollingPeriod = millis.millis,
      peerLiaisonMaxRequestsPerBatch = PositiveInt(maxRequestsPerBatch).get,
      peerLiaisonResendInterval = 5.seconds,
      rateLimits = RateLimits.default
    )
