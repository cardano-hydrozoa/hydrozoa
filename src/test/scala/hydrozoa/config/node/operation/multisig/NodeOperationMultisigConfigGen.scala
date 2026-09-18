package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.number.PositiveInt
import org.scalacheck.Gen
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}

/** Generates a [[NodeOperationMultisigConfig]] with a `cardanoLiaisonPollingPeriod` uniformly
  * sampled from `[1ms, maxPollingPeriod]`, a `peerLiaisonMaxRequestsPerBatch` between 1 and 100,
  * and a `peerLiaisonOutboxDepth` between 1 and 8 replies' worth.
  *
  * The polling period must respect the head's
  * [[hydrozoa.config.head.multisig.timing.TxTiming.Section.maxCardanoLiaisonPollingPeriod]]
  * invariant, so callers should pass `headConfig.maxCardanoLiaisonPollingPeriod`.
  */
def generateNodeOperationMultisigConfig(
    maxPollingPeriod: FiniteDuration = 60.seconds,
    rateLimits: RateLimits = RateLimits.default
): Gen[NodeOperationMultisigConfig] =
    for {
        maxRequestsPerBatch <- Gen.choose(1, 100)
        outboxDepth <- Gen.choose(1, 8)
        millis <- Gen.choose(1L, maxPollingPeriod.toMillis)
        // Present and absent, so a codec round-trip covers both shapes of the field.
    } yield NodeOperationMultisigConfig(
      cardanoLiaisonPollingPeriod = millis.millis,
      peerLiaisonMaxRequestsPerBatch = PositiveInt(maxRequestsPerBatch).get,
      peerLiaisonOutboxDepth = PositiveInt(outboxDepth).get,
      peerLiaisonResendInterval = 5.seconds,
      rateLimits = rateLimits
    )
