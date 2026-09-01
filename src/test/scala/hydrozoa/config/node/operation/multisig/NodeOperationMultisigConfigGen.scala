package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalacheck.Gen
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}

/** Generates a [[NodeOperationMultisigConfig]] with a `cardanoLiaisonPollingPeriod` uniformly
  * sampled from `[1ms, maxPollingPeriod]`, a `peerLiaisonMaxRequestsPerBatch` between 1 and 100,
  * and a `peerLiaisonOutboxCap` between 1 and 4096 — deliberately spanning values *below*
  * `maxPerReply`, so anything property-tested through this generator meets the floor case.
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
        outboxCap <- Gen.choose(1, 4096)
        millis <- Gen.choose(1L, maxPollingPeriod.toMillis)
        // Present and absent, so a codec round-trip covers both shapes of the field.
        transplantStack <- Gen.option(Gen.choose(0, 100000).map(StackNumber.apply))
    } yield NodeOperationMultisigConfig(
      cardanoLiaisonPollingPeriod = millis.millis,
      peerLiaisonMaxRequestsPerBatch = PositiveInt(maxRequestsPerBatch).get,
      peerLiaisonOutboxCap = PositiveInt(outboxCap).get,
      peerLiaisonResendInterval = 5.seconds,
      transplantStackNumber = transplantStack,
      rateLimits = rateLimits
    )
