package hydrozoa.config.head.parameters

import hydrozoa.lib.cardano.scalus.QuantizedTime.given
import io.circe.*
import io.circe.generic.semiauto.*
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Per-message-type minimum wall-clock periods enforced by the
  * [[hydrozoa.multisig.consensus.limiter.Limiter]] actor sitting between two actors.
  *
  * Each throttled message `m` carries its own timestamp (see
  * [[hydrozoa.multisig.consensus.limiter.LimiterTimestamp]]) and is held until
  * `m.limiterTimestamp + minPeriod(m) <= now` — the gate is computed from the message itself, not
  * from limiter-side memory of previous messages. Non-throttled messages on the same lane block
  * behind any currently-held message (strict FIFO).
  *
  * The defaults below are non-zero: these lanes are limited unless a config overrides them.
  *
  * Head-agreed, not per-node: both knobs gate consensus cadence, so peers running different values
  * produce different blocks. They live in [[HeadParameters]] and are covered by `headParamsHash`
  * (design/head-params-hash.md).
  */
final case class RateLimits(
    override val softBlockMinPeriod: FiniteDuration,
    override val hardStackMinPeriod: FiniteDuration
) extends RateLimits.Section {
    override transparent inline def rateLimits: RateLimits = this
}

object RateLimits {
    trait Section {
        def rateLimits: RateLimits

        /** Minimum wall-clock gap between consecutive
          * [[hydrozoa.multisig.ledger.block.Block.SoftConfirmed]] forwards from
          * [[hydrozoa.multisig.consensus.FastConsensusActor]] to
          * [[hydrozoa.multisig.consensus.BlockWeaver]].
          */
        def softBlockMinPeriod: FiniteDuration = rateLimits.softBlockMinPeriod

        /** Minimum wall-clock gap between consecutive
          * [[hydrozoa.multisig.ledger.stack.Stack.HardConfirmed]] forwards from
          * [[hydrozoa.multisig.consensus.SlowConsensusActor]] to
          * [[hydrozoa.multisig.consensus.StackComposer]].
          */
        def hardStackMinPeriod: FiniteDuration = rateLimits.hardStackMinPeriod
    }

    lazy val default: RateLimits = RateLimits(
      softBlockMinPeriod = 100.milliseconds,
      hardStackMinPeriod = 30.seconds
    )

    given Encoder[RateLimits] = deriveEncoder[RateLimits]

    given Decoder[RateLimits] = deriveDecoder[RateLimits]
}
