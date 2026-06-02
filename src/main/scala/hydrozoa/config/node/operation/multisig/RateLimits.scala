package hydrozoa.config.node.operation.multisig

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
  * Defaults are zero (no limiting) so production behavior is unchanged unless explicitly
  * configured.
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
      softBlockMinPeriod = 20.seconds,
      hardStackMinPeriod = 3.minutes
    )

    given Encoder[RateLimits] = deriveEncoder[RateLimits]

    given Decoder[RateLimits] = deriveDecoder[RateLimits]
}
