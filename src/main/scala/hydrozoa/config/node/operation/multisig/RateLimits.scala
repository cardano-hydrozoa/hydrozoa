package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.cardano.scalus.QuantizedTime.given
import hydrozoa.multisig.consensus.limiter.LimiterGate
import io.circe.*
import io.circe.generic.semiauto.*
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Per-message-type minimum wall-clock periods enforced by the
  * [[hydrozoa.multisig.consensus.limiter.Limiter]] actor sitting between two actors.
  *
  * Each lane is a **spacing gate** measured from the limiter's own last release, not from the
  * message's timestamp — see [[hydrozoa.multisig.consensus.limiter.Limiter]] for why that
  * distinction is what makes it a rate limit rather than a delay line.
  *
  * ⚠️ **Node-local.** These change only the cadence at which a peer offers work to its own
  * downstream, never what it sends, so peers may run different values without diverging. The cost
  * is a leader-dependent cadence under rotation; operators should align informally.
  *
  * Defaults are non-zero: these lanes are limited unless a config overrides them.
  */
final case class RateLimits(
    override val softBlockMinPeriod: FiniteDuration,
    override val hardStackMinPeriod: FiniteDuration,
    // Scala-level defaults, matching the decoder's fallbacks below, so that adding a
    // gate knob stays source-compatible with construction sites outside this project.
    override val blockBacklogSoftLimit: Int = RateLimits.defaultBlockBacklogSoftLimit,
    override val blockBacklogHardLimit: Int = RateLimits.defaultBlockBacklogHardLimit,
    override val blockGateFloor: Double = RateLimits.defaultBlockGateFloor,
    override val blockGateSmoothing: Double = RateLimits.defaultBlockGateSmoothing,
    override val blockGateSlice: FiniteDuration = RateLimits.defaultBlockGateSlice
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
          *
          * Binds only when the natural block cycle is faster than the period, so it is
          * self-disabling at low load and needs no feature flag. Throughput ceiling is
          * `1/period * maxRequestsPerBlock`; the cost is up to one period of extra latency for a
          * request arriving just after a block closes.
          */
        def softBlockMinPeriod: FiniteDuration = rateLimits.softBlockMinPeriod

        /** Minimum wall-clock gap between consecutive
          * [[hydrozoa.multisig.ledger.stack.Stack.HardConfirmed]] forwards from
          * [[hydrozoa.multisig.consensus.SlowConsensusActor]] to
          * [[hydrozoa.multisig.consensus.StackComposer]].
          */
        def hardStackMinPeriod: FiniteDuration = rateLimits.hardStackMinPeriod

        /** Soft-confirmed blocks outstanding per stack cycle below which the block gate stays fully
          * open. Above it the block lane is progressively slowed so block production cannot outrun
          * what stack production absorbs.
          *
          * Sized above the accumulation the shaper itself produces in one downstream cycle
          * (`1/softBlockMinPeriod * hardStackMinPeriod`), so a healthy head sits inside the flat
          * region and the gate does nothing unless the composer is falling behind.
          */
        def blockBacklogSoftLimit: Int = rateLimits.blockBacklogSoftLimit

        /** Backlog at which the block lane is slowed all the way to [[blockGateFloor]].
          *
          * Sized well under the point where draining the composer's mailbox turns superlinear
          * (cats-actors scans its queue per message), and far above healthy per-cycle accumulation.
          */
        def blockBacklogHardLimit: Int = rateLimits.blockBacklogHardLimit

        /** The slowest the block lane is ever shaped to, as a fraction of [[softBlockMinPeriod]]'s
          * rate. ⛔ Never 0. Block production is self-clocked by the soft confirmations this lane
          * releases, so a full stop would stop the clock that later reopens the gate.
          */
        def blockGateFloor: Double = rateLimits.blockGateFloor

        /** EWMA weight on the newest downstream cycle. Instantaneous depth is dominated by where in
          * the cycle it is sampled; a controller driven by that sawtooths at the cycle cadence.
          */
        def blockGateSmoothing: Double = rateLimits.blockGateSmoothing

        /** Longest single sleep the block limiter commits to before re-reading its mailbox, which
          * bounds how stale its multiplier can be while a hold is outstanding.
          */
        def blockGateSlice: FiniteDuration = rateLimits.blockGateSlice

        /** The block lane's gate, assembled from the knobs above. */
        def blockLimiterGate: LimiterGate = LimiterGate(
          backlogSoftLimit = blockBacklogSoftLimit,
          backlogHardLimit = blockBacklogHardLimit,
          floor = blockGateFloor,
          smoothing = blockGateSmoothing,
          slice = blockGateSlice
        )
    }

    val defaultBlockBacklogSoftLimit: Int = 600
    val defaultBlockBacklogHardLimit: Int = 3000
    val defaultBlockGateFloor: Double = 0.02
    val defaultBlockGateSmoothing: Double = 0.3
    val defaultBlockGateSlice: FiniteDuration = 150.milliseconds

    lazy val default: RateLimits = RateLimits(
      softBlockMinPeriod = 100.milliseconds,
      hardStackMinPeriod = 30.seconds,
      blockBacklogSoftLimit = defaultBlockBacklogSoftLimit,
      blockBacklogHardLimit = defaultBlockBacklogHardLimit,
      blockGateFloor = defaultBlockGateFloor,
      blockGateSmoothing = defaultBlockGateSmoothing,
      blockGateSlice = defaultBlockGateSlice
    )

    given Encoder[RateLimits] = deriveEncoder[RateLimits]

    /** Hand-written rather than derived so every gate field may be **absent**: a node whose config
      * fails to decode does not start at all, and every `private.json` already deployed predates
      * these fields. The defaults are chosen so that adopting the gate needs no config edit.
      */
    given Decoder[RateLimits] = Decoder.instance(c =>
        for {
            softBlock <- c.downField("softBlockMinPeriod").as[FiniteDuration]
            hardStack <- c.downField("hardStackMinPeriod").as[FiniteDuration]
            softLimit <- c.downField("blockBacklogSoftLimit").as[Option[Int]]
            hardLimit <- c.downField("blockBacklogHardLimit").as[Option[Int]]
            floor <- c.downField("blockGateFloor").as[Option[Double]]
            smoothing <- c.downField("blockGateSmoothing").as[Option[Double]]
            slice <- c.downField("blockGateSlice").as[Option[FiniteDuration]]
        } yield RateLimits(
          softBlockMinPeriod = softBlock,
          hardStackMinPeriod = hardStack,
          blockBacklogSoftLimit = softLimit.getOrElse(defaultBlockBacklogSoftLimit),
          blockBacklogHardLimit = hardLimit.getOrElse(defaultBlockBacklogHardLimit),
          blockGateFloor = floor.getOrElse(defaultBlockGateFloor),
          blockGateSmoothing = smoothing.getOrElse(defaultBlockGateSmoothing),
          blockGateSlice = slice.getOrElse(defaultBlockGateSlice)
        )
    )
}
