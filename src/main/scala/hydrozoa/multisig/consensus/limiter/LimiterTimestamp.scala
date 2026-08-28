package hydrozoa.multisig.consensus.limiter

import hydrozoa.config.node.operation.multisig.RateLimits
import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/** Marker mixed in on messages that the [[Limiter]] actor should throttle.
  *
  * Each implementation exposes:
  *
  *   - [[limiterTimestamp]] — the effective end-time of the upstream work the message represents
  *     (e.g. block-creation end-time, stack-creation end-time). The limiter holds the next
  *     throttled message of the same lane until `limiterTimestamp + minPeriod` of wall-clock time
  *     has elapsed.
  *   - [[minPeriod]] — the minimum gap between consecutive throttled messages on this lane, drawn
  *     from [[RateLimits]] so each message type binds to its own config knob.
  *
  * Messages NOT extending this trait pass through the limiter immediately, but still travel through
  * its single mailbox (strict FIFO with held throttled messages).
  */
trait LimiterTimestamp {

    /** Wall-clock end-time of the upstream work that produced this message. Limiter forwards the
      * next throttled message at `max(now, this + minPeriod)`.
      */
    def limiterTimestamp: Instant

    /** Minimum wall-clock gap between this message and the next throttled message on the same lane.
      */
    def minPeriod(using RateLimits.Section): FiniteDuration

    /** Released without pacing, in arrival order, and does not restart the spacing clock.
      *
      * For messages that end the lane: they add no ongoing backlog, so pacing them only costs
      * latency. In arrival order, not ahead of the queue — overtaking a held message would reorder
      * the lane.
      */
    def limiterExempt: Boolean = false
}
