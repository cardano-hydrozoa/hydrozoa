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
}
