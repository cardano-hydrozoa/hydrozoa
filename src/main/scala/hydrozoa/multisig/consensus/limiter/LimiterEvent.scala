package hydrozoa.multisig.consensus.limiter

/** Typed events emitted by [[Limiter]]. Pure data; formatters in [[LimiterEventFormat]] decide how
  * each variant is rendered to a particular sink.
  */
sealed trait LimiterEvent

object LimiterEvent:
    case object Started extends LimiterEvent

    /** Emitted once when a hold begins, not once per slice. */
    final case class HoldingMsg(msgType: String, holdMs: Long) extends LimiterEvent

    /** The backlog gate re-derived its multiplier from a completed downstream cycle. */
    final case class GateUpdated(backlog: Long, residual: Double, multiplier: Double)
        extends LimiterEvent

    /** More than one throttled message was queued at once. Upstream is supposed to be single-flight
      * on this lane, so this says an invariant the gate's sizing assumes no longer holds.
      */
    final case class QueueDepthUnexpected(throttledPending: Int) extends LimiterEvent
