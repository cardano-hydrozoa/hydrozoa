package hydrozoa.multisig.consensus.limiter

/** Typed events emitted by [[Limiter]]. Pure data; formatters in [[LimiterEventFormat]] decide how
  * each variant is rendered to a particular sink.
  */
sealed trait LimiterEvent

object LimiterEvent:
    case object Started extends LimiterEvent

    final case class HoldingMsg(msgType: String, holdMs: Long) extends LimiterEvent
