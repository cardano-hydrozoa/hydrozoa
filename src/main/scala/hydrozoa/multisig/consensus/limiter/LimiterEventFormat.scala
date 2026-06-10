package hydrozoa.multisig.consensus.limiter

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.consensus.limiter.LimiterEvent.*

/** Renderers from [[LimiterEvent]] to [[LogEvent]] for various back-end sinks. */
object LimiterEventFormat:

    /** `label` identifies which limiter lane this is (e.g. `"BlockWeaver"`, `"StackComposer"`). */
    def humanFormat(label: String)(e: LimiterEvent): LogEvent = {
        val rk = Some(s"Limiter.$label")
        def debug(msg: String) = LogEvent(Level.Debug, msg, routingKey = rk)
        e match {
            case Started           => debug(s"Limiter[$label] started.")
            case HoldingMsg(t, ms) => debug(s"Limiter[$label] holding $t for ${ms}ms")
        }
    }
