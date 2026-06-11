package hydrozoa.multisig.consensus.limiter

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.limiter.LimiterEvent.*

/** Renderers from [[LimiterEvent]] to [[LogEvent]] for various back-end sinks. */
object LimiterEventFormat:

    /** `label` identifies which limiter lane this is (e.g. `"BlockWeaver"`, `"StackComposer"`). */
    def humanFormat(label: String)(e: LimiterEvent): LogEvent = {
        val ev = LogEvent.From(Map.empty, s"Limiter.$label")
        import ev.*
        e match {
            case Started           => debug(s"Limiter[$label] started.")
            case HoldingMsg(t, ms) => debug(s"Limiter[$label] holding $t for ${ms}ms")
        }
    }
