package hydrozoa.config.head.multisig.timing

import hydrozoa.config.head.multisig.timing.TxTimingEvent.*
import hydrozoa.lib.logging.LogEvent

/** Renderers from [[TxTimingEvent]] to [[LogEvent]]. */
object TxTimingEventFormat:

    /** Routes under `TxTiming`. */
    def humanFormat(e: TxTimingEvent): LogEvent = {
        val ev = LogEvent.From(Map.empty, "TxTiming")
        import ev.*
        e match {
            case CanStayMinor(fallback, fmbt, end) =>
                trace(
                  s"blockCanStayMinor: competingFallbackStartTime: $fallback, " +
                      s"forcedMajorBlockWakeupTime: $fmbt, blockCreationEndTime: $end"
                )
        }
    }
