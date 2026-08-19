package hydrozoa.multisig.ledger.remote

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.ledger.remote.RemoteL2ScreenerEvent.*

/** Renderers from [[RemoteL2ScreenerEvent]] to [[LogEvent]]. */
object RemoteL2ScreenerEventFormat:

    /** Routes under `RemoteL2Screener`. */
    def humanFormat(e: RemoteL2ScreenerEvent): LogEvent = {
        val ev = LogEvent.From(Map.empty, "RemoteL2Screener")
        import ev.*
        e match {
            case DepositRejected(reason) =>
                debug(s"Deposit rejected at screening: ${reason.getOrElse("(no reason given)")}")
            case ScreenerUnavailable(cause) =>
                LogEvent(
                  Level.Warn,
                  "Screening endpoint unavailable, passing the request through unscreened: " +
                      s"${cause.getMessage}",
                  cause = Some(cause),
                  routingKey = Some("RemoteL2Screener")
                )
        }
    }
