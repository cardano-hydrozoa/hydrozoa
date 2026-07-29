package hydrozoa.multisig.ledger.remote

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.ledger.remote.RemoteL2LedgerEvent.*

/** Renderers from [[RemoteL2LedgerEvent]] to [[LogEvent]]. */
object RemoteL2LedgerEventFormat:

    /** Routes under `RemoteL2Ledger`. */
    def humanFormat(e: RemoteL2LedgerEvent): LogEvent = {
        val ev = LogEvent.From(Map.empty, "RemoteL2Ledger")
        import ev.*
        e match {
            case Connecting(uri) =>
                info(s"Connecting to WebSocket at $uri")
            case Connected(uri) =>
                info(s"Successfully connected to $uri")
            case Unavailable(uri, cause) =>
                LogEvent(
                  Level.Warn,
                  s"Remote L2 ledger at $uri is unavailable; try again later",
                  cause = Some(cause),
                  routingKey = Some("RemoteL2Ledger")
                )
            case Sending(message) =>
                debug(s"Sending request: $message")
            case Received(message) =>
                debug(s"Received response: $message")
        }
    }
