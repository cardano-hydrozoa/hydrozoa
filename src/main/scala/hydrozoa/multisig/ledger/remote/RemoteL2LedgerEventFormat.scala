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
            case ConnectionError(attempt, backoff, cause) =>
                LogEvent(
                  Level.Warn,
                  s"Connection error (attempt ${attempt + 1}), reconnecting after $backoff",
                  cause = Some(cause),
                  routingKey = Some("RemoteL2Ledger")
                )
            case ReconnectionAttemptFailed(cause) =>
                LogEvent(
                  Level.Debug,
                  s"Reconnection attempt failed, will retry: ${cause.getMessage}",
                  cause = Some(cause),
                  routingKey = Some("RemoteL2Ledger")
                )
            case Sending(message) =>
                debug(s"Sending request: $message")
            case Received(message) =>
                debug(s"Received response: $message")
        }
    }
