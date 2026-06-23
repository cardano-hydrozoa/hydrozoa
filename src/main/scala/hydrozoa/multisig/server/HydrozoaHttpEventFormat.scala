package hydrozoa.multisig.server

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.server.HydrozoaHttpEvent.*

/** Renderers from [[HydrozoaHttpEvent]] to [[LogEvent]]. */
object HydrozoaHttpEventFormat:

    /** Routes under `HydrozoaHttp` — covers both server lifecycle and per-route events. */
    def humanFormat(e: HydrozoaHttpEvent): LogEvent = {
        val ev = LogEvent.From(Map.empty, "HydrozoaHttp")
        import ev.*
        e match {
            case ServerStarted(host, port) =>
                info(s"Hydrozoa HTTP server started at http://$host:$port")
            case RequestHeaders(path, headers) =>
                debug(s"$path - Headers: $headers")
            case RequestBody(path, body) =>
                debug(s"$path - Body: $body")
            case JsonParseError(path, cause) =>
                LogEvent(
                  hydrozoa.lib.logging.Level.Error,
                  s"$path - JSON parse error: ${cause.getMessage}",
                  cause = Some(cause),
                  routingKey = Some("HydrozoaHttp")
                )
            case JsonDecodeError(path, cause) =>
                LogEvent(
                  hydrozoa.lib.logging.Level.Error,
                  s"$path - JSON decode error: ${cause.getMessage}",
                  cause = Some(cause),
                  routingKey = Some("HydrozoaHttp")
                )
            case JsonDecodeErrorHistory(path, history) =>
                error(s"$path - Decode error history: $history")
            case RequestDecoded(path, decoded) =>
                debug(s"$path - Decoded: $decoded")
            case RequestFailed(path, cause) =>
                LogEvent(
                  hydrozoa.lib.logging.Level.Error,
                  s"$path - Error: ${cause.getMessage}",
                  cause = Some(cause),
                  routingKey = Some("HydrozoaHttp")
                )
            case UnauthorizedAdmin(path) =>
                warn(s"$path - Unauthorized attempt")
            case FinalizeTriggered =>
                info("POST /api/admin/finalize - Triggering local head finalization")
            case FinalizeSignalSent =>
                info("POST /api/admin/finalize - Finalization signal sent to BlockWeaver")
        }
    }
