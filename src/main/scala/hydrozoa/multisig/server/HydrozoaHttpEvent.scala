package hydrozoa.multisig.server

import com.comcast.ip4s.{Host, Port}

/** Typed events emitted by [[HydrozoaServer]] and [[HydrozoaRoutes]]. Pure data; formatters in
  * [[HydrozoaHttpEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait HydrozoaHttpEvent

object HydrozoaHttpEvent:

    // ---- HydrozoaServer ----

    /** The Ember HTTP server started listening on [[host]]:[[port]]. */
    final case class ServerStarted(host: Host, port: Port) extends HydrozoaHttpEvent

    // ---- HydrozoaRoutes ----

    /** Inbound request headers (debug). */
    final case class RequestHeaders(path: String, headers: String) extends HydrozoaHttpEvent

    /** Inbound request body (debug). */
    final case class RequestBody(path: String, body: String) extends HydrozoaHttpEvent

    /** The request body failed JSON parsing. */
    final case class JsonParseError(path: String, cause: Throwable) extends HydrozoaHttpEvent

    /** A successfully parsed JSON body failed to decode to the expected type. */
    final case class JsonDecodeError(path: String, cause: Throwable) extends HydrozoaHttpEvent

    /** Decode-failure history (the circe cursor breadcrumb path), separated for diagnostics. */
    final case class JsonDecodeErrorHistory(path: String, history: String) extends HydrozoaHttpEvent

    /** The request body decoded to a domain object (debug).
      *
      * Carries the request's shape rather than the request. Rendering a decoded request means
      * `toString` on its payloads, and a `ByteString`'s `toString` is its hex — which scalus caches
      * on the instance, so a payload that is later retained is retained at three times its size.
      * The event is built on every request whatever the log level, so that cost is paid whether or
      * not anyone is reading debug logs.
      */
    final case class RequestDecoded(path: String, kind: String, payloadBytes: Int)
        extends HydrozoaHttpEvent

    /** A request was rejected by screening or backpressure — an expected 400, not a fault, so it
      * carries only the client-facing reason (no exception or stack trace).
      */
    final case class RequestRejected(path: String, reason: String) extends HydrozoaHttpEvent

    /** An exception escaped the route handler. */
    final case class RequestFailed(path: String, cause: Throwable) extends HydrozoaHttpEvent

    /** An unauthenticated request to an admin endpoint. */
    final case class UnauthorizedAdmin(path: String) extends HydrozoaHttpEvent

    /** Admin finalize: triggering local head finalization. */
    case object FinalizeTriggered extends HydrozoaHttpEvent

    /** Admin finalize: finalization signal forwarded to BlockWeaver. */
    case object FinalizeSignalSent extends HydrozoaHttpEvent
