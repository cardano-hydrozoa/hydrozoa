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

    /** The request body decoded to a domain object (debug). */
    final case class RequestDecoded(path: String, decoded: String) extends HydrozoaHttpEvent

    /** An exception escaped the route handler. */
    final case class RequestFailed(path: String, cause: Throwable) extends HydrozoaHttpEvent

    /** An unauthenticated request to an admin endpoint. */
    final case class UnauthorizedAdmin(path: String) extends HydrozoaHttpEvent

    /** Admin finalize: triggering local head finalization. */
    case object FinalizeTriggered extends HydrozoaHttpEvent

    /** Admin finalize: finalization signal forwarded to BlockWeaver. */
    case object FinalizeSignalSent extends HydrozoaHttpEvent
