package hydrozoa.multisig.consensus.transport

import org.http4s.Uri

/** Typed events emitted by [[CoilPeerWsTransport]]. Pure data; formatters in
  * [[CoilPeerWsTransportEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait CoilPeerWsTransportEvent

object CoilPeerWsTransportEvent:

    // ---- send ----

    /** `send` was called with a request variant that cannot be serialised over the wire. */
    final case class DroppingNonWireRequest(request: String) extends CoilPeerWsTransportEvent

    // ---- inbound dispatch ----

    /** An inbound frame arrived from the hub but no local hub-liaison is registered. */
    case object NoLiaisonForInbound extends CoilPeerWsTransportEvent

    /** Received an inbound wire payload from the hub that is not in the hub-emitted subset. */
    final case class UnexpectedInboundWire(payload: String) extends CoilPeerWsTransportEvent

    /** A frame received on the active dialer connection could not be decoded. */
    final case class DecodeError(cause: Throwable) extends CoilPeerWsTransportEvent

    // ---- dialer ----

    /** The dialer successfully connected to the hub. */
    final case class DialerConnected(uri: Uri) extends CoilPeerWsTransportEvent

    /** A dialer attempt to the hub failed. */
    final case class DialerFailed(cause: Throwable) extends CoilPeerWsTransportEvent
