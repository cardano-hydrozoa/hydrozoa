package hydrozoa.multisig.consensus.transport

import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import org.http4s.Uri

/** Typed events emitted by [[CoilPeerWsTransport]]. Pure data; formatters in
  * [[CoilPeerWsTransportEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait CoilPeerWsTransportEvent

object CoilPeerWsTransportEvent:

    // ---- send ----

    /** `send` was called with a request variant that cannot be serialised over the wire. */
    final case class DroppingNonWireRequest(request: LiaisonProtocol.HubToCoilRequest)
        extends CoilPeerWsTransportEvent

    // ---- inbound dispatch ----

    /** An inbound frame arrived from the hub but no local hub-liaison is registered. */
    case object NoLiaisonForInbound extends CoilPeerWsTransportEvent

    /** Received an inbound wire payload from the hub that is not in the hub-emitted subset. */
    final case class UnexpectedInboundWire(payload: CoilFrame.Wire) extends CoilPeerWsTransportEvent

    /** A frame received on the active dialer connection could not be decoded. */
    final case class DecodeError(cause: Throwable) extends CoilPeerWsTransportEvent

    // ---- dialer ----

    /** The dialer successfully connected to the hub. */
    final case class DialerConnected(uri: Uri) extends CoilPeerWsTransportEvent

    /** A dialer attempt to the hub failed. */
    final case class DialerFailed(cause: Throwable) extends CoilPeerWsTransportEvent

    /** The connection to the hub ended without error — the peer closed cleanly, or the receive side
      * reached end of stream. A read-deadline expiry is an **error** and surfaces as
      * [[DialerFailed]], not here.
      *
      * Logged because `once` returning normally is otherwise an unlogged state change: the dialer
      * goes straight back to redialing and the link looks continuously up.
      */
    final case class DialerDisconnected(uri: Uri) extends CoilPeerWsTransportEvent
