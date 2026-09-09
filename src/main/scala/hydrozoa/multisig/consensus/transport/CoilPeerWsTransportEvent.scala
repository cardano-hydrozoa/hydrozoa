package hydrozoa.multisig.consensus.transport

import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import org.http4s.Uri
import scala.concurrent.duration.FiniteDuration

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

    /** A dial attempt sat in the WebSocket handshake past its budget and was abandoned.
      *
      * Distinct from [[DialerFailed]] because nothing failed: the hub accepted the TCP connection
      * and then never answered. That attempt cannot be cancelled (the client builds its socket in
      * an uncancelable acquire), so it is left running and the dialer moves on — which is the only
      * way this peer keeps reconnecting at all.
      */
    final case class DialerHandshakeStalled(uri: Uri, after: FiniteDuration)
        extends CoilPeerWsTransportEvent

    /** An abandoned dial attempt completed its handshake after the loop had given up on it, and
      * dropped the socket instead of using it.
      *
      * The pair to [[DialerHandshakeStalled]]: seeing both for one attempt means the hub is merely
      * slower than `handshakeBudget`, not black-holing, and the budget is the thing to raise.
      * Seeing the stall alone means the handshake never landed at all.
      */
    final case class DialerHandshakeLate(uri: Uri) extends CoilPeerWsTransportEvent

    /** The connection to the hub ended without error — the peer closed cleanly, or the receive side
      * reached end of stream. A read-deadline expiry is an **error** and surfaces as
      * [[DialerFailed]], not here.
      *
      * Logged because `once` returning normally is otherwise an unlogged state change: the dialer
      * goes straight back to redialing and the link looks continuously up.
      */
    final case class DialerDisconnected(uri: Uri) extends CoilPeerWsTransportEvent
