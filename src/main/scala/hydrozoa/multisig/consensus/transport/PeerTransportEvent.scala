package hydrozoa.multisig.consensus.transport

import hydrozoa.multisig.consensus.peer.HeadPeerId
import org.http4s.Uri
import scala.concurrent.duration.FiniteDuration

/** Typed events emitted by [[PeerTransport]]. Pure data; formatters in [[PeerTransportEventFormat]]
  * decide how each variant is rendered to a particular sink.
  */
sealed trait PeerTransportEvent

object PeerTransportEvent:

    // ---- send / dispatch ----

    /** `send` was called for a remote that has no outbox — wiring bug. */
    final case class NoOutboxForRemote(remote: HeadPeerId) extends PeerTransportEvent

    /** `send` was called with a request variant that cannot be serialised over the wire. */
    final case class DroppingNonWireRequest(remote: HeadPeerId) extends PeerTransportEvent

    /** An inbound frame arrived from a remote that has no registered local liaison. */
    final case class NoLiaisonForInbound(remote: HeadPeerId) extends PeerTransportEvent

    // ---- dialer (client side) ----

    /** A dialer successfully connected to a remote peer. */
    final case class DialerConnected(remote: HeadPeerId, uri: Uri) extends PeerTransportEvent

    /** A dialer attempt to a remote peer failed. */
    final case class DialerFailed(remote: HeadPeerId, cause: Throwable) extends PeerTransportEvent

    /** The connection to a remote ended without error — the peer closed cleanly, or the receive
      * side reached end of stream. A read-deadline expiry is an **error** and surfaces as
      * [[DialerFailed]], not here.
      *
      * Logged because a normal return is otherwise an unlogged state change.
      */
    final case class DialerDisconnected(remote: HeadPeerId, uri: Uri) extends PeerTransportEvent

    /** The dialer fiber for a remote peer was cancelled (resource release). */
    final case class DialerStopped(remote: HeadPeerId, uri: Uri) extends PeerTransportEvent

    /** A dial attempt sat in the WebSocket handshake past its budget and was abandoned. Nothing
      * failed: the remote accepted the TCP connection and never answered. The attempt cannot be
      * cancelled (the client builds its socket in an uncancelable acquire), so it is left running
      * and the dialer moves on — which is what keeps this peer reconnecting at all.
      */
    final case class DialerHandshakeStalled(remote: HeadPeerId, uri: Uri, after: FiniteDuration)
        extends PeerTransportEvent

    /** An abandoned dial attempt completed its handshake after the loop had given up on it, and
      * dropped the socket instead of using it. The pair to [[DialerHandshakeStalled]]: both for one
      * attempt means the remote is merely slower than `handshakeBudget`, not black-holing.
      */
    final case class DialerHandshakeLate(remote: HeadPeerId, uri: Uri) extends PeerTransportEvent

    /** A frame received on an active dialer connection could not be decoded. */
    final case class ClientDecodeError(remote: HeadPeerId, cause: Throwable)
        extends PeerTransportEvent

    // ---- server (accept side) ----

    /** The server accepted an inbound connection after receiving a valid `Hello`. */
    final case class ServerAccepted(remote: HeadPeerId) extends PeerTransportEvent

    /** The server rejected a `Hello` because the peer number violates the topology constraint (only
      * lower-numbered peers dial higher-numbered peers).
      */
    final case class ServerRejectedHello(remotePeerNum: Int, ownPeerNum: Int)
        extends PeerTransportEvent

    /** A `Msg` frame arrived on the server side before the peer sent its `Hello`. */
    case object ServerMsgBeforeHello extends PeerTransportEvent

    /** A frame on the server side could not be decoded. */
    final case class ServerDecodeError(cause: Throwable) extends PeerTransportEvent
