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

    // ---- handshake ----

    /** The remote refused this peer's handshake and is closing the socket. `refusal` is the
      * operator's instruction: which of the two configs to go and fix. The dialer keeps redialing.
      */
    final case class DialerRefused(remote: HeadPeerId, refusal: HandshakeRefusal)
        extends PeerTransportEvent

    /** The remote sent no [[HeadFrame.Challenge]] within the budget, so this attempt was dropped. A
      * peer issues one as the first frame of an accepted socket, so this means it accepted the
      * connection and then said nothing.
      */
    final case class DialerNoChallenge(remote: HeadPeerId, uri: Uri, after: FiniteDuration)
        extends PeerTransportEvent

    /** A [[HeadFrame.Challenge]] arrived on an already-established link. One socket carries one
      * challenge, answered before the link opens; a second one is the remote misbehaving.
      */
    final case class DialerLateChallenge(remote: HeadPeerId) extends PeerTransportEvent

    /** This dialer refused the remote's [[HeadFrame.Challenge]] and dropped the socket without
      * answering it. The mirror of [[DialerRefused]], which is the remote refusing this peer: here
      * the verdict is this peer's own, so it holds even against a remote that would never have said
      * why.
      */
    final case class DialerRefusedChallenge(remote: HeadPeerId, refusal: HandshakeRefusal)
        extends PeerTransportEvent

    // ---- server (accept side) ----

    /** The server accepted an inbound connection whose `Handshake` proved its peer number. */
    final case class ServerAccepted(remote: HeadPeerId) extends PeerTransportEvent

    /** The server refused a `Handshake` and closed the socket. `refusal` says which of version,
      * dial topology, roster, head params, or proof did not hold — each a different thing for an
      * operator to fix.
      */
    final case class ServerRefusedHandshake(remotePeerNum: Int, refusal: HandshakeRefusal)
        extends PeerTransportEvent

    /** A second `Handshake` arrived on a socket that already has a verdict. One socket carries one
      * challenge and one handshake, so this is a peer misbehaving or a replay attempt; the socket
      * keeps the verdict it has.
      */
    final case class ServerRepeatHandshake(remotePeerNum: Int) extends PeerTransportEvent

    /** A `Msg` frame arrived on the server side before the peer's `Handshake` was accepted. */
    case object ServerMsgBeforeHandshake extends PeerTransportEvent

    /** A dialer sent a frame only the accept side ever sends — a `Challenge` or a `Refused`. */
    case object ServerUnexpectedFrame extends PeerTransportEvent

    /** A frame on the server side could not be decoded. */
    final case class ServerDecodeError(cause: Throwable) extends PeerTransportEvent
