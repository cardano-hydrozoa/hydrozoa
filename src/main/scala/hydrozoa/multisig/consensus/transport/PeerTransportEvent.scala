package hydrozoa.multisig.consensus.transport

import hydrozoa.multisig.consensus.peer.HeadPeerId
import org.http4s.Uri

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

    /** The dialer fiber for a remote peer was cancelled (resource release). */
    final case class DialerStopped(remote: HeadPeerId, uri: Uri) extends PeerTransportEvent

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
