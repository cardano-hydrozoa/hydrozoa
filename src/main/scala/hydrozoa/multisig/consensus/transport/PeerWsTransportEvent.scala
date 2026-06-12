package hydrozoa.multisig.consensus.transport

import hydrozoa.multisig.consensus.peer.HeadPeerId
import org.http4s.Uri

/** Typed events emitted by [[PeerWsTransport]]. Pure data; formatters in
  * [[PeerWsTransportEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait PeerWsTransportEvent

object PeerWsTransportEvent:

    // ---- send / dispatch ----

    /** `send` was called for a remote that has no outbox — wiring bug. */
    final case class NoOutboxForRemote(remote: HeadPeerId) extends PeerWsTransportEvent

    /** `send` was called with a request variant that cannot be serialised over the wire. */
    final case class DroppingNonWireRequest(remote: HeadPeerId) extends PeerWsTransportEvent

    /** An inbound frame arrived from a remote that has no registered local liaison. */
    final case class NoLiaisonForInbound(remote: HeadPeerId) extends PeerWsTransportEvent

    // ---- dialer (client side) ----

    /** A dialer successfully connected to a remote peer. */
    final case class DialerConnected(remote: HeadPeerId, uri: Uri) extends PeerWsTransportEvent

    /** A dialer attempt to a remote peer failed. */
    final case class DialerFailed(remote: HeadPeerId, cause: Throwable) extends PeerWsTransportEvent

    /** The dialer fiber for a remote peer was cancelled (resource release). */
    final case class DialerStopped(remote: HeadPeerId, uri: Uri) extends PeerWsTransportEvent

    /** A frame received on an active dialer connection could not be decoded. */
    final case class ClientDecodeError(remote: HeadPeerId, cause: Throwable)
        extends PeerWsTransportEvent

    // ---- server (accept side) ----

    /** The server accepted an inbound connection after receiving a valid `Hello`. */
    final case class ServerAccepted(remote: HeadPeerId) extends PeerWsTransportEvent

    /** The server rejected a `Hello` because the peer number violates the topology constraint (only
      * lower-numbered peers dial higher-numbered peers).
      */
    final case class ServerRejectedHello(remotePeerNum: Int, ownPeerNum: Int)
        extends PeerWsTransportEvent

    /** A `Msg` frame arrived on the server side before the peer sent its `Hello`. */
    case object ServerMsgBeforeHello extends PeerWsTransportEvent

    /** A frame on the server side could not be decoded. */
    final case class ServerDecodeError(cause: Throwable) extends PeerWsTransportEvent
