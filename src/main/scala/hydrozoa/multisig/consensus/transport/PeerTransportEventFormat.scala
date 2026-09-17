package hydrozoa.multisig.consensus.transport

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.transport.PeerTransportEvent.*

/** Renderers from [[PeerTransportEvent]] to [[LogEvent]]. */
object PeerTransportEventFormat:

    /** Routes under `PeerTransport.<own>` — tune all transport events for one peer at once. */
    def humanFormat(own: HeadPeerNumber)(e: PeerTransportEvent): LogEvent = {
        val ownPn: Int = own
        val ev = LogEvent.From(Map("peer" -> ownPn.toString), s"PeerTransport.$ownPn")
        import ev.*
        e match {
            case NoOutboxForRemote(remote) =>
                warn(s"send: no outbox for remote=${remote.peerNum: Int}")
            case DroppingNonWireRequest(remote) =>
                warn(s"send: dropping non-wire request to remote=${remote.peerNum: Int}")
            case NoLiaisonForInbound(remote) =>
                warn(s"inbound from remote=${remote.peerNum: Int} but no liaison registered")
            case DialerConnected(remote, uri) =>
                info(s"dialer: connected to remote=${remote.peerNum: Int} at $uri")
            case DialerFailed(remote, cause) =>
                // Class as well as message: getMessage is null for most connection exceptions.
                warn(
                  s"dialer to remote=${remote.peerNum: Int} failed: " +
                      s"${cause.getClass.getSimpleName}: ${cause.getMessage}"
                )
            case DialerDisconnected(remote, uri) =>
                warn(s"dialer: disconnected from remote=${remote.peerNum: Int} at $uri; redialing")
            case DialerStopped(remote, uri) =>
                info(s"dialer: stopped for remote=${remote.peerNum: Int} at $uri")
            case DialerHandshakeStalled(remote, uri, after) =>
                warn(
                  s"dialer: handshake to remote=${remote.peerNum: Int} at $uri stalled past " +
                      s"$after and was abandoned; the remote accepted the connection but never " +
                      "completed it — redialing"
                )
            case DialerHandshakeLate(remote, uri) =>
                warn(
                  s"dialer: handshake to remote=${remote.peerNum: Int} at $uri completed after " +
                      "it was abandoned; dropping the socket"
                )
            case ClientDecodeError(remote, cause) =>
                warn(
                  s"failed to decode frame from remote=${remote.peerNum: Int}: ${cause.getMessage}"
                )
            case ServerAccepted(remote) =>
                info(s"server: accepted inbound from remote=${remote.peerNum: Int}")
            case ServerRejectedHandshake(remotePeerNum, ownPeerNum) =>
                warn(
                  s"server: rejecting handshake from peerNum=$remotePeerNum " +
                      s"(own=$ownPeerNum, must be lower)"
                )
            case ServerRejectedHeadIdentity(remotePeerNum, detail) =>
                warn(s"server: rejecting peerNum=$remotePeerNum — $detail")
            case ServerRejectedProtocolVersion(remotePeerNum, found, expected) =>
                warn(
                  s"server: rejecting peerNum=$remotePeerNum — protocol version " +
                      s"${ProtocolVersion.describe(found)}, this node speaks $expected"
                )
            case ServerMsgBeforeHandshake =>
                warn("server: msg before handshake, dropping")
            case ServerDecodeError(cause) =>
                warn(s"server: failed to decode frame: ${cause.getMessage}")
        }
    }
