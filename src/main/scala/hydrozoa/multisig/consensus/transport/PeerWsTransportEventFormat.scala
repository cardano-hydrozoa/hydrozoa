package hydrozoa.multisig.consensus.transport

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.transport.PeerWsTransportEvent.*

/** Renderers from [[PeerWsTransportEvent]] to [[LogEvent]]. */
object PeerWsTransportEventFormat:

    /** Routes under `PeerWsTransport.<own>` — tune all transport events for one peer at once. */
    def humanFormat(own: HeadPeerNumber)(e: PeerWsTransportEvent): LogEvent = {
        val ownPn: Int = own
        val ev = LogEvent.From(Map("peer" -> ownPn.toString), s"PeerWsTransport.$ownPn")
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
                warn(s"dialer to remote=${remote.peerNum: Int} failed: ${cause.getMessage}")
            case DialerStopped(remote, uri) =>
                info(s"dialer: stopped for remote=${remote.peerNum: Int} at $uri")
            case ClientDecodeError(remote, cause) =>
                warn(
                  s"failed to decode frame from remote=${remote.peerNum: Int}: ${cause.getMessage}"
                )
            case ServerAccepted(remote) =>
                info(s"server: accepted inbound from remote=${remote.peerNum: Int}")
            case ServerRejectedHello(remotePeerNum, ownPeerNum) =>
                warn(
                  s"server: rejecting hello from peerNum=$remotePeerNum " +
                      s"(own=$ownPeerNum, must be lower)"
                )
            case ServerMsgBeforeHello =>
                warn("server: msg before hello, dropping")
            case ServerDecodeError(cause) =>
                warn(s"server: failed to decode frame: ${cause.getMessage}")
        }
    }
