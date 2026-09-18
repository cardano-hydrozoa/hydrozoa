package hydrozoa.multisig.consensus.transport

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.transport.HubWsTransportEvent.*

/** Renderers from [[HubWsTransportEvent]] to [[LogEvent]]. */
object HubWsTransportEventFormat:

    /** Routes under `HubWsTransport.<hubPeerNum>` — one logger per hub head peer. */
    def humanFormat(hubPeerNum: HeadPeerNumber)(e: HubWsTransportEvent): LogEvent = {
        val hubPn: Int = hubPeerNum
        val ev = LogEvent.From(Map("peer" -> hubPn.toString), s"HubWsTransport.$hubPn")
        import ev.*
        e match {
            case NoOutboxForCoil(coil) =>
                warn(s"send: no outbox for coil=${coil.convert}")
            case DroppingNonWireRequest(coil, request) =>
                warn(s"send: dropping non-wire request to coil=${coil.convert}: $request")
            case NoLiaisonForInbound(coil) =>
                warn(s"inbound from coil=${coil.convert} but no liaison registered")
            case UnexpectedInboundWire(coil, payload) =>
                warn(s"unexpected hub-bound wire from coil=${coil.convert}: $payload")
            case ServerAccepted(coilNum) =>
                info(s"coil server: accepted inbound from coil=$coilNum")
            case ServerRefusedHandshake(coilNum, refusal) =>
                warn(
                  s"coil server: refusing coil=$coilNum and closing the socket — " +
                      HandshakeRefusal.describe(refusal)
                )
            case ServerRepeatHandshake(coilNum) =>
                warn(
                  s"coil server: a second handshake from coil=$coilNum on a socket that already " +
                      "has a verdict; keeping the verdict"
                )
            case ServerMsgBeforeHandshake =>
                warn("coil server: msg before an accepted handshake, dropping")
            case ServerUnexpectedFrame =>
                warn("coil server: a coil sent a hub-only frame, dropping")
            case ServerDecodeError(cause) =>
                warn(s"coil server: failed to decode frame: ${cause.getMessage}")
        }
    }
