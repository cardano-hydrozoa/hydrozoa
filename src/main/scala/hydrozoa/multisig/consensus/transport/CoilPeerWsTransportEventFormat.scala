package hydrozoa.multisig.consensus.transport

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.peer.CoilPeerNumber
import hydrozoa.multisig.consensus.transport.CoilPeerWsTransportEvent.*

/** Renderers from [[CoilPeerWsTransportEvent]] to [[LogEvent]]. */
object CoilPeerWsTransportEventFormat:

    /** Routes under `CoilPeerWsTransport.c<ownCoilNum>` — one logger per coil peer's uplink. */
    def humanFormat(ownCoilNum: CoilPeerNumber)(e: CoilPeerWsTransportEvent): LogEvent = {
        val coilNum: Int = ownCoilNum.convert
        val ev = LogEvent.From(
          Map("coil" -> coilNum.toString),
          s"CoilPeerWsTransport.c$coilNum"
        )
        import ev.*
        e match {
            case DroppingNonWireRequest(request) =>
                warn(s"send: dropping non-wire request to hub: $request")
            case NoLiaisonForInbound =>
                warn("inbound from hub but no liaison registered")
            case UnexpectedInboundWire(payload) =>
                warn(s"unexpected coil-bound wire from hub: $payload")
            case DecodeError(cause) =>
                warn(s"failed to decode coil frame from hub: ${cause.getMessage}")
            case DialerConnected(uri) =>
                info(s"dialer: connected to hub at $uri")
            case DialerFailed(cause) =>
                // Class as well as message: getMessage is null for most connection exceptions,
                // which made a live incident undiagnosable from the logs.
                warn(s"dialer to hub failed: ${cause.getClass.getSimpleName}: ${cause.getMessage}")
            case DialerHandshakeStalled(uri, after) =>
                warn(
                  s"dialer: handshake to hub at $uri stalled past $after and was abandoned; " +
                      "the hub accepted the connection but never completed it — redialing"
                )
            case DialerDisconnected(uri) =>
                warn(s"dialer: disconnected from hub at $uri; redialing")
        }
    }
