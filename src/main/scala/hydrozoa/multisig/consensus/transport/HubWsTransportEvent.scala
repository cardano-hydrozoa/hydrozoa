package hydrozoa.multisig.consensus.transport

import hydrozoa.multisig.consensus.peer.CoilPeerNumber

/** Typed events emitted by [[HubWsTransport]]. Pure data; formatters in
  * [[HubWsTransportEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait HubWsTransportEvent

object HubWsTransportEvent:

    // ---- send / dispatch ----

    /** `send` was called for a coil peer that has no outbox — wiring bug. */
    final case class NoOutboxForCoil(coil: CoilPeerNumber) extends HubWsTransportEvent

    /** `send` was called with a request variant that cannot be serialised over the wire. */
    final case class DroppingNonWireRequest(coil: CoilPeerNumber, request: String)
        extends HubWsTransportEvent

    /** An inbound frame arrived from a coil peer that has no registered local liaison. */
    final case class NoLiaisonForInbound(coil: CoilPeerNumber) extends HubWsTransportEvent

    /** Received an inbound wire payload from a coil peer that is not in the coil-emitted subset. */
    final case class UnexpectedInboundWire(coil: CoilPeerNumber, payload: String)
        extends HubWsTransportEvent

    // ---- server (accept side) ----

    /** The server accepted an inbound connection from a coil after receiving a valid `Hello`. */
    final case class ServerAccepted(coilNum: Int) extends HubWsTransportEvent

    /** The server rejected a `Hello` from an unknown coil peer number. */
    final case class ServerRejectedHello(coilNum: Int) extends HubWsTransportEvent

    /** A `Msg` frame arrived on the server side before the coil peer sent its `Hello`. */
    case object ServerMsgBeforeHello extends HubWsTransportEvent

    /** A frame on the server side could not be decoded. */
    final case class ServerDecodeError(cause: Throwable) extends HubWsTransportEvent
