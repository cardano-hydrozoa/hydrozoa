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

    /** An inbound frame arrived from a coil peer that has no registered local liaison. */
    final case class NoLiaisonForInbound(coil: CoilPeerNumber) extends HubWsTransportEvent

    /** Received an inbound wire payload from a coil peer that is not in the coil-emitted subset. */
    final case class UnexpectedInboundWire(coil: CoilPeerNumber, payload: CoilFrame.Wire)
        extends HubWsTransportEvent

    // ---- server (accept side) ----

    /** The server accepted an inbound connection from a coil whose `Handshake` proved its number.
      */
    final case class ServerAccepted(coilNum: Int) extends HubWsTransportEvent

    /** The server refused a `Handshake` and closed the socket. `refusal` says which of version,
      * roster, head params, or proof did not hold — each a different thing for an operator to fix.
      */
    final case class ServerRefusedHandshake(coilNum: Int, refusal: HandshakeRefusal)
        extends HubWsTransportEvent

    /** A `Msg` frame arrived on the server side before the coil peer sent its `Handshake`. */
    /** A second `Handshake` arrived on a socket that already has a verdict. One socket carries one
      * challenge and one handshake, so this is a coil misbehaving or a replay attempt; the socket
      * keeps the verdict it has.
      */
    final case class ServerRepeatHandshake(coilNum: Int) extends HubWsTransportEvent

    /** A `Msg` frame arrived on the server side before the coil peer's `Handshake` was accepted. */
    case object ServerMsgBeforeHandshake extends HubWsTransportEvent

    /** A coil sent a frame only the hub ever sends — a `Challenge` or a `Refused`. */
    case object ServerUnexpectedFrame extends HubWsTransportEvent

    /** A frame on the server side could not be decoded. */
    final case class ServerDecodeError(cause: Throwable) extends HubWsTransportEvent
