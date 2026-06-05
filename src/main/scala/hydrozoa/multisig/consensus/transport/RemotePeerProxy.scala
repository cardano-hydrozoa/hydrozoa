package hydrozoa.multisig.consensus.transport

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.consensus.PeerLiaisonHeadToHead
import hydrozoa.multisig.consensus.peer.HeadPeerId

/** A drop-in replacement for a remote [[PeerLiaisonHeadToHead.Handle]] that forwards wire-eligible
  * requests over a [[PeerWsTransport]].
  *
  * Local-only request variants (RemoteBroadcast, BlockConfirmed, PreStart) are dropped with a log
  * line — they're never sent across the wire by the [[PeerLiaisonHeadToHead]] protocol, so reaching
  * one indicates a wiring bug.
  */
final class RemotePeerProxy private (
    remote: HeadPeerId,
    transport: PeerWsTransport,
) extends Actor[IO, PeerLiaisonHeadToHead.Request] {

    override def receive: Receive[IO, PeerLiaisonHeadToHead.Request] =
        PartialFunction.fromFunction(req => transport.send(remote, req))
}

object RemotePeerProxy {
    def apply(remote: HeadPeerId, transport: PeerWsTransport): IO[RemotePeerProxy] =
        IO(new RemotePeerProxy(remote, transport))
}
