package hydrozoa.multisig.consensus.transport

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.consensus.PeerLiaison
import hydrozoa.multisig.consensus.peer.HeadPeerId

/** A drop-in replacement for a remote [[PeerLiaison.Handle]] that forwards wire-eligible requests
  * over a [[PeerWsTransport]].
  *
  * Local-only request variants (RemoteBroadcast, BlockConfirmed, PreStart) are dropped with a log
  * line — they're never sent across the wire by the [[PeerLiaison]] protocol, so reaching one
  * indicates a wiring bug.
  */
final class RemotePeerProxy private (
    remote: HeadPeerId,
    transport: PeerWsTransport,
) extends Actor[IO, PeerLiaison.Request] {

    override def receive: Receive[IO, PeerLiaison.Request] =
        PartialFunction.fromFunction(req => transport.send(remote, req))
}

object RemotePeerProxy {
    def apply(remote: HeadPeerId, transport: PeerWsTransport): IO[RemotePeerProxy] =
        IO(new RemotePeerProxy(remote, transport))
}
