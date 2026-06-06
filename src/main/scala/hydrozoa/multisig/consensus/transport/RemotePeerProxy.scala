package hydrozoa.multisig.consensus.transport

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.peer.HeadPeerId

/** A drop-in replacement for a remote head-mesh liaison handle that forwards wire-eligible batch
  * messages over a [[PeerWsTransport]].
  *
  * Local-only request variants (the appended artifacts, control ticks) are dropped — they're never
  * sent across the wire by the mesh liaison protocol, so reaching one indicates a wiring bug.
  */
final class RemotePeerProxy private (
    remote: HeadPeerId,
    transport: PeerWsTransport,
) extends Actor[IO, LiaisonProtocol.HeadToHeadRequest] {

    override def receive: Receive[IO, LiaisonProtocol.HeadToHeadRequest] =
        PartialFunction.fromFunction(req => transport.send(remote, req))
}

object RemotePeerProxy {
    def apply(remote: HeadPeerId, transport: PeerWsTransport): IO[RemotePeerProxy] =
        IO(new RemotePeerProxy(remote, transport))
}
