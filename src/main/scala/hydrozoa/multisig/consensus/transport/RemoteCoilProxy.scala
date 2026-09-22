package hydrozoa.multisig.consensus.transport

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.peer.CoilPeerNumber

/** A drop-in replacement, on a hub head peer, for one coil peer's
  * [[hydrozoa.multisig.consensus.liaison.PeerLiaisonCoilToHub]] handle: it forwards everything a
  * hub may emit ([[LiaisonProtocol.HubEmitted]]) over a [[HubTransport]] to the bound coil peer.
  *
  * It is typed at that send vocabulary rather than at the coil liaison's inbox, so a local-only
  * request cannot reach it and there is nothing for the transport to drop.
  */
final class RemoteCoilProxy private (
    coil: CoilPeerNumber,
    transport: HubTransport,
) extends Actor[IO, LiaisonProtocol.HubEmitted] {

    override def receive: Receive[IO, LiaisonProtocol.HubEmitted] =
        PartialFunction.fromFunction(req => transport.send(coil, req))
}

object RemoteCoilProxy {
    def apply(coil: CoilPeerNumber, transport: HubTransport): IO[RemoteCoilProxy] =
        IO(new RemoteCoilProxy(coil, transport))
}
