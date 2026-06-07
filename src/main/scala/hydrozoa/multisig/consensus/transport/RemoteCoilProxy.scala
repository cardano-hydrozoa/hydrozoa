package hydrozoa.multisig.consensus.transport

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.peer.CoilPeerNumber

/** A drop-in replacement, on a hub head peer, for one coil peer's
  * [[hydrozoa.multisig.consensus.liaison.PeerLiaisonCoilToHub]] handle: it forwards the hub-emitted
  * batch messages (`Population.New` / `OwnHardAck.Get`) over a [[CoilHubTransport]] to the bound
  * coil peer. Local-only request variants are dropped by the transport's `send`.
  */
final class RemoteCoilProxy private (
    coil: CoilPeerNumber,
    transport: CoilHubTransport,
) extends Actor[IO, LiaisonProtocol.CoilToHubRequest] {

    override def receive: Receive[IO, LiaisonProtocol.CoilToHubRequest] =
        PartialFunction.fromFunction(req => transport.send(coil, req))
}

object RemoteCoilProxy {
    def apply(coil: CoilPeerNumber, transport: CoilHubTransport): IO[RemoteCoilProxy] =
        IO(new RemoteCoilProxy(coil, transport))
}
