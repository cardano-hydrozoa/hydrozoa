package hydrozoa.multisig.consensus.transport

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol

/** A drop-in replacement, on a coil peer, for its hub's
  * [[hydrozoa.multisig.consensus.liaison.PeerLiaisonHubToCoil]] handle: it forwards the
  * coil-emitted batch messages (`Population.Get` / `OwnHardAck.New`) over a
  * [[CoilPeerWsTransport]]. Local-only request variants are dropped by the transport's `send`.
  */
final class RemoteHubProxy private (
    uplink: CoilPeerWsTransport,
) extends Actor[IO, LiaisonProtocol.HubToCoilRequest] {

    override def receive: Receive[IO, LiaisonProtocol.HubToCoilRequest] =
        PartialFunction.fromFunction(req => uplink.send(req))
}

object RemoteHubProxy {
    def apply(uplink: CoilPeerWsTransport): IO[RemoteHubProxy] =
        IO(new RemoteHubProxy(uplink))
}
