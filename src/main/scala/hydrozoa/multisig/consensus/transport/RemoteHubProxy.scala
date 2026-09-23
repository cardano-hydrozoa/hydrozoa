package hydrozoa.multisig.consensus.transport

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol

/** A drop-in replacement, on a coil peer, for its hub's
  * [[hydrozoa.multisig.consensus.liaison.PeerLiaisonHubToCoil]] handle: it forwards everything a
  * coil may emit ([[LiaisonProtocol.CoilEmitted]]) over a [[CoilTransport]].
  *
  * It is typed at that send vocabulary rather than at the hub liaison's inbox, so a local-only
  * request cannot reach it and there is nothing for the transport to drop.
  */
final class RemoteHubProxy private (
    uplink: CoilTransport,
) extends Actor[IO, LiaisonProtocol.CoilEmitted] {

    override def receive: Receive[IO, LiaisonProtocol.CoilEmitted] =
        PartialFunction.fromFunction(req => uplink.send(req))
}

object RemoteHubProxy {
    def apply(uplink: CoilTransport): IO[RemoteHubProxy] =
        IO(new RemoteHubProxy(uplink))
}
