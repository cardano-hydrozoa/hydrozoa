package hydrozoa.multisig.consensus.transport

import cats.effect.IO
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonHeadToHead}
import hydrozoa.multisig.consensus.peer.HeadPeerId

/** Wraps a [[PeerTransport]] and drops outbound [[send]] calls when `shouldDrop` says so. Every
  * other method delegates unchanged. Emits [[FirewalledPeerTransportEvent.DroppedOutbound]] on
  * drop.
  */
final class FirewalledPeerTransport(
    underlying: PeerTransport,
    shouldDrop: HeadPeerId => IO[Boolean],
    firewallTracer: ContraTracer[IO, FirewalledPeerTransportEvent],
) extends PeerTransport:

    override def ownPeerId: HeadPeerId = underlying.ownPeerId

    override def register(
        remote: HeadPeerId,
        localLiaison: PeerLiaisonHeadToHead.Handle,
    ): IO[Unit] =
        underlying.register(remote, localLiaison)

    override def send(remote: HeadPeerId, request: LiaisonProtocol.HeadToHeadRequest): IO[Unit] =
        shouldDrop(remote).flatMap {
            case true =>
                firewallTracer.traceWith(FirewalledPeerTransportEvent.DroppedOutbound(remote))
            case false =>
                underlying.send(remote, request)
        }
