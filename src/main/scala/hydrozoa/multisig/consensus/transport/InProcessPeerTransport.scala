package hydrozoa.multisig.consensus.transport

import cats.effect.{IO, Ref}
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonHeadToHead}
import hydrozoa.multisig.consensus.peer.HeadPeerId

/** In-process [[PeerTransport]] for test harnesses (e.g. `integration/stage4`): routes sends
  * directly to the destination peer's local liaison actor, with no serialization.
  *
  * All transports in a multi-peer test share a single [[InProcessPeerTransport.Registry]] — peer
  * A's `send(B, msg)` looks up B's transport in the registry and pushes the request into the
  * inbound map B has populated via [[register]].
  */
final class InProcessPeerTransport private (
    val ownPeerId: HeadPeerId,
    private val inboundRef: Ref[IO, Map[HeadPeerId, PeerLiaisonHeadToHead.Handle]],
    private val registry: InProcessPeerTransport.Registry,
) extends PeerTransport {

    override def register(
        remote: HeadPeerId,
        localLiaison: PeerLiaisonHeadToHead.Handle
    ): IO[Unit] =
        inboundRef.update(_.updated(remote, localLiaison))

    override def send(
        remote: HeadPeerId,
        request: LiaisonProtocol.HeadToHeadRequest
    ): IO[Unit] =
        registry.get.flatMap { m =>
            m.get(remote) match {
                case Some(remoteTransport) => remoteTransport.dispatchInbound(ownPeerId, request)
                // Unregistered destination — a wiring bug. Silently drop for now; the test will
                // hang on whatever message was expected to flow, surfacing the misconfiguration.
                case None => IO.unit
            }
        }

    private def dispatchInbound(
        sender: HeadPeerId,
        payload: LiaisonProtocol.HeadToHeadRequest
    ): IO[Unit] =
        inboundRef.get.flatMap { m =>
            m.get(sender) match {
                case Some(liaison) => liaison ! payload
                case None          => IO.unit
            }
        }
}

object InProcessPeerTransport {

    /** Shared lookup table used by every peer's transport in one test scenario. The test builds one
      * of these via [[emptyRegistry]], then passes it to each peer's [[create]] call.
      */
    type Registry = Ref[IO, Map[HeadPeerId, InProcessPeerTransport]]

    def emptyRegistry: IO[Registry] = Ref[IO].of(Map.empty)

    /** Allocate this peer's transport and register it in the shared map under [[ownPeerId]]. */
    def create(ownPeerId: HeadPeerId, registry: Registry): IO[InProcessPeerTransport] =
        for {
            inboundRef <- Ref[IO].of(Map.empty[HeadPeerId, PeerLiaisonHeadToHead.Handle])
            transport = new InProcessPeerTransport(ownPeerId, inboundRef, registry)
            _ <- registry.update(_.updated(ownPeerId, transport))
        } yield transport
}
