package hydrozoa.multisig.consensus.transport

import cats.effect.{IO, Ref}
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonCoilToHub, PeerLiaisonHubToCoil}
import hydrozoa.multisig.consensus.peer.CoilPeerNumber

/** In-process pair of [[HubTransport]] / [[CoilTransport]] for test harnesses (e.g.
  * `integration/stage4`): hub-coil sends route directly to the destination's local liaison actor
  * with no serialization.
  *
  * All transports in a multi-peer test share a single [[InProcessHubCoilTransport.Registry]].
  * Hub-side [[InProcessHubCoilTransport.Hub]] writes its per-coil hub→coil liaisons into
  * `hubInbound`; each coil's [[InProcessHubCoilTransport.Coil]] writes its single coil→hub liaison
  * into `coilInbound`. Sends look up the other end of the link by `CoilPeerNumber`.
  */
object InProcessHubCoilTransport {

    /** Per-coil endpoints. Both sides are filled in independently as the hub MRM and the coil MRM
      * each spawn and register their liaisons.
      */
    final case class Endpoints(
        hubInbound: Option[PeerLiaisonHubToCoil.Handle],
        coilInbound: Option[PeerLiaisonCoilToHub.Handle],
    )

    object Endpoints:
        val empty: Endpoints = Endpoints(None, None)

    /** Shared lookup table used by both ends of every hub↔coil link in one test scenario. The test
      * builds one of these via [[emptyRegistry]], then passes it to each hub's [[Hub.create]] and
      * each coil's [[Coil.create]].
      */
    type Registry = Ref[IO, Map[CoilPeerNumber, Endpoints]]

    def emptyRegistry: IO[Registry] = Ref[IO].of(Map.empty)

    /** Hub-side transport: one per hub head peer, talks to every coil it hubs. */
    final class Hub private (registry: Registry) extends HubTransport {
        override def register(
            coil: CoilPeerNumber,
            localLiaison: PeerLiaisonHubToCoil.Handle
        ): IO[Unit] =
            registry.update(m =>
                m.updated(
                  coil,
                  m.getOrElse(coil, Endpoints.empty).copy(hubInbound = Some(localLiaison))
                )
            )

        override def send(
            coil: CoilPeerNumber,
            request: LiaisonProtocol.CoilToHubRequest
        ): IO[Unit] =
            registry.get.flatMap { m =>
                m.get(coil).flatMap(_.coilInbound) match {
                    case Some(liaison) => liaison ! request
                    // Unregistered destination — a wiring bug. Silently drop; the test will hang on
                    // whatever message was expected to flow, surfacing the misconfiguration. Same
                    // policy as InProcessPeerTransport.
                    case None => IO.unit
                }
            }
    }

    object Hub:
        def create(registry: Registry): IO[Hub] = IO(new Hub(registry))

    /** Coil-side transport: one per coil peer, talks to its single hub. */
    final class Coil private (ownCoilNum: CoilPeerNumber, registry: Registry)
        extends CoilTransport {
        override def register(localLiaison: PeerLiaisonCoilToHub.Handle): IO[Unit] =
            registry.update(m =>
                m.updated(
                  ownCoilNum,
                  m.getOrElse(ownCoilNum, Endpoints.empty).copy(coilInbound = Some(localLiaison))
                )
            )

        override def send(request: LiaisonProtocol.HubToCoilRequest): IO[Unit] =
            registry.get.flatMap { m =>
                m.get(ownCoilNum).flatMap(_.hubInbound) match {
                    case Some(liaison) => liaison ! request
                    case None          => IO.unit
                }
            }
    }

    object Coil:
        def create(ownCoilNum: CoilPeerNumber, registry: Registry): IO[Coil] =
            IO(new Coil(ownCoilNum, registry))
}
