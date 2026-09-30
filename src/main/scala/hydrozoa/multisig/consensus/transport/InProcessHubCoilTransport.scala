package hydrozoa.multisig.consensus.transport

import cats.effect.{IO, Ref}
import hydrozoa.multisig.consensus.liaison.BatchMessages.Join
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.peer.CoilPeerNumber
import scala.concurrent.duration.DurationInt

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
        hubInbound: Option[LiaisonProtocol.HubLiaisonHandle],
        coilInbound: Option[LiaisonProtocol.CoilLiaisonHandle],
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
            localLiaison: LiaisonProtocol.HubLiaisonHandle
        ): IO[Unit] =
            registry.update(m =>
                m.updated(
                  coil,
                  m.getOrElse(coil, Endpoints.empty).copy(hubInbound = Some(localLiaison))
                )
            )

        /** Clears the hub end: sends to it are then dropped silently, as for an unregistered one.
          */
        override def unregister(coil: CoilPeerNumber): IO[Unit] =
            registry.update(m => m.updatedWith(coil)(_.map(_.copy(hubInbound = None))))

        override def send(
            coil: CoilPeerNumber,
            request: Join.Answer | LiaisonProtocol.HubEmitted
        ): IO[Unit] =
            registry.get.flatMap { m =>
                val endpoints = m.get(coil)
                // Everything a hub emits, the answer included, goes to the coil liaison: it is in
                // join mode waiting for exactly that.
                toCoil(endpoints, request)
            }

        private def toCoil(
            endpoints: Option[Endpoints],
            request: Join.Answer | LiaisonProtocol.HubEmitted
        ): IO[Unit] =
            endpoints.flatMap(_.coilInbound) match {
                case Some(liaison) => liaison ! request
                // Unregistered destination — a wiring bug. Silently drop; the test will hang on
                // whatever message was expected to flow, surfacing the misconfiguration. Same
                // policy as InProcessPeerTransport.
                case None => IO.unit
            }
    }

    object Hub:
        def create(registry: Registry): IO[Hub] = IO(new Hub(registry))

    /** Coil-side transport: one per coil peer, talks to its single hub. */
    final class Coil private (
        ownCoilNum: CoilPeerNumber,
        registry: Registry,
        marks: Ref[IO, Join.Connected]
    ) extends CoilTransport {
        override def register(localLiaison: LiaisonProtocol.CoilLiaisonHandle): IO[Unit] =
            registry.update(m =>
                m.updated(
                  ownCoilNum,
                  m.getOrElse(ownCoilNum, Endpoints.empty).copy(coilInbound = Some(localLiaison))
                )
            )

        /** Clears the coil end: sends to it are then dropped silently, as for an unregistered one.
          */
        override def unregister: IO[Unit] =
            registry.update(m => m.updatedWith(ownCoilNum)(_.map(_.copy(coilInbound = None))))

        override def send(request: LiaisonProtocol.CoilEmitted): IO[Unit] =
            registry.get.flatMap { m =>
                m.get(ownCoilNum).flatMap(_.hubInbound) match {
                    case Some(liaison) => liaison ! request
                    case None          => IO.unit
                }
            }

        /** Hand the hub this coil's marks, once both ends of the link have a liaison.
          *
          * The same rule the WebSocket transports enforce by not opening a socket: nothing crosses
          * a link until there is somewhere to deliver on each end. The hub answers a position once,
          * to whatever this registry says `coilInbound` is at that moment, so announcing before
          * this coil has registered strands the answer and the join never ends.
          *
          * Forked, because the caller is the coil liaison's join mode and it must stay free to
          * receive the answer this announcement provokes.
          */
        override def announceMarks(m: Join.Connected): IO[Unit] =
            def announce: IO[Unit] =
                registry.get.flatMap { reg =>
                    val ends = reg.get(ownCoilNum)
                    (ends.flatMap(_.hubInbound), ends.flatMap(_.coilInbound)) match {
                        case (Some(hub), Some(_)) => hub ! m
                        case _                    => IO.sleep(100.millis) >> announce
                    }
                }
            marks.set(m) >> announce.start.void
    }

    object Coil:
        def create(ownCoilNum: CoilPeerNumber, registry: Registry): IO[Coil] =
            Ref[IO]
                .of(Join.Connected(None, None))
                .map(marks => new Coil(ownCoilNum, registry, marks))
}
