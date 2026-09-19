package hydrozoa.multisig.consensus.transport

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import hydrozoa.multisig.consensus.liaison.BatchMessages.Join
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonCoilToHub, PeerLiaisonHubToCoil}
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
        hubInbound: Option[PeerLiaisonHubToCoil.Handle],
        coilInbound: Option[PeerLiaisonCoilToHub.Handle],
        /** Where a hub's answer to the join exchange lands. A coil reads its start point **before**
          * its actors exist, so the answer cannot be routed to `coilInbound` — there is nothing
          * there yet.
          */
        joinAnswer: Option[Deferred[IO, Join.Answer]],
    )

    object Endpoints:
        val empty: Endpoints = Endpoints(None, None, None)

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
            request: LiaisonProtocol.CoilRequestServed
        ): IO[Unit] =
            registry.get.flatMap { m =>
                val endpoints = m.get(coil)
                request match {
                    // The join answer goes to the coil's boot, not its liaison. `complete` has one
                    // winner, so a second answer falls through and is declined there.
                    case a @ (_: Join.Offer | _: Join.NoOffer) =>
                        endpoints.flatMap(_.joinAnswer) match {
                            case Some(d) =>
                                d.complete(a).flatMap(won => IO.unlessA(won)(toCoil(endpoints, a)))
                            case None => toCoil(endpoints, a)
                        }
                    case other => toCoil(endpoints, other)
                }
            }

        private def toCoil(
            endpoints: Option[Endpoints],
            request: LiaisonProtocol.CoilRequestServed
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
        marks: Ref[IO, Join.Connected],
        answer: Ref[IO, Deferred[IO, Join.Answer]]
    ) extends CoilTransport {
        override def register(localLiaison: PeerLiaisonCoilToHub.Handle): IO[Unit] =
            registry.update(m =>
                m.updated(
                  ownCoilNum,
                  m.getOrElse(ownCoilNum, Endpoints.empty).copy(coilInbound = Some(localLiaison))
                )
            )

        override def send(request: LiaisonProtocol.HubRequestServed): IO[Unit] =
            registry.get.flatMap { m =>
                m.get(ownCoilNum).flatMap(_.hubInbound) match {
                    case Some(liaison) => liaison ! request
                    case None          => IO.unit
                }
            }

        override def announceMarks(m: Join.Connected): IO[Unit] = marks.set(m)

        /** Run the join exchange over the direct link: hand the hub this coil's marks and wait for
          * its answer.
          *
          * The wait for a registered hub liaison is what a dialer does over a socket — a coil that
          * comes up before its hub retries until the hub is there. It is also why the marks must
          * already be set: they are what the hub decides on.
          */
        override def joinAnswer: IO[Join.Answer] =
            def announce: IO[Unit] =
                (registry.get, marks.get).flatMapN { (m, own) =>
                    m.get(ownCoilNum).flatMap(_.hubInbound) match {
                        case Some(hub) => hub ! own
                        case None      => IO.sleep(100.millis) >> announce
                    }
                }
            // A FRESH sink per exchange. The transport outlives the coil actors in a harness that
            // restarts them, and a `Deferred` fires once — reuse it and a restarted coil reads the
            // answer its predecessor got, silently, while looking like a successful exchange.
            for {
                fresh <- Deferred[IO, Join.Answer]
                _ <- answer.set(fresh)
                _ <- registry.update(m =>
                    m.updated(
                      ownCoilNum,
                      m.getOrElse(ownCoilNum, Endpoints.empty).copy(joinAnswer = Some(fresh))
                    )
                )
                _ <- announce
                a <- fresh.get
            } yield a
    }

    object Coil:
        def create(ownCoilNum: CoilPeerNumber, registry: Registry): IO[Coil] =
            for {
                marks <- Ref[IO].of(Join.Connected(None, None))
                first <- Deferred[IO, Join.Answer]
                answer <- Ref[IO].of(first)
            } yield new Coil(ownCoilNum, registry, marks, answer)
}
