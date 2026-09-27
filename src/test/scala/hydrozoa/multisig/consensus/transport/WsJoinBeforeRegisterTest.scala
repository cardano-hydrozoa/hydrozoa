package hydrozoa.multisig.consensus.transport

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref, Resource}
import com.comcast.ip4s.{Port, host}
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.lib.actor.HydrozoaActorSystem
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.BatchMessages.Join
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.peer.CoilPeerNumber
import hydrozoa.multisig.consensus.transport.HandshakeFixture.given
import org.http4s.Uri
import org.http4s.jdkhttpclient.JdkWSClient
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** The join exchange over a real WebSocket link when a frame of it arrives before the liaison that
  * owns it has registered on its transport.
  *
  * Each side of the exchange is sent once per dial: the coil's position rides its handshake, and
  * the hub answers it once. Neither is resent on a healthy link, and a cold coil waits for the
  * answer indefinitely. So a join frame the transport drops for want of a registered liaison is
  * never replaced, and the coil never boots. A node registers its liaisons while it builds its
  * actors, which can finish after its dialer (coil) or its server (hub) is already up.
  */
class WsJoinBeforeRegisterTest extends AnyFunSuite {

    private val coil = CoilPeerNumber(1)

    /** Time for a frame the other side has already sent to cross the loopback socket. */
    private val settle: FiniteDuration = 1.second

    /** How long a registered liaison is given to receive the held frame. */
    private val deliveryBudget: FiniteDuration = 5.seconds

    private val quietServerTracer = ContraTracer[IO, NodeWsServerEvent](_ => IO.unit)

    private class Recorder[M](seen: Ref[IO, Vector[M]]) extends Actor[IO, M] {
        override def receive: Receive[IO, M] =
            PartialFunction.fromFunction(m => seen.update(_ :+ m))
    }

    /** A hub liaison that answers every position it is handed with `NoOffer`, as a hub with no
      * settlement to seed from does.
      */
    private class AnsweringHub(hub: HubTransport, seen: Ref[IO, Vector[LiaisonProtocol.FromCoil]])
        extends Actor[IO, LiaisonProtocol.FromCoil] {
        override def receive: Receive[IO, LiaisonProtocol.FromCoil] =
            PartialFunction.fromFunction {
                case c: Join.Connected =>
                    seen.update(_ :+ c) >> hub.send(coil, Join.NoOffer("nothing to seed from"))
                case other => seen.update(_ :+ other)
            }
    }

    test(
      "a hub answer that reaches the coil before its liaison registers is delivered on register"
    ) {
        val delivered = HydrozoaActorSystem.withoutRoot("ws-join-coil-side").use { system =>
            for {
                hub <- newHub
                hubSeen <- Ref[IO].of(Vector.empty[LiaisonProtocol.FromCoil])
                hubLiaison <- system.actorOf(new AnsweringHub(hub, hubSeen))
                _ <- hub.register(coil, hubLiaison)
                coilTransport <- newCoil
                got <- link(hub, coilTransport).use { _ =>
                    for {
                        // The hub has answered, and the answer has had time to arrive: the coil
                        // transport has it and no liaison to hand it to.
                        _ <- awaitNonEmpty(hubSeen, "the hub never received the coil's position")
                        _ <- IO.sleep(settle)
                        coilSeen <- Ref[IO].of(Vector.empty[LiaisonProtocol.FromHub])
                        coilLiaison <- system.actorOf(
                          new Recorder[LiaisonProtocol.FromHub](coilSeen)
                        )
                        _ <- coilTransport.register(coilLiaison)
                        got <- awaitMatch(coilSeen)(_.isInstanceOf[Join.NoOffer])
                    } yield got
                }
            } yield got
        }
        assert(delivered.timeout(60.seconds).unsafeRunSync(), "the hub's answer was lost")
    }

    test(
      "a coil position that reaches the hub before its liaison registers is delivered on register"
    ) {
        val delivered = HydrozoaActorSystem.withoutRoot("ws-join-hub-side").use { system =>
            for {
                hubEvents <- Ref[IO].of(Vector.empty[HubWsTransportEvent])
                hub <- newHub(ContraTracer[IO, HubWsTransportEvent](e => hubEvents.update(_ :+ e)))
                coilTransport <- newCoil
                coilSeen <- Ref[IO].of(Vector.empty[LiaisonProtocol.FromHub])
                coilLiaison <- system.actorOf(new Recorder[LiaisonProtocol.FromHub](coilSeen))
                _ <- coilTransport.register(coilLiaison)
                got <- link(hub, coilTransport).use { _ =>
                    for {
                        // The hub accepted the handshake, which carries the position: it has it and
                        // no liaison to hand it to.
                        _ <- awaitMatch(hubEvents)(_ == HubWsTransportEvent.ServerAccepted(1))
                            .flatMap(
                              IO.raiseUnless(_)(new AssertionError("the hub never accepted"))
                            )
                        _ <- IO.sleep(settle)
                        hubSeen <- Ref[IO].of(Vector.empty[LiaisonProtocol.FromCoil])
                        hubLiaison <- system.actorOf(
                          new Recorder[LiaisonProtocol.FromCoil](hubSeen)
                        )
                        _ <- hub.register(coil, hubLiaison)
                        got <- awaitMatch(hubSeen)(_.isInstanceOf[Join.Connected])
                    } yield got
                }
            } yield got
        }
        assert(delivered.timeout(60.seconds).unsafeRunSync(), "the coil's position was lost")
    }

    private def newHub: IO[HubWsTransport] =
        newHub(ContraTracer[IO, HubWsTransportEvent](_ => IO.unit))

    private def newHub(tracer: ContraTracer[IO, HubWsTransportEvent]): IO[HubWsTransport] =
        HubWsTransport.create(
          List(coil),
          HandshakeFixture.coilPeers,
          HandshakeFixture.headParamsHash,
          HandshakeFixture.ownHead,
          tracer
        )

    private def newCoil: IO[CoilPeerWsTransport] =
        CoilPeerWsTransport.create(
          coil,
          HandshakeFixture.coilWallet(1),
          HandshakeFixture.headParamsHash,
          IO.pure(HandshakeFixture.marks),
          HandshakeFixture.ownHead,
          ContraTracer[IO, CoilPeerWsTransportEvent](_ => IO.unit)
        )

    /** The hub's route on a bound server, with the coil dialing it. */
    private def link(hub: HubWsTransport, coilTransport: CoilPeerWsTransport): Resource[IO, Unit] =
        for {
            server <- NodeWsServer.resource(
              host"127.0.0.1",
              Port.fromInt(0).get,
              List(wsb => hub.routes(wsb)),
              quietServerTracer
            )
            uri = Uri.unsafeFromString(s"ws://127.0.0.1:${server.address.getPort}/hub")
            client <- Resource.eval(JdkWSClient.simple[IO])
            _ <- coilTransport.startDialer(client, uri)
        } yield ()

    private def awaitNonEmpty[A](seen: Ref[IO, Vector[A]], failure: String): IO[Unit] =
        awaitMatch(seen)(_ => true).flatMap(IO.raiseUnless(_)(new AssertionError(failure)))

    /** Whether an element matching `p` shows up within [[deliveryBudget]]. */
    private def awaitMatch[A](seen: Ref[IO, Vector[A]])(p: A => Boolean): IO[Boolean] = {
        def go: IO[Boolean] =
            seen.get.flatMap(v => if v.exists(p) then IO.pure(true) else IO.sleep(20.millis) >> go)
        go.timeoutTo(deliveryBudget, IO.pure(false))
    }
}
