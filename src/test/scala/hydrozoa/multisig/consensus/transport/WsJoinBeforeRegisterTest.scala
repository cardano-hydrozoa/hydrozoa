package hydrozoa.multisig.consensus.transport

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref, Resource}
import com.comcast.ip4s.{Port, host}
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.lib.actor.HydrozoaActorSystem
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.BatchMessages.{Join, OwnHardAck}
import hydrozoa.multisig.consensus.liaison.{BatchNumber, LiaisonProtocol}
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
  * answer indefinitely, so a join frame that finds no liaison is never replaced and the coil never
  * boots. A node registers its liaisons while it builds its actors, which finishes after its dialer
  * (coil) or its server (hub) is already up.
  *
  * The two ends answer that differently, because they can. The hub reads the position off a
  * handshake it is in the middle of processing, so it waits there until the liaison exists. The
  * coil receives the answer on an established socket with nothing to block, so it holds the answer
  * and hands it over on register.
  *
  * Registration is node-local — a `Ref` write naming the actor a link's inbound goes to — and is
  * invisible to the remote.
  */
class WsJoinBeforeRegisterTest extends AnyFunSuite {

    private val coil = CoilPeerNumber(1)

    /** Any coil-emitted frame; this test cares only that it crosses the link. */
    private val coilPush: OwnHardAck.New = OwnHardAck.New(BatchNumber.zero, None)

    /** Long enough that a link which was going to open would have. */
    private val settle: FiniteDuration = 2.seconds

    /** Short, so the "still waiting" notice lands inside [[settle]]. */
    private val reportAfter: FiniteDuration = 300.millis

    /** How long a frame is given to cross the loopback socket once the link may open. */
    private val deliveryBudget: FiniteDuration = 10.seconds

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
                        // transport holds it with no liaison to hand it to.
                        _ <- awaitMatch(hubSeen)(_ => true)
                            .flatMap(
                              IO.raiseUnless(_)(
                                new AssertionError("the hub never received the coil's position")
                              )
                            )
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
        assert(delivered.timeout(90.seconds).unsafeRunSync(), "the hub's answer was lost")
    }

    test("the hub holds a coil's position until its own liaison for that coil is registered") {
        val outcome = HydrozoaActorSystem.withoutRoot("ws-link-hub-side").use { system =>
            for {
                hubEvents <- Ref[IO].of(Vector.empty[HubWsTransportEvent])
                hub <- newHub(ContraTracer[IO, HubWsTransportEvent](e => hubEvents.update(_ :+ e)))
                coilTransport <- newCoil
                coilSeen <- Ref[IO].of(Vector.empty[LiaisonProtocol.FromHub])
                coilLiaison <- system.actorOf(new Recorder[LiaisonProtocol.FromHub](coilSeen))
                _ <- coilTransport.register(coilLiaison)
                result <- link(hub, coilTransport).use { _ =>
                    for {
                        // The coil dials and proves its number, so the hub accepts it — that
                        // verdict is about the proof and the roster, and nothing local changes
                        // it. What waits is the position the handshake carries.
                        accepted <- awaitMatch(hubEvents)(
                          _ == HubWsTransportEvent.ServerAccepted(1)
                        )
                        waited <- awaitMatch(hubEvents)(
                          _.isInstanceOf[HubWsTransportEvent.AwaitingLiaison]
                        )
                        hubSeen <- Ref[IO].of(Vector.empty[LiaisonProtocol.FromCoil])
                        hubLiaison <- system.actorOf(new AnsweringHub(hub, hubSeen))
                        _ <- hub.register(coil, hubLiaison)
                        positioned <- awaitMatch(hubSeen)(_.isInstanceOf[Join.Connected])
                        answered <- awaitMatch(coilSeen)(_.isInstanceOf[Join.NoOffer])
                    } yield (accepted, waited, positioned, answered)
                }
            } yield result
        }
        val (accepted, waited, positioned, answered) = outcome.timeout(90.seconds).unsafeRunSync()
        val _ = assert(accepted, "the hub never accepted the coil's handshake")
        val _ = assert(waited, "the hub never reported that it was holding the position")
        val _ =
            assert(positioned, "the coil's position never reached the liaison registered for it")
        assert(answered, "the coil never got its answer")
    }

    /** The invariant the hub's wait buys, stated positively: on an established hub link there is no
      * such thing as inbound with no liaison to take it. A coil that dials and starts talking
      * before the hub has registered its liaison has every frame delivered, in the order it sent
      * them and behind the position its handshake carried — never dropped, and never reported as
      * the wiring fault [[HubWsTransportEvent.NoLiaisonForInbound]] names.
      */
    test(
      "a coil's traffic before the hub registers its liaison is delivered in order, never faulted"
    ) {
        val outcome = HydrozoaActorSystem.withoutRoot("ws-join-hub-invariant").use { system =>
            for {
                hubEvents <- Ref[IO].of(Vector.empty[HubWsTransportEvent])
                hub <- newHub(ContraTracer[IO, HubWsTransportEvent](e => hubEvents.update(_ :+ e)))
                coilTransport <- newCoil
                coilLiaison <- system.actorOf(
                  new Recorder[LiaisonProtocol.FromHub](
                    Ref.unsafe[IO, Vector[LiaisonProtocol.FromHub]](Vector.empty)
                  )
                )
                _ <- coilTransport.register(coilLiaison)
                result <- link(hub, coilTransport).use { _ =>
                    for {
                        // Accepted, so the socket is live and the coil is free to talk; the hub
                        // still has no liaison for it.
                        accepted <- awaitMatch(hubEvents)(
                          _ == HubWsTransportEvent.ServerAccepted(1)
                        )
                        _ <- coilTransport.send(coilPush)
                        _ <- IO.sleep(settle)
                        faulted <- hubEvents.get.map(
                          _.exists(_.isInstanceOf[HubWsTransportEvent.NoLiaisonForInbound])
                        )
                        hubSeen <- Ref[IO].of(Vector.empty[LiaisonProtocol.FromCoil])
                        hubLiaison <- system.actorOf(
                          new Recorder[LiaisonProtocol.FromCoil](hubSeen)
                        )
                        _ <- hub.register(coil, hubLiaison)
                        arrived <- awaitMatch(hubSeen)(_ == coilPush)
                        order <- hubSeen.get
                    } yield (accepted, faulted, arrived, order)
                }
            } yield result
        }
        val (accepted, faulted, arrived, order) = outcome.timeout(90.seconds).unsafeRunSync()
        val _ = assert(accepted, "the hub never accepted the coil's handshake")
        val _ = assert(!faulted, s"a frame on a live link was reported unrouted: $order")
        val _ = assert(arrived, "the push sent before registration never reached the liaison")
        assert(
          order.indexWhere(_.isInstanceOf[Join.Connected]) < order.indexOf(coilPush),
          s"the push overtook the position its handshake carried: $order"
        )
    }

    private def newHub: IO[HubWsTransport] =
        newHub(ContraTracer[IO, HubWsTransportEvent](_ => IO.unit))

    private def newHub(tracer: ContraTracer[IO, HubWsTransportEvent]): IO[HubWsTransport] =
        HubWsTransport.create(
          List(coil),
          HandshakeFixture.coilPeers,
          HandshakeFixture.headParamsHash,
          HandshakeFixture.ownHead,
          tracer,
          liaisonWaitReport = reportAfter
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

    /** Whether an element matching `p` shows up within [[deliveryBudget]]. */
    private def awaitMatch[A](seen: Ref[IO, Vector[A]])(p: A => Boolean): IO[Boolean] = {
        def go: IO[Boolean] =
            seen.get.flatMap(v => if v.exists(p) then IO.pure(true) else IO.sleep(20.millis) >> go)
        go.timeoutTo(deliveryBudget, IO.pure(false))
    }
}
