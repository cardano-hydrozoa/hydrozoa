package hydrozoa.multisig.consensus.transport

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref, Resource}
import com.comcast.ip4s.{Port, host}
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.lib.actor.HydrozoaActorSystem
import hydrozoa.lib.logging.{ContraTracer, Level}
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.ack.{HardAckNumber, HubHardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.{Mesh, OwnHardAck}
import hydrozoa.multisig.consensus.liaison.{BatchNumber, LiaisonProtocol}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.consensus.transport.HandshakeFixture.given
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import org.http4s.Uri
import org.http4s.jdkhttpclient.JdkWSClient
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** A transport's inbound after its local liaison was unregistered, over real WebSocket links.
  *
  * At the handoff to the rule-based regime a node stops its liaisons, while the peers at the other
  * end of each link — which hand off on their own, when they observe the fallback on L1 — keep
  * pulling. The node unregisters each liaison before stopping it, so the transport drops those
  * frames as expected (DEBUG) instead of delivering them to a stopped actor (a WARN dead letter). A
  * frame on a link that never had a liaison is still a fault, and stays a WARN.
  */
class TransportUnregisterTest extends AnyFunSuite {

    private val coil = CoilPeerNumber(1)
    private val nPeers: PositiveInt = PositiveInt.unsafeApply(2)
    private val head0 = HeadPeerId(HeadPeerNumber(0), nPeers)
    private val head1 = HeadPeerId(HeadPeerNumber(1), nPeers)

    /** How long a frame is given to cross the loopback socket and be traced. */
    private val budget: FiniteDuration = 5.seconds

    private val quietServerTracer = ContraTracer[IO, NodeWsServerEvent](_ => IO.unit)

    private val meshPull: LiaisonProtocol.MeshEmitted = Mesh.Get(
      batchNum = BatchNumber.zero,
      block = BlockNumber(1),
      stack = StackNumber(1),
      request = RequestNumber.zero,
      requestCeiling = RequestNumber.zero,
      softAck = SoftAckNumber.zero,
      headHardAck = HardAckNumber.zero,
      hubHardAck = HubHardAckNumber.zero
    )

    private val hubPull: OwnHardAck.Get = OwnHardAck.Get(BatchNumber.zero, HardAckNumber.zero)

    private val coilPush: OwnHardAck.New = OwnHardAck.New(BatchNumber.zero, None)

    private class Recorder[M](seen: Ref[IO, Vector[M]]) extends Actor[IO, M] {
        override def receive: Receive[IO, M] =
            PartialFunction.fromFunction(m => seen.update(_ :+ m))
    }

    // ---- head mesh: WsPeerTransport ----

    test("mesh: inbound after unregister is dropped as expected, not delivered") {
        val run = HydrozoaActorSystem.withoutRoot("unregister-mesh").use { system =>
            for {
                seen <- Ref[IO].of(Vector.empty[PeerTransportEvent])
                acceptor <- newMeshPeer(1, head0, seen)
                dialer <- newMeshPeer(
                  0,
                  head1,
                  Ref.unsafe[IO, Vector[PeerTransportEvent]](Vector.empty)
                )
                got <- Ref[IO].of(Vector.empty[LiaisonProtocol.MeshEmitted])
                liaison <- system.actorOf(new Recorder[LiaisonProtocol.MeshEmitted](got))
                _ <- acceptor.register(head0, liaison)
                out <- meshLink(acceptor, dialer).use { _ =>
                    for {
                        _ <- dialer.send(head1, meshPull)
                        _ <- await(got)(_.nonEmpty, "the registered liaison never got the pull")
                        _ <- acceptor.unregister(head0)
                        _ <- dialer.send(head1, meshPull)
                        _ <- await(seen)(
                          _.contains(PeerTransportEvent.InboundAfterUnregister(head0)),
                          "the pull after unregister was not dropped as expected"
                        )
                        delivered <- got.get
                        events <- seen.get
                    } yield (delivered.size, events)
                }
            } yield out
        }
        val (delivered, events) = run.timeout(60.seconds).unsafeRunSync()
        val _ = assert(delivered == 1, "the pull after unregister reached the stopped liaison")
        assert(
          !events.exists(_.isInstanceOf[PeerTransportEvent.NoLiaisonForInbound]),
          s"an unregistered link was reported as never registered: $events"
        )
    }

    test("mesh: inbound on a link that never had a liaison is still a fault") {
        val events = for {
            seen <- Ref[IO].of(Vector.empty[PeerTransportEvent])
            acceptor <- newMeshPeer(1, head0, seen)
            dialer <- newMeshPeer(
              0,
              head1,
              Ref.unsafe[IO, Vector[PeerTransportEvent]](Vector.empty)
            )
            events <- meshLink(acceptor, dialer).use { _ =>
                dialer.send(head1, meshPull) >>
                    await(seen)(
                      _.contains(PeerTransportEvent.NoLiaisonForInbound(head0)),
                      "a pull with no liaison ever registered was not reported"
                    ) >> seen.get
            }
        } yield events
        assert(
          !events
              .timeout(60.seconds)
              .unsafeRunSync()
              .exists(_.isInstanceOf[PeerTransportEvent.InboundAfterUnregister])
        )
    }

    // ---- hub side: HubWsTransport ----

    test("hub: a coil's inbound after unregister is dropped as expected, not delivered") {
        val run = HydrozoaActorSystem.withoutRoot("unregister-hub").use { system =>
            for {
                seen <- Ref[IO].of(Vector.empty[HubWsTransportEvent])
                hub <- newHub(seen)
                coilT <- newCoil(Ref.unsafe[IO, Vector[CoilPeerWsTransportEvent]](Vector.empty))
                got <- Ref[IO].of(Vector.empty[LiaisonProtocol.FromCoil])
                liaison <- system.actorOf(new Recorder[LiaisonProtocol.FromCoil](got))
                _ <- hub.register(coil, liaison)
                out <- hubLink(hub, coilT).use { _ =>
                    for {
                        _ <- coilT.send(coilPush)
                        _ <- await(got)(_.contains(coilPush), "the hub liaison never got the push")
                        _ <- hub.unregister(coil)
                        _ <- coilT.send(coilPush)
                        _ <- await(seen)(
                          _.contains(HubWsTransportEvent.InboundAfterUnregister(coil)),
                          "the push after unregister was not dropped as expected"
                        )
                        delivered <- got.get
                        events <- seen.get
                    } yield (delivered.count(_ == coilPush), events)
                }
            } yield out
        }
        val (delivered, events) = run.timeout(60.seconds).unsafeRunSync()
        val _ = assert(delivered == 1, "the push after unregister reached the stopped liaison")
        assert(
          !events.exists(_.isInstanceOf[HubWsTransportEvent.NoLiaisonForInbound]),
          s"an unregistered link was reported as never registered: $events"
        )
    }

    test("hub: a coil's inbound with no liaison ever registered is still a fault") {
        val events = for {
            seen <- Ref[IO].of(Vector.empty[HubWsTransportEvent])
            hub <- newHub(seen)
            coilT <- newCoil(Ref.unsafe[IO, Vector[CoilPeerWsTransportEvent]](Vector.empty))
            events <- hubLink(hub, coilT).use { _ =>
                coilT.send(coilPush) >>
                    await(seen)(
                      _.contains(HubWsTransportEvent.NoLiaisonForInbound(coil)),
                      "a push with no liaison ever registered was not reported"
                    ) >> seen.get
            }
        } yield events
        assert(
          !events
              .timeout(60.seconds)
              .unsafeRunSync()
              .exists(_.isInstanceOf[HubWsTransportEvent.InboundAfterUnregister])
        )
    }

    // ---- coil side: CoilPeerWsTransport ----

    test("coil: the hub's inbound after unregister is dropped as expected, not delivered") {
        val run = HydrozoaActorSystem.withoutRoot("unregister-coil").use { system =>
            for {
                seen <- Ref[IO].of(Vector.empty[CoilPeerWsTransportEvent])
                hub <- newHub(Ref.unsafe[IO, Vector[HubWsTransportEvent]](Vector.empty))
                coilT <- newCoil(seen)
                got <- Ref[IO].of(Vector.empty[LiaisonProtocol.FromHub])
                liaison <- system.actorOf(new Recorder[LiaisonProtocol.FromHub](got))
                _ <- coilT.register(liaison)
                out <- hubLink(hub, coilT).use { _ =>
                    for {
                        _ <- hub.send(coil, hubPull)
                        _ <- await(got)(_.contains(hubPull), "the coil liaison never got the pull")
                        _ <- coilT.unregister
                        _ <- hub.send(coil, hubPull)
                        _ <- await(seen)(
                          _.contains(CoilPeerWsTransportEvent.InboundAfterUnregister),
                          "the pull after unregister was not dropped as expected"
                        )
                        delivered <- got.get
                        events <- seen.get
                    } yield (delivered.count(_ == hubPull), events)
                }
            } yield out
        }
        val (delivered, events) = run.timeout(60.seconds).unsafeRunSync()
        val _ = assert(delivered == 1, "the pull after unregister reached the stopped liaison")
        assert(
          !events.contains(CoilPeerWsTransportEvent.NoLiaisonForInbound),
          s"an unregistered link was reported as never registered: $events"
        )
    }

    test("coil: the hub's inbound with no liaison ever registered is still a fault") {
        val events = for {
            seen <- Ref[IO].of(Vector.empty[CoilPeerWsTransportEvent])
            hub <- newHub(Ref.unsafe[IO, Vector[HubWsTransportEvent]](Vector.empty))
            coilT <- newCoil(seen)
            events <- hubLink(hub, coilT).use { _ =>
                hub.send(coil, hubPull) >>
                    await(seen)(
                      _.contains(CoilPeerWsTransportEvent.NoLiaisonForInbound),
                      "a pull with no liaison ever registered was not reported"
                    ) >> seen.get
            }
        } yield events
        assert(
          !events
              .timeout(60.seconds)
              .unsafeRunSync()
              .contains(CoilPeerWsTransportEvent.InboundAfterUnregister)
        )
    }

    // ---- levels: expected drops are DEBUG, faults stay WARN ----

    test("a drop after unregister logs at DEBUG; inbound with no liaison ever registered at WARN") {
        val mesh = PeerTransportEventFormat.humanFormat(HeadPeerNumber(1))
        val hub = HubWsTransportEventFormat.humanFormat(HeadPeerNumber(0))
        val coilF = CoilPeerWsTransportEventFormat.humanFormat(coil)
        val levels = List(
          mesh(PeerTransportEvent.InboundAfterUnregister(head0)).level -> Level.Debug,
          mesh(PeerTransportEvent.NoLiaisonForInbound(head0)).level -> Level.Warn,
          mesh(PeerTransportEvent.LiaisonUnregistered(head0)).level -> Level.Info,
          hub(HubWsTransportEvent.InboundAfterUnregister(coil)).level -> Level.Debug,
          hub(HubWsTransportEvent.NoLiaisonForInbound(coil)).level -> Level.Warn,
          hub(HubWsTransportEvent.LiaisonUnregistered(coil)).level -> Level.Info,
          coilF(CoilPeerWsTransportEvent.InboundAfterUnregister).level -> Level.Debug,
          coilF(CoilPeerWsTransportEvent.NoLiaisonForInbound).level -> Level.Warn,
          coilF(CoilPeerWsTransportEvent.LiaisonUnregistered).level -> Level.Info,
        )
        assert(levels.forall(_ == _), levels)
    }

    // ---- fixtures ----

    private def newMeshPeer(
        own: Int,
        remote: HeadPeerId,
        seen: Ref[IO, Vector[PeerTransportEvent]]
    ): IO[WsPeerTransport] =
        WsPeerTransport.create(
          HeadPeerId(HeadPeerNumber(own), nPeers),
          HandshakeFixture.headWallet(own),
          HandshakeFixture.headPeers,
          HandshakeFixture.headParamsHash,
          HandshakeFixture.ownHead,
          List(remote),
          ContraTracer[IO, PeerTransportEvent](e => seen.update(_ :+ e))
        )

    /** Head peer 1 accepting on a bound server, head peer 0 dialing it (lower dials higher). */
    private def meshLink(acceptor: WsPeerTransport, dialer: WsPeerTransport): Resource[IO, Unit] =
        for {
            server <- NodeWsServer.resource(
              host"127.0.0.1",
              Port.fromInt(0).get,
              List(wsb => acceptor.routes(wsb)),
              quietServerTracer
            )
            uri = Uri.unsafeFromString(s"ws://127.0.0.1:${server.address.getPort}/head")
            client <- Resource.eval(JdkWSClient.simple[IO])
            _ <- dialer.startDialers(client, Map(head1 -> uri))
        } yield ()

    private def newHub(seen: Ref[IO, Vector[HubWsTransportEvent]]): IO[HubWsTransport] =
        HubWsTransport.create(
          List(coil),
          HandshakeFixture.coilPeers,
          HandshakeFixture.headParamsHash,
          HandshakeFixture.ownHead,
          ContraTracer[IO, HubWsTransportEvent](e => seen.update(_ :+ e))
        )

    private def newCoil(seen: Ref[IO, Vector[CoilPeerWsTransportEvent]]): IO[CoilPeerWsTransport] =
        CoilPeerWsTransport.create(
          coil,
          HandshakeFixture.coilWallet(1),
          HandshakeFixture.headParamsHash,
          IO.pure(HandshakeFixture.marks),
          HandshakeFixture.ownHead,
          ContraTracer[IO, CoilPeerWsTransportEvent](e => seen.update(_ :+ e))
        )

    /** The hub's route on a bound server, with the coil dialing it. */
    private def hubLink(hub: HubWsTransport, coilT: CoilPeerWsTransport): Resource[IO, Unit] =
        for {
            server <- NodeWsServer.resource(
              host"127.0.0.1",
              Port.fromInt(0).get,
              List(wsb => hub.routes(wsb)),
              quietServerTracer
            )
            uri = Uri.unsafeFromString(s"ws://127.0.0.1:${server.address.getPort}/hub")
            client <- Resource.eval(JdkWSClient.simple[IO])
            _ <- coilT.startDialer(client, uri)
        } yield ()

    /** Fail unless `p` holds of `seen` within [[budget]]. */
    private def await[A](
        seen: Ref[IO, Vector[A]]
    )(p: Vector[A] => Boolean, failure: String): IO[Unit] = {
        def go: IO[Unit] =
            seen.get.flatMap(v => if p(v) then IO.unit else IO.sleep(20.millis) >> go)
        go.timeoutTo(budget, IO.raiseError(new AssertionError(failure)))
    }
}
