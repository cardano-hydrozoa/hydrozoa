package hydrozoa.multisig.consensus.transport

import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref, Resource}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.BatchMessages.Join
import hydrozoa.multisig.consensus.peer.CoilPeerNumber
import hydrozoa.multisig.consensus.transport.CoilPeerWsTransportEvent.*
import org.http4s.Uri
import org.http4s.client.websocket.{WSClient, WSConnection, WSFrame, WSRequest}
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** What the dialer's handshake budget may and may not cut short, on a virtual clock.
  *
  * The budget exists because `JdkWSClient` builds its socket inside an uncancelable acquire, so a
  * hub that accepts the TCP connection and never answers can only be abandoned, never cancelled. It
  * must bound the handshake and nothing past it: an established link is *supposed* to occupy the
  * dialer for as long as it lives, so a budget that reaches beyond the handshake tears down working
  * connections on a timer and dials a second one on top of each — every frame then goes to
  * whichever of them the shared outbox happens to hand it to.
  */
class CoilDialerBudgetTest extends AnyFunSuite {

    private given CardanoNetwork.Section = CardanoNetwork.Preview

    /** A head identity for the fixtures; these suites are not about which head a peer is in. */
    private val ownHead: HeadIdentity =
        HeadIdentity(
          hydrozoa.config.head.initialization.InitializationParameters.HeadId(
            scalus.cardano.ledger.AssetName(scalus.uplc.builtin.ByteString.fromString("testhead"))
          ),
          scalus.cardano.ledger.Hash32.fromByteString(
            scalus.uplc.builtin.ByteString.fromArray(Array.fill[Byte](32)(0x11))
          )
        )

    private val hubUri = Uri.unsafeFromString("ws://hub.invalid:3001/ws")

    /** A quiet-but-live link: the hub's `Challenge` first, then a keep-alive Ping every
      * `pingEvery`, which is what holds `WsDuplex`'s read deadline off a real idle connection
      * (`NodeWsServer` pings every 10s).
      *
      * It closes after `pings` of them rather than running forever, only so `TestControl` has an
      * end to reach: an eternal link leaves work scheduled at every future instant and `tickAll`
      * chases it without terminating. Size it past the window under test.
      */
    private def pinging(
        pingEvery: FiniteDuration,
        pings: Int = 200,
        opening: CoilFrame.Challenge = CoilFrame.Challenge.own(HandshakeFixture.nonce)
    ): IO[WSConnection[IO]] =
        Ref.of[IO, Int](pings).map { left =>
            new WSConnection[IO] {
                private val challenge = WSFrame.Text(CoilFrame.encode(opening), last = true)
                override def send(wsf: WSFrame): IO[Unit] = IO.unit
                override def sendMany[G[_]: cats.Foldable, A <: WSFrame](wsfs: G[A]): IO[Unit] =
                    IO.unit
                override def receive: IO[Option[WSFrame]] =
                    left.modify(n => (n - 1, n)).flatMap {
                        // The hub's first frame on an accepted socket, and it arrives at once — the
                        // dialer has nothing to send until it does.
                        case n if n == pings => IO.pure(Some(challenge))
                        case n if n > 0      => IO.sleep(pingEvery).as(Some(WSFrame.Ping(empty)))
                        // End of stream: the peer closed its receiving side.
                        case _ => IO.sleep(pingEvery).as(None)
                    }
                override def subprotocol: Option[String] = None
            }
        }

    /** A hub that accepts the socket and issues no challenge — silent from the first frame on. */
    private def silent: IO[WSConnection[IO]] =
        IO.pure(new WSConnection[IO] {
            override def send(wsf: WSFrame): IO[Unit] = IO.unit
            override def sendMany[G[_]: cats.Foldable, A <: WSFrame](wsfs: G[A]): IO[Unit] = IO.unit
            override def receive: IO[Option[WSFrame]] = IO.never
            override def subprotocol: Option[String] = None
        })

    private val empty = scodec.bits.ByteVector.empty

    /** Run the real dialer against `connect` for `forHowLong` of virtual time, and report how many
      * dial attempts it made and what it traced.
      */
    private def dial(
        connect: Resource[IO, WSConnection[IO]],
        forHowLong: FiniteDuration
    ): (Int, Vector[CoilPeerWsTransportEvent]) = {
        val prog = for {
            attempts <- Ref.of[IO, Int](0)
            seen <- Ref.of[IO, Vector[CoilPeerWsTransportEvent]](Vector.empty)
            tracer = ContraTracer[IO, CoilPeerWsTransportEvent](e => seen.update(_ :+ e))
            transport <- CoilPeerWsTransport.create(
              CoilPeerNumber(0),
              HandshakeFixture.coilWallet(0),
              HandshakeFixture.headParamsHash,
              IO.pure(Join.Connected(None, None)),
              ownHead,
              tracer
            )
            client = WSClient[IO](respondToPings = false) { (_: WSRequest) =>
                Resource.eval(attempts.update(_ + 1)).flatMap(_ => connect)
            }
            _ <- transport.startDialer(client, hubUri).surround(IO.sleep(forHowLong))
            n <- attempts.get
            events <- seen.get
        } yield (n, events)
        TestControl.executeEmbed(prog).unsafeRunSync()(using cats.effect.unsafe.implicits.global)
    }

    private def stalls(events: Vector[CoilPeerWsTransportEvent]): Int =
        events.count(_.isInstanceOf[DialerHandshakeStalled])

    test("a live link holds the dialer: no redial and no stall for as long as it stays up") {
        // Ten minutes is twenty budgets. The link is quiet but not silent, exactly as a real one is
        // between blocks, so nothing about it should ever look like a stalled handshake.
        val (attempts, events) = dial(Resource.eval(pinging(10.seconds)), 10.minutes)
        assert(
          (attempts, stalls(events)) == (1, 0),
          "the link never dropped, so the dialer must neither redial nor report a stall; " +
              s"dialed $attempts times, traced $events"
        )
    }

    test("a handshake that never completes is abandoned, and the dialer keeps redialing") {
        // 30s budget + the 1s inter-attempt delay, so 65s covers three attempts and two expiries.
        val (attempts, events) = dial(Resource.eval(IO.never[WSConnection[IO]]), 65.seconds)
        assert(
          (attempts, stalls(events)) == (3, 2),
          "expected a redial per expired budget, each announced; " +
              s"dialed $attempts times, traced $events"
        )
    }

    test("a hub that issues no challenge is given up on, and the dialer keeps redialing") {
        // The challenge budget is 10s and the inter-attempt delay 1s, so 25s covers three attempts
        // and two expiries. This must be the *challenge* budget that ends each attempt, not the
        // 30s handshake budget: an attempt that outlived the loop's claim would sit in `receive`
        // holding an open socket while the loop had already dialed another.
        val (attempts, events) = dial(Resource.eval(silent), 25.seconds)
        assert(
          (
            attempts,
            events.count(_.isInstanceOf[DialerNoChallenge]),
            stalls(events),
            events.count(_.isInstanceOf[DialerConnected])
          ) == (3, 2, 0, 0),
          "expected a redial per challenge budget, each announced, and no handshake stall; " +
              s"dialed $attempts times, traced $events"
        )
    }

    test("a hub announcing another protocol version is refused before the coil answers") {
        // The link would hold if the coil answered — `pinging` keeps it alive — so nothing but the
        // version check can be what ends each attempt. The coil decides for itself here rather
        // than waiting for a `Refused` the hub may be too old to send.
        val other =
            CoilFrame.Challenge(HandshakeFixture.nonce, Some(ProtocolVersion.current + 1))
        val (attempts, events) =
            dial(Resource.eval(pinging(10.seconds, opening = other)), 5.seconds)
        // `attempts` is counted when the socket connects, the refusal when it is traced, so the
        // window can close with the last attempt's refusal still in flight. One short is the race,
        // not a coil that answered: an answered challenge would show as `DialerConnected`.
        val refusals = events.count(_.isInstanceOf[DialerRefusedChallenge])
        assert(
          (
            attempts > 1,
            refusals >= attempts - 1 && refusals <= attempts,
            events.count(_.isInstanceOf[DialerConnected]),
            stalls(events)
          ) == (true, true, 0, 0),
          "every attempt must refuse the challenge and none may open a link; " +
              s"dialed $attempts times, refused $refusals, traced $events"
        )
    }

    test("a hub announcing no protocol version is refused the same way a mismatch is") {
        val silentVersion = CoilFrame.Challenge(HandshakeFixture.nonce, None)
        val (_, events) =
            dial(Resource.eval(pinging(10.seconds, opening = silentVersion)), 5.seconds)
        val refusals = events.collect { case DialerRefusedChallenge(r) => r }
        assert(
          refusals.nonEmpty && refusals.forall(
            _ == HandshakeRefusal.ProtocolVersionMismatch(None, ProtocolVersion.current)
          ),
          s"expected every refusal to name the absent version; traced $events"
        )
    }

    test(
      "a handshake landing after the budget drops its socket rather than opening a second link"
    ) {
        // The deadline case the `handshook` claim exists for: the hub answers at 35s, five seconds
        // after the loop gave up and one attempt into the redial. Exactly one of the two may win,
        // so the late attempt must disown itself and never reach `DialerConnected`.
        val late = Resource.eval(IO.sleep(35.seconds) >> pinging(10.seconds, pings = 2))
        val (attempts, events) = dial(late, 40.seconds)
        assert(
          (
            attempts,
            stalls(events),
            events.count(_.isInstanceOf[DialerHandshakeLate]),
            events.count(_.isInstanceOf[DialerConnected])
          ) == (2, 1, 1, 0),
          "the budget expired once and the late handshake disowned itself; " +
              s"dialed $attempts times, traced $events"
        )
    }
}
