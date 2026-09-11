package hydrozoa.multisig.consensus.transport

import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref, Resource}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.consensus.transport.PeerTransportEvent.*
import org.http4s.Uri
import org.http4s.client.websocket.{WSClient, WSConnection, WSFrame, WSRequest}
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** The head-mesh mirror of [[CoilDialerBudgetTest]].
  *
  * `WsPeerTransport.dialerLoop` and `CoilPeerWsTransport.dialerLoop` are the same code twice over,
  * and the budget defect was in both — so the fix needs a witness on both. Everything the coil
  * suite pins is pinned here against `WsPeerTransport` instead: a live link must occupy the dialer
  * for as long as it lives, a handshake that never lands must be abandoned and redialed, and one
  * that lands after the budget must drop its socket rather than leave a second connection draining
  * this remote's outbox.
  */
class HeadDialerBudgetTest extends AnyFunSuite {

    private given CardanoNetwork.Section = CardanoNetwork.Preview

    private val nPeers = PositiveInt.unsafeApply(2)
    private val ownId = HeadPeerId(HeadPeerNumber(0), nPeers)
    // Lower peerNum dials higher, so peer 0 is the dialer and peer 1 the remote.
    private val remoteId = HeadPeerId(HeadPeerNumber(1), nPeers)
    private val remoteUri = Uri.unsafeFromString("ws://peer1.invalid:3001/head")

    /** A quiet-but-live link: a keep-alive Ping every `pingEvery`, which is what holds `WsDuplex`'s
      * read deadline off a real idle connection. Ends after `pings` so `TestControl` has an end to
      * reach.
      */
    private def pinging(pingEvery: FiniteDuration, pings: Int = 200): IO[WSConnection[IO]] =
        Ref.of[IO, Int](pings).map { left =>
            new WSConnection[IO] {
                override def send(wsf: WSFrame): IO[Unit] = IO.unit
                override def sendMany[G[_]: cats.Foldable, A <: WSFrame](wsfs: G[A]): IO[Unit] =
                    IO.unit
                override def receive: IO[Option[WSFrame]] =
                    IO.sleep(pingEvery) >> left.modify(n => (n - 1, n)).map {
                        case n if n > 0 => Some(WSFrame.Ping(scodec.bits.ByteVector.empty))
                        case _          => None
                    }
                override def subprotocol: Option[String] = None
            }
        }

    private def dial(
        connect: Resource[IO, WSConnection[IO]],
        forHowLong: FiniteDuration
    ): (Int, Vector[PeerTransportEvent]) = {
        val prog = for {
            attempts <- Ref.of[IO, Int](0)
            seen <- Ref.of[IO, Vector[PeerTransportEvent]](Vector.empty)
            tracer = ContraTracer[IO, PeerTransportEvent](e => seen.update(_ :+ e))
            transport <- WsPeerTransport.create(ownId, List(remoteId), tracer)
            client = WSClient[IO](respondToPings = false) { (_: WSRequest) =>
                Resource.eval(attempts.update(_ + 1)).flatMap(_ => connect)
            }
            _ <- transport
                .startDialers(client, Map(remoteId -> remoteUri))
                .surround(IO.sleep(forHowLong))
            n <- attempts.get
            events <- seen.get
        } yield (n, events)
        TestControl.executeEmbed(prog).unsafeRunSync()(using cats.effect.unsafe.implicits.global)
    }

    private def stalls(events: Vector[PeerTransportEvent]): Int =
        events.count(_.isInstanceOf[DialerHandshakeStalled])

    test("a live link holds the head dialer: no redial and no stall for as long as it stays up") {
        val (attempts, events) = dial(Resource.eval(pinging(10.seconds)), 10.minutes)
        assert(
          (attempts, stalls(events)) == (1, 0),
          "the link never dropped, so the dialer must neither redial nor report a stall; " +
              s"dialed $attempts times, traced $events"
        )
    }

    test("a head handshake that never completes is abandoned, and the dialer keeps redialing") {
        val (attempts, events) = dial(Resource.eval(IO.never[WSConnection[IO]]), 65.seconds)
        assert(
          (attempts, stalls(events)) == (3, 2),
          "expected a redial per expired budget, each announced; " +
              s"dialed $attempts times, traced $events"
        )
    }

    test("a head handshake landing after the budget drops its socket rather than opening a link") {
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
