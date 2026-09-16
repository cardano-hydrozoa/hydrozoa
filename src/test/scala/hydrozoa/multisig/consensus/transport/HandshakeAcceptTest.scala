package hydrozoa.multisig.consensus.transport

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref, Resource}
import com.comcast.ip4s.{Port, host}
import fs2.Stream
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber, PeerWallet}
import hydrozoa.multisig.consensus.transport.HandshakeFixture.given
import org.http4s.client.websocket.{WSConnection, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.{HttpRoutes, Uri}
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** What the accept side of a liaison link does with a handshake, over a real Ember server and a
  * real WebSocket client.
  *
  * The frames are driven by hand rather than through the dialers, because the properties under test
  * are about the **wire**: that the challenge is the server's first frame, that a refusal is named
  * before the socket closes rather than left to an idle timeout, and that a signature made for
  * something else does not open a session. A test driving a dialer would only ever send well-formed
  * handshakes, and could not express the second half of any of those.
  */
class HandshakeAcceptTest extends AnyFunSuite {

    private val nPeers = PositiveInt.unsafeApply(2)

    /** How long an accepted link is watched before the test concludes nothing else is coming. An
      * accepted link stays open by design, so there is no close to wait for.
      */
    private val quietWindow: FiniteDuration = 2.seconds

    private val quietServerTracer = ContraTracer[IO, NodeWsServerEvent](_ => IO.unit)

    // ---- the hub link ----

    test("the hub's first frame is a challenge, before the coil has said anything") {
        val (opening, _, _) = runHub(_ => None)
        assert(
          CoilFrame.parse(opening).exists(_.isInstanceOf[CoilFrame.Challenge]),
          s"expected a challenge as the hub's opening frame, got: $opening"
        )
    }

    test("a coil that proves its number is accepted, and is sent no refusal") {
        val (_, replies, events) = runHub(answerAsCoil(1, HandshakeFixture.coilWallet(1)))
        val _ = assert(
          events.contains(HubWsTransportEvent.ServerAccepted(1)),
          s"expected the hub to accept coil 1; traced $events"
        )
        assert(refusalsOnWire(replies).isEmpty, s"an accepted coil must not be refused: $replies")
    }

    test("a stranger signing as coil 1 is refused for a bad signature, and the socket closes") {
        assertHubRefuses(
          answerAsCoil(1, HandshakeFixture.strangerWallet),
          HandshakeRefusal.BadSignature
        )
    }

    test("a coil replaying a proof made for another nonce is refused") {
        // The replay the challenge exists to stop: a well-formed handshake, correctly signed by the
        // real coil 1 — over a different socket's nonce.
        assertHubRefuses(
          _ =>
              Some(
                CoilFrame.encode(
                  CoilFrame.Handshake.own(
                    1,
                    HandshakeFixture.coilWallet(1),
                    HandshakeFixture.headParamsHash,
                    HandshakeFixture.otherNonce
                  )
                )
              ),
          HandshakeRefusal.BadSignature
        )
    }

    test("a coil this hub does not hub is refused before its proof is looked at") {
        assertHubRefuses(
          answerAsCoil(7, HandshakeFixture.strangerWallet),
          HandshakeRefusal.NotHubbed(7)
        )
    }

    test("a coil on another head is refused for its head params") {
        assertHubRefuses(
          opening =>
              hubNonce(opening).map(nonce =>
                  CoilFrame.encode(
                    CoilFrame.Handshake.own(
                      1,
                      HandshakeFixture.coilWallet(1),
                      HandshakeFixture.otherHeadParamsHash,
                      nonce
                    )
                  )
              ),
          HandshakeRefusal.HeadParamsMismatch(
            HandshakeFixture.otherHeadParamsHash,
            HandshakeFixture.headParamsHash
          )
        )
    }

    test("a coil speaking another protocol version is refused before its proof is looked at") {
        // The proof is coil 1's own and good for this socket. The version is checked first, so it
        // is never reached — which is the point: a peer speaking another protocol may not mean the
        // same thing by its own number.
        val theirs = ProtocolVersion.current + 1
        assertHubRefuses(
          opening =>
              hubNonce(opening).map { nonce =>
                  val auth = HandshakeProof.sign(
                    HandshakeFixture.coilWallet(1),
                    HandshakeProof.Link.CoilToHub,
                    claimant = 1,
                    theirs,
                    HandshakeFixture.headParamsHash,
                    nonce
                  )
                  CoilFrame.encode(CoilFrame.Handshake(1, Some(theirs), auth))
              },
          HandshakeRefusal.ProtocolVersionMismatch(Some(theirs), ProtocolVersion.current)
        )
    }

    // ---- the head mesh ----

    test("a head peer that proves its number is accepted on the mesh") {
        val (_, _, events) = runMesh(answerAsHead(0, HandshakeFixture.headWallet(0)))
        assert(
          events.exists {
              case PeerTransportEvent.ServerAccepted(remote) => (remote.peerNum: Int) == 0
              case _                                         => false
          },
          s"expected the mesh to accept peer 0; traced $events"
        )
    }

    test("a stranger signing as head peer 0 is refused for a bad signature") {
        assertMeshRefuses(
          answerAsHead(0, HandshakeFixture.strangerWallet),
          HandshakeRefusal.BadSignature
        )
    }

    test("a proof made for the hub link does not open a mesh link") {
        // What binding the link into the preimage buys. Everything else about this handshake is
        // genuine: head peer 0's own key, its own number, this very socket's nonce — and a proof
        // made for the `/hub` lane.
        assertMeshRefuses(
          opening =>
              meshNonce(opening).map { nonce =>
                  val auth = HandshakeProof.sign(
                    HandshakeFixture.headWallet(0),
                    HandshakeProof.Link.CoilToHub,
                    claimant = 0,
                    ProtocolVersion.current,
                    HandshakeFixture.headParamsHash,
                    nonce
                  )
                  HeadFrame.encode(HeadFrame.Handshake(0, Some(ProtocolVersion.current), auth))
              },
          HandshakeRefusal.BadSignature
        )
    }

    test("a peer dialing against the mesh topology is refused, proof or no proof") {
        assertMeshRefuses(
          answerAsHead(1, HandshakeFixture.headWallet(1)),
          HandshakeRefusal.WrongDialDirection(1, 1)
        )
    }

    // ---- driving one exchange ----

    /** Connect, read the server's opening frame, answer it with `reply`, and collect every text
      * frame that comes back until the server closes or [[quietWindow]] passes.
      *
      * `reply` sees the raw opening line, so a test can build a handshake against the nonce it
      * actually carries — or deliberately against a different one.
      */
    private def exchange(uri: Uri)(reply: String => Option[String]): IO[(String, List[String])] =
        JdkWSClient.simple[IO].flatMap { client =>
            client.connect(WSRequest(uri)).use { conn =>
                for {
                    opening <- firstText(conn)
                    _ <- reply(opening).fold(IO.unit)(l => conn.send(WSFrame.Text(l)))
                    rest <- collectUntilClose(conn)
                } yield (opening, rest)
            }
        }

    private def firstText(conn: WSConnection[IO]): IO[String] =
        Stream
            .repeatEval(conn.receive)
            .unNoneTerminate
            .collect { case WSFrame.Text(text, true) => text }
            .head
            .compile
            .lastOrError

    private def collectUntilClose(conn: WSConnection[IO]): IO[List[String]] =
        Stream
            .repeatEval(conn.receive)
            .unNoneTerminate
            .takeWhile {
                case _: WSFrame.Close => false
                case _                => true
            }
            .collect { case WSFrame.Text(text, true) => text }
            .interruptAfter(quietWindow)
            .compile
            .toList

    /** One hub exchange: the opening frame, the frames that followed, and what the hub traced. */
    private def runHub(
        reply: String => Option[String]
    ): (String, List[String], Vector[HubWsTransportEvent]) = {
        val prog = for {
            seen <- Ref[IO].of(Vector.empty[HubWsTransportEvent])
            tracer = ContraTracer[IO, HubWsTransportEvent](e => seen.update(_ :+ e))
            result <- hubLink(tracer).use { uri => exchange(uri)(reply) }
            events <- seen.get
        } yield (result._1, result._2, events)
        prog.timeout(30.seconds).unsafeRunSync()
    }

    private def runMesh(
        reply: String => Option[String]
    ): (String, List[String], Vector[PeerTransportEvent]) = {
        val prog = for {
            seen <- Ref[IO].of(Vector.empty[PeerTransportEvent])
            tracer = ContraTracer[IO, PeerTransportEvent](e => seen.update(_ :+ e))
            result <- meshLink(tracer).use { uri => exchange(uri)(reply) }
            events <- seen.get
        } yield (result._1, result._2, events)
        prog.timeout(30.seconds).unsafeRunSync()
    }

    /** The hub route on a bound server, hubbing coil peers 0 and 1. */
    private def hubLink(tracer: ContraTracer[IO, HubWsTransportEvent]): Resource[IO, Uri] =
        Resource
            .eval(
              HubWsTransport.create(
                List(CoilPeerNumber(0), CoilPeerNumber(1)),
                HandshakeFixture.coilPeers,
                HandshakeFixture.headParamsHash,
                tracer
              )
            )
            .flatMap(hub => bind(wsb => hub.routes(wsb), "hub"))

    /** The mesh route on a bound server, owned by head peer 1 — so head peer 0 may dial it. */
    private def meshLink(tracer: ContraTracer[IO, PeerTransportEvent]): Resource[IO, Uri] =
        Resource
            .eval(
              WsPeerTransport.create(
                HeadPeerId(HeadPeerNumber(1), nPeers),
                HandshakeFixture.headWallet(1),
                HandshakeFixture.headPeers,
                HandshakeFixture.headParamsHash,
                List(HeadPeerId(HeadPeerNumber(0), nPeers)),
                tracer
              )
            )
            .flatMap(peer => bind(wsb => peer.routes(wsb), "head"))

    private def bind(
        route: WebSocketBuilder2[IO] => HttpRoutes[IO],
        path: String
    ): Resource[IO, Uri] =
        NodeWsServer
            .resource(host"127.0.0.1", Port.fromInt(0).get, List(route), quietServerTracer)
            .map(server => Uri.unsafeFromString(s"ws://127.0.0.1:${server.address.getPort}/$path"))

    // ---- assertions and frame builders ----

    /** The hub named exactly `expected` on the wire and then closed, and traced the same thing.
      * Both halves matter: a refusal nobody is told about reads as a network fault, and a refusal
      * nobody logged leaves an operator nothing to act on.
      */
    private def assertHubRefuses(
        reply: String => Option[String],
        expected: HandshakeRefusal
    ): Assertion = {
        val (_, replies, events) = runHub(reply)
        val _ = assert(
          refusalsOnWire(replies) == List(expected),
          s"expected one $expected on the wire; got ${refusalsOnWire(replies)} " +
              s"(frames: $replies, traced $events)"
        )
        assert(
          events.exists {
              case HubWsTransportEvent.ServerRefusedHandshake(_, r) => r == expected
              case _                                                => false
          },
          s"the refusal must also be traced; traced $events"
        )
    }

    private def assertMeshRefuses(
        reply: String => Option[String],
        expected: HandshakeRefusal
    ): Assertion = {
        val (_, replies, events) = runMesh(reply)
        val onWire = replies.flatMap(l => HeadFrame.parse(l).toOption).collect {
            case HeadFrame.Refused(r) => r
        }
        val _ = assert(
          onWire == List(expected),
          s"expected one $expected on the wire; got $onWire (frames: $replies, traced $events)"
        )
        assert(
          events.exists {
              case PeerTransportEvent.ServerRefusedHandshake(_, r) => r == expected
              case _                                               => false
          },
          s"the refusal must also be traced; traced $events"
        )
    }

    private def refusalsOnWire(replies: List[String]): List[HandshakeRefusal] =
        replies.flatMap(l => CoilFrame.parse(l).toOption).collect { case CoilFrame.Refused(r) => r }

    private def hubNonce(opening: String): Option[HandshakeNonce] =
        CoilFrame.parse(opening).toOption.collect { case CoilFrame.Challenge(nonce) => nonce }

    private def meshNonce(opening: String): Option[HandshakeNonce] =
        HeadFrame.parse(opening).toOption.collect { case HeadFrame.Challenge(nonce) => nonce }

    private def answerAsCoil(coilNum: Int, wallet: PeerWallet)(opening: String): Option[String] =
        hubNonce(opening).map(nonce =>
            CoilFrame.encode(
              CoilFrame.Handshake.own(coilNum, wallet, HandshakeFixture.headParamsHash, nonce)
            )
        )

    private def answerAsHead(peerNum: Int, wallet: PeerWallet)(opening: String): Option[String] =
        meshNonce(opening).map(nonce =>
            HeadFrame.encode(
              HeadFrame.Handshake.own(peerNum, wallet, HandshakeFixture.headParamsHash, nonce)
            )
        )
}
