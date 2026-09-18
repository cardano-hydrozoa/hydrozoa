package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.{Deferred, FiberIO, IO, Ref, Resource}
import cats.syntax.all.*
import fs2.Stream
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.QuietRelease
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonHeadToHead}
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber, PeerWallet}
import hydrozoa.multisig.consensus.transport.PeerTransportEvent.*
import org.http4s.client.websocket.{WSClient, WSFrame, WSRequest}
import org.http4s.dsl.io.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.{HttpRoutes, Uri}
import scala.concurrent.duration.*
import scalus.cardano.ledger.Hash32

/** The head-peer mesh transport one peer uses to talk to the others. */
trait PeerTransport {

    /** This transport's own identity in the head mesh. */
    def ownPeerId: HeadPeerId

    /** Wire a local `PeerLiaisonHeadToHead` handle as the inbound dispatch target for messages
      * arriving from [[remote]]. Must be called before the link to [[remote]] starts receiving
      * traffic.
      */
    def register(remote: HeadPeerId, localLiaison: PeerLiaisonHeadToHead.Handle): IO[Unit]

    /** Enqueue a request for delivery to [[remote]]. Returns immediately. */
    def send(remote: HeadPeerId, request: LiaisonProtocol.HeadToHeadRequest): IO[Unit]
}

/** Real WS-backed [[PeerTransport]]: contributes the `/head` route to the peer's shared
  * [[NodeWsServer]], dials peers with higher peerNum, accepts inbound from peers with lower
  * peerNum.
  *
  * It does NOT own the Ember server — a hub head peer shares one server across the mesh and the
  * hub→coil link, so server ownership lives in [[NodeWsServer]] and the caller mounts [[routes]]
  * there.
  *
  * Connection topology: lower-numbered peer dials higher-numbered peer. Exactly one logical link
  * per (own, remote) pair — full-duplex over a single WS connection.
  *
  * **The socket's identity is proven, not asserted.** The accepting peer opens with a
  * [[HeadFrame.Challenge]], the dialer answers with a [[HeadFrame.Handshake]] signed over that
  * nonce, and the accepting peer checks the signature against the `headPeers` key for the claimed
  * number. A socket whose handshake does not check out is told why ([[HeadFrame.Refused]]) and
  * closed. The dial topology is a separate rule from identity and both are enforced: it is what
  * keeps the mesh at one link per pair, not what says who is on it.
  *
  * Outbound queues are unbounded and retained across reconnects. The protocol on top
  * ([[PeerLiaisonHeadToHead]]) is idempotent (GetMsgBatch/NewMsgBatch with explicit numbering), so
  * a brief window where the same message is delivered twice during a reconnect is harmless.
  */
final class WsPeerTransport private (
    val ownPeerId: HeadPeerId,
    private val ownWallet: PeerWallet,
    private val headPeers: HeadPeers.Section,
    private val headParamsHash: Hash32,
    private val outboxes: Map[HeadPeerId, Queue[IO, String]],
    private val inboundRef: Ref[IO, Map[HeadPeerId, PeerLiaisonHeadToHead.Handle]],
    private val keepAlivePing: FiniteDuration,
    private val tracer: ContraTracer[IO, PeerTransportEvent],
)(using CardanoNetwork.Section)
    extends PeerTransport {

    override def register(
        remote: HeadPeerId,
        localLiaison: PeerLiaisonHeadToHead.Handle
    ): IO[Unit] =
        inboundRef.update(_.updated(remote, localLiaison))

    /** Enqueue a request for delivery to [[remote]]. Returns immediately. The message is held in
      * the per-remote outbox queue until the WS link drains it.
      */
    override def send(remote: HeadPeerId, request: LiaisonProtocol.HeadToHeadRequest): IO[Unit] =
        HeadFrame.fromWire(request) match {
            case Some(wire) =>
                val line = HeadFrame.encode(HeadFrame.Msg(wire))
                outboxes.get(remote) match {
                    case Some(q) => q.offer(line)
                    case None    => tracer.traceWith(NoOutboxForRemote(remote))
                }
            // TODO: this should be panic at least, better should be not possible by types @Claude
            case None =>
                tracer.traceWith(DroppingNonWireRequest(remote))
        }

    private def dispatchInbound(
        remote: HeadPeerId,
        payload: LiaisonProtocol.HeadToHeadRequest
    ): IO[Unit] =
        inboundRef.get.flatMap { m =>
            m.get(remote) match {
                case Some(liaison) => liaison ! payload
                // TODO This may be panic, but then there will be a very simple way to shut down any peer
                case None => tracer.traceWith(NoLiaisonForInbound(remote))
            }
        }

    /** Parse one inbound text line on an established link and dispatch a [[HeadFrame.Msg]] payload.
      */
    private def onLine(remote: HeadPeerId)(s: String): IO[Unit] =
        HeadFrame.parse(s) match {
            case Right(HeadFrame.Msg(payload)) => dispatchInbound(remote, payload)
            // The remote's verdict on the handshake this attempt just sent: it refused, and the
            // socket is about to close. The dialer redials regardless — a refusal describes this
            // attempt, not the remote — so the trace is what tells an operator to go fix a config.
            case Right(HeadFrame.Refused(refusal)) =>
                tracer.traceWith(DialerRefused(remote, refusal))
            // A challenge is answered before the duplex starts, and a remote sends exactly one; a
            // second is the remote misbehaving, not a renegotiation.
            case Right(_: HeadFrame.Challenge) => tracer.traceWith(DialerLateChallenge(remote))
            case Right(_: HeadFrame.Handshake) =>
                // A handshake is only valid as the first frame; subsequent ones are ignored.
                IO.unit
            case Left(err) =>
                tracer.traceWith(ClientDecodeError(remote, err))
        }

    /** A remote's opening challenge, or `None` if it opened with something else. */
    private def challenge(line: String): Option[HeadFrame.Challenge] =
        HeadFrame.parse(line).toOption.collect { case c: HeadFrame.Challenge => c }

    /** Long-running dialer for a single remote. Attempts to reconnect forever with a 1-second
      * delay. Outbox is preserved across reconnects.
      */
    private def dialerLoop(
        client: WSClient[IO],
        remote: HeadPeerId,
        uri: Uri
    ): IO[Nothing] = {
        val request = WSRequest(uri)

        // Low-level `connect`, not `connectHighLevel`: the dialer needs to see the remote's
        // keep-alive Ping to know the link is alive (see WsDuplex).
        // `handshook` arbitrates between this attempt and the loop's budget, and the remote speaks
        // first: its challenge is what this attempt's proof is signed over, so the socket is
        // claimed only once there is a nonce to answer. Same shape as the coil dialer in
        // `CoilPeerWsTransport`, including why an attempt without a challenge must end itself.
        def once(handshook: Deferred[IO, Unit]): IO[Unit] =
            QuietRelease(client.connect(request)).use { conn =>
                WsDuplex.firstLine(conn, challengeBudget).map(_.flatMap(challenge)).flatMap {
                    case None => tracer.traceWith(DialerNoChallenge(remote, uri, challengeBudget))
                    case Some(HeadFrame.Challenge(nonce, protocolVersion)) =>
                        ProtocolVersion.check(protocolVersion) match {
                            // Refuse before answering. Signing a proof for a peer this node cannot
                            // talk to is wasted work, and deciding here is what lets the dialer
                            // name both versions against a remote that never says why — which is
                            // what a remote too old to send `Refused` does.
                            case ProtocolVersion.Check.Incompatible(found, expected) =>
                                tracer.traceWith(
                                  DialerRefusedChallenge(
                                    remote,
                                    HandshakeRefusal.ProtocolVersionMismatch(found, expected)
                                  )
                                )
                            case ProtocolVersion.Check.Compatible =>
                                val handshakeLine = HeadFrame.encode(
                                  HeadFrame.Handshake
                                      .own(ownPeerId.peerNum, ownWallet, headParamsHash, nonce)
                                )
                                handshook.complete(()).flatMap {
                                    case true =>
                                        tracer.traceWith(DialerConnected(remote, uri)) >>
                                            conn.send(WSFrame.Text(handshakeLine)) >>
                                            WsDuplex.run(conn, outboxes(remote), onLine(remote))
                                    // Lost the claim: the budget expired and the loop has already
                                    // redialed. Return instead, so `use` closes this socket rather
                                    // than leaving a second live connection draining this remote's
                                    // outbox.
                                    case false =>
                                        tracer.traceWith(DialerHandshakeLate(remote, uri))
                                }
                        }
                }
            }

        def attempt(handshook: Deferred[IO, Unit]): IO[Unit] =
            (once(handshook) >> tracer.traceWith(DialerDisconnected(remote, uri)))
                .handleErrorWith(e => tracer.traceWith(DialerFailed(remote, e)))

        // `onCancel` is what keeps teardown closing the socket: the attempt runs on its own fiber,
        // so cancelling this loop cancels the JOIN and not the attempt behind it. Safe past the
        // claim and only there — by then the uncancelable acquire has finished.
        def own(f: FiberIO[Unit]): IO[Unit] = f.join.void.onCancel(f.cancel)

        // Same bound as the coil dialer, and bounding the same thing: the HANDSHAKE only.
        // `JdkWSClient` builds its socket in an uncancelable acquire, so a remote that accepts the
        // connection and never answers blocks the attempt forever and this loop stops retrying
        // altogether. But `WsDuplex.run` holds the attempt for the whole life of a healthy link, so
        // bounding the attempt as a whole abandons working connections on a timer instead.
        val bounded: IO[Unit] =
            for {
                handshook <- Deferred[IO, Unit]
                f <- attempt(handshook).start
                _ <- IO.race(IO.race(handshook.get, f.join), IO.sleep(handshakeBudget)).flatMap {
                    case Left(Left(_))  => own(f)
                    case Left(Right(_)) => IO.unit
                    case Right(_) =>
                        handshook.complete(()).flatMap {
                            case true =>
                                tracer.traceWith(
                                  DialerHandshakeStalled(remote, uri, handshakeBudget)
                                )
                            case false => own(f)
                        }
                }
            } yield ()

        (bounded >> IO.sleep(1.second)).foreverM
    }

    /** The ordered verdict on one inbound handshake.
      *
      * Version first: a peer speaking another protocol may not even mean the same thing by its own
      * number, so there is nothing to place in the topology until the two ends agree on the
      * vocabulary. The dial topology next, because the claimed number is what resolves the key the
      * proof is checked against. Only then the proof itself.
      */
    private def admit(
        peerNum: Int,
        protocolVersion: Option[Int],
        auth: HandshakeAuth,
        nonce: HandshakeNonce
    ): Either[HandshakeRefusal, HeadPeerId] = {
        val ownPn: Int = ownPeerId.peerNum
        ProtocolVersion.check(protocolVersion) match {
            case ProtocolVersion.Check.Incompatible(found, expected) =>
                Left(HandshakeRefusal.ProtocolVersionMismatch(found, expected))
            // Topology: the server only accepts inbound from lower-numbered peers. It was never
            // authentication — it is the dial rule, and it stays because two peers dialing each
            // other would build two links where the mesh has one.
            case ProtocolVersion.Check.Compatible =>
                if peerNum >= ownPn || peerNum < 0 then
                    Left(HandshakeRefusal.WrongDialDirection(peerNum, ownPn))
                else
                    val remote = HeadPeerId(HeadPeerNumber(peerNum), ownPeerId.nHeadPeers)
                    headPeers
                        .headPeerVKey(HeadPeerNumber(peerNum))
                        .toRight(HandshakeRefusal.NotInRoster(peerNum))
                        .flatMap { vkey =>
                            HandshakeProof
                                .verify(
                                  vkey,
                                  HandshakeProof.Link.HeadToHead,
                                  peerNum,
                                  ProtocolVersion.current,
                                  headParamsHash,
                                  nonce,
                                  auth
                                )
                                .map(_ => remote)
                        }
        }
    }

    /** Server-side handler for an incoming WS connection. The server opens with a
      * [[HeadFrame.Challenge]]; the dialer's first frame must be a [[HeadFrame.Handshake]] proving
      * its peer number over that nonce, and subsequent frames are dispatched as [[HeadFrame.Msg]].
      */
    private def serverHandler(wsb: WebSocketBuilder2[IO]): IO[org.http4s.Response[IO]] =
        for {
            nonce <- HandshakeNonce.random
            // Right: bound to a remote, drain its outbox. Left: refused, say why and close. Nothing
            // else ever reaches the send stream, so a socket that is neither simply carries the
            // challenge and dies on the server's idle timeout.
            verdictD <- Deferred[IO, Either[HandshakeRefusal, HeadPeerId]]
            sendStream: Stream[IO, WebSocketFrame] =
                Stream.emit(
                  WebSocketFrame.Text(HeadFrame.encode(HeadFrame.Challenge.own(nonce)))
                ) ++
                    Stream.eval(verdictD.get).flatMap {
                        case Right(remote) =>
                            NodeWsServer.withKeepAlive(keepAlivePing)(
                              Stream
                                  .fromQueueUnterminated(outboxes(remote))
                                  .map(line => WebSocketFrame.Text(line))
                            )
                        // Ending the stream is what closes the socket; the frames before it are
                        // what stop the dialer reading that close as a network fault.
                        case Left(refusal) =>
                            Stream(
                              WebSocketFrame.Text(HeadFrame.encode(HeadFrame.Refused(refusal))),
                              NodeWsServer.closeFrame(HandshakeRefusal.describe(refusal))
                            )
                    }
            receivePipe: fs2.Pipe[IO, WebSocketFrame, Unit] = _.evalMap {
                case WebSocketFrame.Text(s, _) =>
                    HeadFrame.parse(s) match {
                        case Right(HeadFrame.Handshake(peerNum, protocolVersion, auth)) =>
                            val verdict = admit(peerNum, protocolVersion, auth, nonce)
                            // One nonce, one handshake: a socket that already has a verdict keeps
                            // it, so a replayed handshake cannot re-bind an established session.
                            verdictD.complete(verdict).flatMap {
                                case false => tracer.traceWith(ServerRepeatHandshake(peerNum))
                                case true =>
                                    verdict match {
                                        case Right(remote) =>
                                            tracer.traceWith(ServerAccepted(remote))
                                        case Left(refusal) =>
                                            tracer.traceWith(
                                              ServerRefusedHandshake(peerNum, refusal)
                                            )
                                    }
                            }
                        case Right(HeadFrame.Msg(payload)) =>
                            verdictD.tryGet.flatMap {
                                case Some(Right(remote)) => dispatchInbound(remote, payload)
                                case _ => tracer.traceWith(ServerMsgBeforeHandshake)
                            }
                        case Right(_: HeadFrame.Challenge | _: HeadFrame.Refused) =>
                            // Both are accept-side frames; a dialer sending one is misbehaving.
                            tracer.traceWith(ServerUnexpectedFrame)
                        case Left(err) =>
                            tracer.traceWith(ServerDecodeError(err))
                    }
                case _ => IO.unit
            }
            response <- wsb.build(sendStream, receivePipe)
        } yield response

    /** The `/head` route to mount on the peer's shared [[NodeWsServer]]. */
    def routes(wsb: WebSocketBuilder2[IO]): HttpRoutes[IO] =
        HttpRoutes.of[IO] { case GET -> Root / "head" =>
            serverHandler(wsb)
        }

    /** How long one dial attempt may sit in the WebSocket handshake before it is abandoned. */
    private val handshakeBudget: FiniteDuration = 30.seconds

    /** How long one dial attempt waits for the remote's [[HeadFrame.Challenge]] once the WebSocket
      * handshake has completed. Sized and bounded exactly as the coil dialer's is — see
      * [[CoilPeerWsTransport]].
      */
    private val challengeBudget: FiniteDuration = 10.seconds

    /** How long teardown waits for a dialer to acknowledge cancellation before proceeding. */
    private val dialerCancelBudget: FiniteDuration = 5.seconds

    /** Launch a dialer fiber for each remote with peerNum greater than ours (lower dials higher).
      * The fibers are torn down when the resource is released. URIs are passed at dial-start time
      * so the caller can bind on an OS-assigned ephemeral port and only build the URI map after
      * every peer's server is bound.
      */
    def startDialers(
        client: WSClient[IO],
        remotes: Map[HeadPeerId, Uri],
    ): Resource[IO, Unit] =
        remotes.toList
            .filter { case (rid, _) => (rid.peerNum: Int) > (ownPeerId.peerNum: Int) }
            .traverse_ { case (rid, uri) =>
                Resource
                    .make(dialerLoop(client, rid, uri).start)(fiber =>
                        // Bounded for the same reason as the coil side: `cancel` waits for the
                        // fiber to finalize and is itself uncancelable, so a dialer stuck in the
                        // handshake would hang teardown. Bound the JOIN, which is cancelable.
                        tracer.traceWith(DialerStopped(rid, uri)) >>
                            fiber.cancel.start
                                .flatMap(_.join.timeoutTo(dialerCancelBudget, IO.unit))
                                .void
                    )
                    .void
            }
}

object WsPeerTransport {

    /** Allocate the per-peer mesh transport: one outbox per remote + an empty inbound map. URIs
      * aren't required here — they're passed to [[WsPeerTransport.startDialers]] later so the
      * server can be bound on an OS-assigned port first.
      *
      * @param ownPeerId
      *   identity of this peer.
      * @param remoteIds
      *   identities of the other head peers; one outbox queue is allocated per id.
      * @param tracer
      *   sink for transport-level events.
      */
    def create(
        ownPeerId: HeadPeerId,
        ownWallet: PeerWallet,
        headPeers: HeadPeers.Section,
        headParamsHash: Hash32,
        remoteIds: List[HeadPeerId],
        tracer: ContraTracer[IO, PeerTransportEvent],
        keepAlivePing: FiniteDuration = NodeWsServer.defaultKeepAlivePing,
    )(using CardanoNetwork.Section): IO[WsPeerTransport] =
        for {
            outboxes <- remoteIds
                .traverse(rid => Queue.unbounded[IO, String].map(rid -> _))
                .map(_.toMap)
            inboundRef <- Ref[IO].of(Map.empty[HeadPeerId, PeerLiaisonHeadToHead.Handle])
        } yield new WsPeerTransport(
          ownPeerId,
          ownWallet,
          headPeers,
          headParamsHash,
          outboxes,
          inboundRef,
          keepAlivePing,
          tracer
        )
}
