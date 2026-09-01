package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import fs2.Stream
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonHeadToHead}
import hydrozoa.multisig.consensus.peer.HeadPeerId
import hydrozoa.multisig.consensus.transport.PeerTransportEvent.*
import org.http4s.client.websocket.{WSClient, WSFrame, WSRequest}
import org.http4s.dsl.io.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.{HttpRoutes, Uri}
import scala.concurrent.duration.*

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
  * Outbound queues are unbounded and retained across reconnects. The protocol on top
  * ([[PeerLiaisonHeadToHead]]) is idempotent (GetMsgBatch/NewMsgBatch with explicit numbering), so
  * a brief window where the same message is delivered twice during a reconnect is harmless.
  */
final class WsPeerTransport private (
    val ownPeerId: HeadPeerId,
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
            case Right(HeadFrame.Hello(_))     =>
                // Hello is only valid on the first frame; subsequent ones ignored.
                IO.unit
            case Left(err) =>
                tracer.traceWith(ClientDecodeError(remote, err))
        }

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
        def once: IO[Unit] =
            client.connect(request).use { conn =>
                val helloLine = HeadFrame.encode(HeadFrame.Hello(ownPeerId.peerNum))
                tracer.traceWith(DialerConnected(remote, uri)) >>
                    conn.send(WSFrame.Text(helloLine)) >>
                    WsDuplex.run(conn, outboxes(remote), onLine(remote))
            }

        val attempt: IO[Unit] =
            (once >> tracer.traceWith(DialerDisconnected(remote, uri)))
                .handleErrorWith(e => tracer.traceWith(DialerFailed(remote, e)))

        // Same bound as the coil dialer: `JdkWSClient` builds its socket in an uncancelable acquire,
        // so a remote that accepts the connection and never answers the handshake blocks `once`
        // forever and this loop stops retrying altogether. `race` cancels the losing join, not the
        // attempt, so the stalled fiber is abandoned and the dialer moves on.
        val bounded: IO[Unit] =
            attempt.start.flatMap(f =>
                IO.race(f.join, IO.sleep(handshakeBudget)).flatMap {
                    case Left(_) => IO.unit
                    case Right(_) =>
                        tracer.traceWith(DialerHandshakeStalled(remote, uri, handshakeBudget))
                }
            )

        (bounded >> IO.sleep(1.second)).foreverM
    }

    /** Server-side handler for an incoming WS connection. The first frame must be
      * [[HeadFrame.Hello]] carrying the connecting peer's number; subsequent frames are dispatched
      * as [[HeadFrame.Msg]].
      */
    private def serverHandler(wsb: WebSocketBuilder2[IO]): IO[org.http4s.Response[IO]] =
        for {
            peerD <- cats.effect.Deferred[IO, HeadPeerId]
            sendStream: Stream[IO, WebSocketFrame] = NodeWsServer.withKeepAlive(keepAlivePing)(
              Stream
                  .eval(peerD.get)
                  .flatMap { remote =>
                      Stream
                          .fromQueueUnterminated(outboxes(remote))
                          .map(line => WebSocketFrame.Text(line))
                  }
            )
            receivePipe: fs2.Pipe[IO, WebSocketFrame, Unit] = _.evalMap {
                case WebSocketFrame.Text(s, _) =>
                    HeadFrame.parse(s) match {
                        case Right(HeadFrame.Hello(peerNum)) =>
                            // Topology: the server only accepts inbound from lower-numbered peers.
                            val pn: Int = peerNum
                            val ownPn: Int = ownPeerId.peerNum
                            if pn < ownPn && pn >= 0 then {
                                val remote = HeadPeerId(pn, ownPeerId.nHeadPeers)
                                tracer.traceWith(ServerAccepted(remote)) >>
                                    peerD.complete(remote).void
                            } else {
                                tracer.traceWith(ServerRejectedHello(pn, ownPn))
                            }
                        case Right(HeadFrame.Msg(payload)) =>
                            peerD.tryGet.flatMap {
                                case Some(remote) => dispatchInbound(remote, payload)
                                case None         => tracer.traceWith(ServerMsgBeforeHello)
                            }
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
        remoteIds: List[HeadPeerId],
        tracer: ContraTracer[IO, PeerTransportEvent],
        keepAlivePing: FiniteDuration = NodeWsServer.defaultKeepAlivePing,
    )(using CardanoNetwork.Section): IO[WsPeerTransport] =
        for {
            outboxes <- remoteIds
                .traverse(rid => Queue.unbounded[IO, String].map(rid -> _))
                .map(_.toMap)
            inboundRef <- Ref[IO].of(Map.empty[HeadPeerId, PeerLiaisonHeadToHead.Handle])
        } yield new WsPeerTransport(ownPeerId, outboxes, inboundRef, keepAlivePing, tracer)
}
