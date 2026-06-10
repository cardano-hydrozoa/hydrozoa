package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import fs2.Stream
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonHeadToHead}
import hydrozoa.multisig.consensus.peer.HeadPeerId
import org.http4s.client.websocket.{WSClient, WSFrame, WSRequest}
import org.http4s.dsl.io.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.{HttpRoutes, Uri}
import scala.concurrent.duration.*

/** The head-peer mesh WS link for one peer: contributes the `/head` route to the peer's shared
  * [[NodeWsServer]], dials peers with higher peerNum, accepts inbound from peers with lower
  * peerNum, and exposes a [[send]] / [[register]] API.
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
final class PeerWsTransport private (
    val ownPeerId: HeadPeerId,
    private val outboxes: Map[HeadPeerId, Queue[IO, String]],
    private val inboundRef: Ref[IO, Map[HeadPeerId, PeerLiaisonHeadToHead.Handle]],
    private val remotes: Map[HeadPeerId, Uri],
)(using CardanoNetwork.Section) {

    private val logger = Logging.loggerIO(s"PeerWsTransport.${ownPeerId.peerNum: Int}")

    /** Wire a local PeerLiaisonHeadToHead handle as the inbound dispatch target for messages
      * arriving from [[remote]]. Must be called before the link to [[remote]] starts receiving
      * traffic.
      */
    def register(remote: HeadPeerId, localLiaison: PeerLiaisonHeadToHead.Handle): IO[Unit] =
        inboundRef.update(_.updated(remote, localLiaison))

    /** Enqueue a request for delivery to [[remote]]. Returns immediately. The message is held in
      * the per-remote outbox queue until the WS link drains it.
      */
    def send(remote: HeadPeerId, request: LiaisonProtocol.HeadToHeadRequest): IO[Unit] =
        HeadFrame.fromWire(request) match {
            case Some(wire) =>
                val line = HeadFrame.encode(HeadFrame.Msg(wire))
                outboxes.get(remote) match {
                    case Some(q) => q.offer(line)
                    case None =>
                        logger.warn(s"send: no outbox for remote=${remote.peerNum: Int}")
                }
            // TODO: this should be panic at least, better should be not possible by types @Claude
            case None =>
                logger.warn(
                  s"send: dropping non-wire request to remote=${remote.peerNum: Int}: $request"
                )
        }

    private def dispatchInbound(
        remote: HeadPeerId,
        payload: LiaisonProtocol.HeadToHeadRequest
    ): IO[Unit] =
        inboundRef.get.flatMap { m =>
            m.get(remote) match {
                case Some(liaison) => liaison ! payload
                case None          =>
                    // TODO This may be panic, but then there will be a very simple way to shut down any peer
                    logger.warn(
                      s"inbound from remote=${remote.peerNum: Int} but no liaison registered"
                    )
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
                logger.warn(
                  s"failed to decode frame from remote=${remote.peerNum: Int}: ${err.getMessage}"
                )
        }

    /** Long-running dialer for a single remote. Reconnects with exponential backoff (capped).
      * Outbox is preserved across reconnects.
      */
    private def dialerLoop(
        client: WSClient[IO],
        remote: HeadPeerId,
        uri: Uri
    ): IO[Nothing] = {
        val request = WSRequest(uri)

        def once: IO[Unit] =
            client.connectHighLevel(request).use { conn =>
                val helloLine = HeadFrame.encode(HeadFrame.Hello(ownPeerId.peerNum))
                logger.info(s"dialer: connected to remote=${remote.peerNum: Int} at $uri") >>
                    conn.send(WSFrame.Text(helloLine)) >>
                    WsDuplex.run(conn, outboxes(remote), onLine(remote))
            }

        val attempt: IO[Unit] = once.handleErrorWith(e =>
            logger.warn(s"dialer to remote=${remote.peerNum: Int} failed: ${e.getMessage}")
        )

        (attempt >> IO.sleep(1.second)).foreverM
    }

    /** Server-side handler for an incoming WS connection. The first frame must be
      * [[HeadFrame.Hello]] carrying the connecting peer's number; subsequent frames are dispatched
      * as [[HeadFrame.Msg]].
      */
    private def serverHandler(wsb: WebSocketBuilder2[IO]): IO[org.http4s.Response[IO]] =
        for {
            peerD <- cats.effect.Deferred[IO, HeadPeerId]
            sendStream: Stream[IO, WebSocketFrame] =
                Stream
                    .eval(peerD.get)
                    .flatMap { remote =>
                        Stream
                            .fromQueueUnterminated(outboxes(remote))
                            .map(line => WebSocketFrame.Text(line))
                    }
            receivePipe: fs2.Pipe[IO, WebSocketFrame, Unit] = _.evalMap {
                case WebSocketFrame.Text(s, _) =>
                    HeadFrame.parse(s) match {
                        case Right(HeadFrame.Hello(peerNum)) =>
                            // Topology: the server only accepts inbound from lower-numbered peers.
                            val pn: Int = peerNum
                            val ownPn: Int = ownPeerId.peerNum
                            if pn < ownPn && pn >= 0 then {
                                val remote = HeadPeerId(pn, ownPeerId.nHeadPeers)
                                logger.info(s"server: accepted inbound from remote=$pn") >>
                                    peerD.complete(remote).void
                            } else {
                                logger.warn(
                                  s"server: rejecting hello from peerNum=$pn " +
                                      s"(own=$ownPn, must be lower)"
                                )
                            }
                        case Right(HeadFrame.Msg(payload)) =>
                            peerD.tryGet.flatMap {
                                case Some(remote) => dispatchInbound(remote, payload)
                                case None => logger.warn("server: msg before hello, dropping")
                            }
                        case Left(err) =>
                            logger.warn(s"server: failed to decode frame: ${err.getMessage}")
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

    /** Launch a dialer fiber for each remote with peerNum greater than ours (lower dials higher).
      * The fibers are torn down when the resource is released.
      */
    def startDialers(client: WSClient[IO]): Resource[IO, Unit] =
        remotes.toList
            .filter { case (rid, _) => (rid.peerNum: Int) > (ownPeerId.peerNum: Int) }
            .traverse_ { case (rid, uri) =>
                Resource.make(dialerLoop(client, rid, uri).start)(_.cancel).void
            }
}

object PeerWsTransport {

    /** Allocate the per-peer mesh transport: one outbox per remote + an empty inbound map. The
      * caller mounts [[routes]] on a [[NodeWsServer]] and calls [[startDialers]] with a shared
      * [[WSClient]].
      *
      * @param ownPeerId
      *   identity of this peer.
      * @param remotes
      *   remote peers to their WebSocket URI (used for dialing higher-numbered peers).
      */
    def create(
        ownPeerId: HeadPeerId,
        remotes: Map[HeadPeerId, Uri],
    )(using CardanoNetwork.Section): IO[PeerWsTransport] =
        for {
            outboxes <- remotes.keys.toList
                .traverse(rid => Queue.unbounded[IO, String].map(rid -> _))
                .map(_.toMap)
            inboundRef <- Ref[IO].of(Map.empty[HeadPeerId, PeerLiaisonHeadToHead.Handle])
        } yield new PeerWsTransport(ownPeerId, outboxes, inboundRef, remotes)
}
