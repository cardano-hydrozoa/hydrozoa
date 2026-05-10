package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.{Host, Port}
import com.suprnation.actor.ActorRef.ActorRef
import fs2.Stream
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.PeerLiaison
import hydrozoa.multisig.consensus.peer.HeadPeerId
import org.http4s.client.websocket.{WSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.server.{Server, websocket as _}
import org.http4s.websocket.WebSocketFrame
import org.http4s.{HttpRoutes, Uri}
import scala.concurrent.duration.*

/** Per-peer transport: owns a local WebSocket server, dials peers with higher peerNum, accepts
  * inbound connections from peers with lower peerNum, and exposes a [[send]] / [[register]] API.
  *
  * Connection topology: lower-numbered peer dials higher-numbered peer. Exactly one logical link
  * per (own, remote) pair — full-duplex over a single WS connection.
  *
  * Outbound queues are unbounded and retained across reconnects. The protocol on top
  * ([[PeerLiaison]]) is idempotent (GetMsgBatch/NewMsgBatch with explicit numbering), so a brief
  * window where the same message is delivered twice during a reconnect is harmless.
  */
final class PeerWsTransport private (
    val ownPeerId: HeadPeerId,
    private val outboxes: Map[HeadPeerId, Queue[IO, String]],
    private val inboundRef: Ref[IO, Map[HeadPeerId, PeerLiaison.Handle]],
)(using CardanoNetwork.Section) {

    private val logger = Logging.loggerIO(s"PeerWsTransport.${ownPeerId.peerNum: Int}")

    /** Wire a local PeerLiaison handle as the inbound dispatch target for messages arriving from
      * [[remote]]. Must be called before the link to [[remote]] starts receiving traffic.
      */
    def register(remote: HeadPeerId, localLiaison: PeerLiaison.Handle): IO[Unit] =
        inboundRef.update(_.updated(remote, localLiaison))

    /** Enqueue a request for delivery to [[remote]]. Returns immediately. The message is held in
      * the per-remote outbox queue until the WS link drains it.
      */
    def send(remote: HeadPeerId, request: PeerLiaison.Request): IO[Unit] =
        Frame.fromWire(request) match {
            case Some(wire) =>
                val line = Frame.encode(Frame.Msg(wire))
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

    private def dispatchInbound(remote: HeadPeerId, payload: PeerLiaison.Request): IO[Unit] =
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

    /** Run the read+write loop for an established connection. Read and write run in parallel;
      * either side completing terminates the duplex.
      */
    private def runDuplexHigh(
        remote: HeadPeerId,
        conn: WSConnectionHighLevel[IO]
    ): IO[Unit] = {
        val outbox = outboxes(remote)

        val writer: IO[Unit] =
            Stream
                .fromQueueUnterminated(outbox)
                .evalMap(line => conn.send(WSFrame.Text(line)))
                .compile
                .drain

        val reader: IO[Unit] =
            conn.receiveStream
                .evalMap {
                    case WSFrame.Text(s, _) =>
                        Frame.parse(s) match {
                            case Right(Frame.Msg(payload)) => dispatchInbound(remote, payload)
                            case Right(Frame.Hello(_))     =>
                                // Hello is only valid on the first frame; subsequent ones ignored.
                                IO.unit
                            case Left(err) =>
                                logger.warn(
                                  "failed to decode frame from " +
                                      s"remote=${remote.peerNum: Int}: ${err.getMessage}"
                                )
                        }
                    case _ => IO.unit
                }
                .compile
                .drain

        // Race: whichever side ends first cancels the other.
        IO.race(writer, reader).void
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
                val helloLine = Frame.encode(Frame.Hello(ownPeerId.peerNum))
                logger.info(s"dialer: connected to remote=${remote.peerNum: Int} at $uri") >>
                    conn.send(WSFrame.Text(helloLine)) >>
                    runDuplexHigh(remote, conn)
            }

        // Reconnect-on-drop with a small backoff. Errors are logged then retried.
        val attempt: IO[Unit] = once.handleErrorWith(e =>
            logger.warn(s"dialer to remote=${remote.peerNum: Int} failed: ${e.getMessage}")
        )

        (attempt >> IO.sleep(1.second)).foreverM
    }

    /** Server-side handler for an incoming WS connection. The first frame must be [[Frame.Hello]]
      * carrying the connecting peer's number; subsequent frames are dispatched as [[Frame.Msg]].
      */
    private def serverHandler(wsb: WebSocketBuilder2[IO]): IO[org.http4s.Response[IO]] = {
        // Used by the send-side to learn which peer is on the other end before pulling from
        // that peer's outbox. The server doesn't know who's connecting until the Hello frame
        // arrives, so we gate the send stream on a Deferred populated by the receive side.
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
                    Frame.parse(s) match {
                        case Right(Frame.Hello(peerNum)) =>
                            // Validate topology: server only accepts inbound from lower-numbered peers
                            val pn: Int = peerNum
                            val ownPn: Int = ownPeerId.peerNum
                            if pn < ownPn && pn >= 0 then {
                                val remote = HeadPeerId(pn, ownPeerId.nHeadPeers)
                                logger.info(
                                  s"server: accepted inbound from remote=$pn"
                                ) >> peerD.complete(remote).void
                            } else {
                                logger.warn(
                                  s"server: rejecting hello from peerNum=$pn " +
                                      s"(own=$ownPn, must be lower)"
                                )
                            }
                        case Right(Frame.Msg(payload)) =>
                            // Lookup which peer this connection belongs to (must have seen Hello)
                            peerD.tryGet.flatMap {
                                case Some(remote) => dispatchInbound(remote, payload)
                                case None =>
                                    logger.warn("server: msg before hello, dropping")
                            }
                        case Left(err) =>
                            logger.warn(s"server: failed to decode frame: ${err.getMessage}")
                    }
                case _ => IO.unit
            }
            response <- wsb.build(sendStream, receivePipe)
        } yield response
    }

    private[transport] def routes(wsb: WebSocketBuilder2[IO]): HttpRoutes[IO] =
        HttpRoutes.of[IO] { case GET -> Root / "peer" =>
            serverHandler(wsb)
        }
}

object PeerWsTransport {

    /** Build a transport. Allocates outboxes for every remote, starts the WS server, and launches a
      * dialer fiber for each remote with peerNum > ownPeerId.peerNum.
      *
      * @param ownPeerId
      *   Identity of this peer.
      * @param bindHost
      *   Local host to bind the WS server.
      * @param bindPort
      *   Local port to bind the WS server.
      * @param remotes
      *   Map of remote peers to their WebSocket URI (used for dialing higher-numbered peers).
      */
    def resource(
        ownPeerId: HeadPeerId,
        bindHost: Host,
        bindPort: Port,
        remotes: Map[HeadPeerId, Uri],
    )(using CardanoNetwork.Section): Resource[IO, PeerWsTransport] = {
        val logger = Logging.loggerIO(s"WsTransport.${ownPeerId.peerNum: Int}")

        for {
            outboxes <- Resource.eval(
              remotes.keys.toList
                  .traverse(rid => Queue.unbounded[IO, String].map(rid -> _))
                  .map(_.toMap)
            )
            inboundRef <- Resource.eval(Ref[IO].of(Map.empty[HeadPeerId, PeerLiaison.Handle]))
            transport = new PeerWsTransport(ownPeerId, outboxes, inboundRef)

            _: Server <- EmberServerBuilder
                .default[IO]
                .withHost(bindHost)
                .withPort(bindPort)
                .withHttpWebSocketApp(wsb => transport.routes(wsb).orNotFound)
                .build
                .evalTap(_ => logger.info(s"WS server bound at ws://$bindHost:$bindPort/peer"))

            client <- Resource.eval(JdkWSClient.simple[IO])

            // Dialer fibers: own dials remotes with higher peerNum.
            _ <- remotes.toList
                .filter { case (rid, _) => (rid.peerNum: Int) > (ownPeerId.peerNum: Int) }
                .traverse { case (rid, uri) =>
                    Resource.make(transport.dialerLoop(client, rid, uri).start)(_.cancel)
                }
        } yield transport
    }
}
