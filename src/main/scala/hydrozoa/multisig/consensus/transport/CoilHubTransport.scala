package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import fs2.Stream
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.liaison.BatchMessages.{OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonHubToCoil}
import hydrozoa.multisig.consensus.peer.CoilPeerNumber
import org.http4s.HttpRoutes
import org.http4s.dsl.io.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame

/** The hub side of the hub→coil WS links: contributes the `/coil` route to the hub's shared
  * [[NodeWsServer]] and serves every coil peer the hub hubs. The hub runs no dialer — each coil
  * dials in and identifies itself with [[CoilFrame.Hello]]; the hub binds that socket to the coil's
  * [[CoilPeerNumber]], routes inbound batches to that coil's [[PeerLiaisonHubToCoil]], and drains
  * that coil's outbox for outbound batches.
  *
  * Outbound is the hub-emitted subset ([[Population.New]] / [[OwnHardAck.Get]]); inbound is the
  * coil-emitted subset ([[Population.Get]] / [[OwnHardAck.New]]).
  */
final class CoilHubTransport private (
    private val outboxes: Map[CoilPeerNumber, Queue[IO, String]],
    private val inboundRef: Ref[IO, Map[CoilPeerNumber, PeerLiaisonHubToCoil.Handle]],
)(using CardanoNetwork.Section) {

    private val logger = Logging.loggerIO("CoilHubTransport")

    /** Wire a local PeerLiaisonHubToCoil handle as the inbound dispatch target for the given coil
      * peer. Must be called before that coil's link starts receiving traffic.
      */
    def register(coil: CoilPeerNumber, localLiaison: PeerLiaisonHubToCoil.Handle): IO[Unit] =
        inboundRef.update(_.updated(coil, localLiaison))

    /** Enqueue a hub→coil batch for delivery to [[coil]]. */
    def send(coil: CoilPeerNumber, request: LiaisonProtocol.CoilToHubRequest): IO[Unit] =
        CoilFrame.fromWire(request) match {
            case Some(wire) =>
                val line = CoilFrame.encode(CoilFrame.Msg(wire))
                outboxes.get(coil) match {
                    case Some(q) => q.offer(line)
                    case None    => logger.warn(s"send: no outbox for coil=${coil.convert}")
                }
            case None =>
                logger.warn(s"send: dropping non-wire request to coil=${coil.convert}: $request")
        }

    private def dispatchInbound(coil: CoilPeerNumber, payload: CoilFrame.Wire): IO[Unit] =
        payload match {
            // Only the coil-emitted subset is valid inbound here.
            case p @ (_: Population.Get | _: OwnHardAck.New) =>
                inboundRef.get.flatMap { m =>
                    m.get(coil) match {
                        case Some(liaison) => liaison ! p
                        case None =>
                            logger.warn(
                              s"inbound from coil=${coil.convert} but no liaison registered"
                            )
                    }
                }
            case other =>
                logger.warn(s"unexpected hub-bound wire from coil=${coil.convert}: $other")
        }

    private def serverHandler(wsb: WebSocketBuilder2[IO]): IO[org.http4s.Response[IO]] =
        for {
            coilD <- Deferred[IO, CoilPeerNumber]
            sendStream: Stream[IO, WebSocketFrame] =
                Stream
                    .eval(coilD.get)
                    .flatMap { coil =>
                        Stream
                            .fromQueueUnterminated(outboxes(coil))
                            .map(line => WebSocketFrame.Text(line))
                    }
            receivePipe: fs2.Pipe[IO, WebSocketFrame, Unit] = _.evalMap {
                case WebSocketFrame.Text(s, _) =>
                    CoilFrame.parse(s) match {
                        case Right(CoilFrame.Hello(coilNum)) =>
                            val coil = CoilPeerNumber(coilNum)
                            if outboxes.contains(coil) then
                                logger.info(s"coil server: accepted inbound from coil=$coilNum") >>
                                    coilD.complete(coil).void
                            else
                                logger.warn(
                                  s"coil server: rejecting hello from unknown coil=$coilNum"
                                )
                        case Right(CoilFrame.Msg(payload)) =>
                            coilD.tryGet.flatMap {
                                case Some(coil) => dispatchInbound(coil, payload)
                                case None => logger.warn("coil server: msg before hello, dropping")
                            }
                        case Left(err) =>
                            logger.warn(s"coil server: failed to decode frame: ${err.getMessage}")
                    }
                case _ => IO.unit
            }
            response <- wsb.build(sendStream, receivePipe)
        } yield response

    /** The `/coil` route to mount on the hub's shared [[NodeWsServer]]. */
    def routes(wsb: WebSocketBuilder2[IO]): HttpRoutes[IO] =
        HttpRoutes.of[IO] { case GET -> Root / "coil" =>
            serverHandler(wsb)
        }
}

object CoilHubTransport {

    /** Allocate the hub-side coil transport: one outbox per hubbed coil peer + an empty inbound
      * map. The caller mounts [[routes]] on the hub's [[NodeWsServer]] and [[register]]s each
      * coil's liaison once it is spawned.
      */
    def create(
        coils: List[CoilPeerNumber]
    )(using CardanoNetwork.Section): IO[CoilHubTransport] =
        for {
            outboxes <- coils
                .traverse(c => Queue.unbounded[IO, String].map(c -> _))
                .map(_.toMap)
            inboundRef <- Ref[IO].of(Map.empty[CoilPeerNumber, PeerLiaisonHubToCoil.Handle])
        } yield new CoilHubTransport(outboxes, inboundRef)
}
