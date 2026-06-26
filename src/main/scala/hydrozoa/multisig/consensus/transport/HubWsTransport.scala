package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import fs2.Stream
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.BatchMessages.{OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonHubToCoil}
import hydrozoa.multisig.consensus.peer.CoilPeerNumber
import hydrozoa.multisig.consensus.transport.HubWsTransportEvent.*
import org.http4s.HttpRoutes
import org.http4s.dsl.io.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame

/** The hub side of a hub↔coil link, in the abstract. Concrete impls: [[HubWsTransport]] (real WS)
  * and [[InProcessHubCoilTransport.Hub]] (test harness).
  */
trait HubTransport {

    /** Wire a local [[PeerLiaisonHubToCoil]] handle as the inbound dispatch target for the given
      * coil peer. Must be called before that coil's link starts receiving traffic.
      */
    def register(coil: CoilPeerNumber, localLiaison: PeerLiaisonHubToCoil.Handle): IO[Unit]

    /** Enqueue a hub→coil batch for delivery to [[coil]]. */
    def send(coil: CoilPeerNumber, request: LiaisonProtocol.CoilToHubRequest): IO[Unit]
}

/** The hub side of the hub→coil WS links: contributes the `/hub` route to the hub's shared
  * [[NodeWsServer]] and serves every coil peer the hub hubs. The hub runs no dialer — each coil
  * dials in and identifies itself with [[CoilFrame.Hello]]; the hub binds that socket to the coil's
  * [[CoilPeerNumber]], routes inbound batches to that coil's [[PeerLiaisonHubToCoil]], and drains
  * that coil's outbox for outbound batches.
  *
  * Outbound is the hub-emitted subset ([[Population.New]] / [[OwnHardAck.Get]]); inbound is the
  * coil-emitted subset ([[Population.Get]] / [[OwnHardAck.New]]).
  */
final class HubWsTransport private (
    private val outboxes: Map[CoilPeerNumber, Queue[IO, String]],
    private val inboundRef: Ref[IO, Map[CoilPeerNumber, PeerLiaisonHubToCoil.Handle]],
    private val tracer: ContraTracer[IO, HubWsTransportEvent],
)(using CardanoNetwork.Section)
    extends HubTransport {

    override def register(
        coil: CoilPeerNumber,
        localLiaison: PeerLiaisonHubToCoil.Handle
    ): IO[Unit] =
        inboundRef.update(_.updated(coil, localLiaison))

    override def send(coil: CoilPeerNumber, request: LiaisonProtocol.CoilToHubRequest): IO[Unit] =
        CoilFrame.fromWire(request) match {
            case Some(wire) =>
                val line = CoilFrame.encode(CoilFrame.Msg(wire))
                outboxes.get(coil) match {
                    case Some(q) => q.offer(line)
                    case None    => tracer.traceWith(NoOutboxForCoil(coil))
                }
            case None =>
                tracer.traceWith(DroppingNonWireRequest(coil, request.toString))
        }

    private def dispatchInbound(coil: CoilPeerNumber, payload: CoilFrame.Wire): IO[Unit] =
        payload match {
            // Only the coil-emitted subset is valid inbound here.
            case p @ (_: Population.Get | _: OwnHardAck.New) =>
                inboundRef.get.flatMap { m =>
                    m.get(coil) match {
                        case Some(liaison) => liaison ! p
                        case None          => tracer.traceWith(NoLiaisonForInbound(coil))
                    }
                }
            case other =>
                tracer.traceWith(UnexpectedInboundWire(coil, other.toString))
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
                                tracer.traceWith(ServerAccepted(coilNum)) >>
                                    coilD.complete(coil).void
                            else tracer.traceWith(ServerRejectedHello(coilNum))
                        case Right(CoilFrame.Msg(payload)) =>
                            coilD.tryGet.flatMap {
                                case Some(coil) => dispatchInbound(coil, payload)
                                case None       => tracer.traceWith(ServerMsgBeforeHello)
                            }
                        case Left(err) =>
                            tracer.traceWith(ServerDecodeError(err))
                    }
                case _ => IO.unit
            }
            response <- wsb.build(sendStream, receivePipe)
        } yield response

    /** The `/hub` route to mount on the hub's shared [[NodeWsServer]]. */
    def routes(wsb: WebSocketBuilder2[IO]): HttpRoutes[IO] =
        HttpRoutes.of[IO] { case GET -> Root / "hub" =>
            serverHandler(wsb)
        }
}

object HubWsTransport {

    /** Allocate the hub-side coil transport: one outbox per hubbed coil peer + an empty inbound
      * map. The caller mounts [[routes]] on the hub's [[NodeWsServer]] and [[register]]s each
      * coil's liaison once it is spawned.
      */
    def create(
        coils: List[CoilPeerNumber],
        tracer: ContraTracer[IO, HubWsTransportEvent],
    )(using CardanoNetwork.Section): IO[HubWsTransport] =
        for {
            outboxes <- coils
                .traverse(c => Queue.unbounded[IO, String].map(c -> _))
                .map(_.toMap)
            inboundRef <- Ref[IO].of(Map.empty[CoilPeerNumber, PeerLiaisonHubToCoil.Handle])
        } yield new HubWsTransport(outboxes, inboundRef, tracer)
}
