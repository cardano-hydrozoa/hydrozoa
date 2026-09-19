package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import fs2.Stream
import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.BatchMessages.{Join, OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonHubToCoil}
import hydrozoa.multisig.consensus.peer.CoilPeerNumber
import hydrozoa.multisig.consensus.transport.HubWsTransportEvent.*
import org.http4s.HttpRoutes
import org.http4s.dsl.io.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import scala.concurrent.duration.FiniteDuration
import scalus.cardano.ledger.Hash32

/** The hub side of a hub↔coil link, in the abstract. Concrete impls: [[HubWsTransport]] (real WS)
  * and [[InProcessHubCoilTransport.Hub]] (test harness).
  */
trait HubTransport {

    /** Wire a local [[PeerLiaisonHubToCoil]] handle as the inbound dispatch target for the given
      * coil peer. Must be called before that coil's link starts receiving traffic.
      */
    def register(coil: CoilPeerNumber, localLiaison: PeerLiaisonHubToCoil.Handle): IO[Unit]

    /** Enqueue a hub→coil batch for delivery to [[coil]]. */
    def send(coil: CoilPeerNumber, request: LiaisonProtocol.CoilRequestServed): IO[Unit]
}

/** The hub side of the hub→coil WS links: contributes the `/hub` route to the hub's shared
  * [[NodeWsServer]] and serves every coil peer the hub hubs. The hub runs no dialer — each coil
  * dials in and proves which coil peer it is, and the hub binds that socket to the coil's
  * [[CoilPeerNumber]], routes inbound batches to that coil's [[PeerLiaisonHubToCoil]], and drains
  * that coil's outbox for outbound batches.
  *
  * **The socket's identity is proven, not asserted.** The hub opens with a [[CoilFrame.Challenge]],
  * the coil answers with a [[CoilFrame.Handshake]] signed over that nonce, and the hub checks the
  * signature against the `coilPeers` key for the claimed number. A socket whose handshake does not
  * check out is told why ([[CoilFrame.Refused]]) and closed, rather than left holding a connection
  * that carries nothing.
  *
  * Outbound is the hub-emitted subset ([[Join.Offer]], [[Population.New]], [[OwnHardAck.Get]]);
  * inbound is the coil-emitted subset ([[Population.Get]] / [[OwnHardAck.New]]).
  */
final class HubWsTransport private (
    private val outboxes: Map[CoilPeerNumber, Queue[IO, String]],
    private val coilPeers: CoilPeers,
    private val headParamsHash: Hash32,
    private val inboundRef: Ref[IO, Map[CoilPeerNumber, PeerLiaisonHubToCoil.Handle]],
    private val ownHead: HeadIdentity,
    private val keepAlivePing: FiniteDuration,
    private val tracer: ContraTracer[IO, HubWsTransportEvent],
)(using CardanoNetwork.Section)
    extends HubTransport {

    override def register(
        coil: CoilPeerNumber,
        localLiaison: PeerLiaisonHubToCoil.Handle
    ): IO[Unit] =
        inboundRef.update(_.updated(coil, localLiaison))

    override def send(coil: CoilPeerNumber, request: LiaisonProtocol.CoilRequestServed): IO[Unit] =
        CoilFrame.fromWire(request) match {
            case Some(wire) =>
                val line = CoilFrame.encode(CoilFrame.Msg(wire))
                outboxes.get(coil) match {
                    case Some(q) => q.offer(line)
                    case None    => tracer.traceWith(NoOutboxForCoil(coil))
                }
            case None =>
                tracer.traceWith(DroppingNonWireRequest(coil, request))
        }

    private def dispatchInbound(coil: CoilPeerNumber, payload: CoilFrame.Wire): IO[Unit] =
        payload match {
            // Only the coil-emitted subset is valid inbound here.
            case p @ (_: Population.Get | _: OwnHardAck.New) => toLiaison(coil, p)
            case other =>
                tracer.traceWith(UnexpectedInboundWire(coil, other))
        }

    private def toLiaison(
        coil: CoilPeerNumber,
        request: LiaisonProtocol.HubRequestServed
    ): IO[Unit] =
        inboundRef.get.flatMap { m =>
            m.get(coil) match {
                case Some(liaison) => liaison ! request
                case None          => tracer.traceWith(NoLiaisonForInbound(coil))
            }
        }

    /** The ordered verdict on one inbound handshake.
      *
      * Version first: a coil speaking another protocol may not even mean the same thing by its own
      * number, so there is nothing to look up until the two ends agree on the vocabulary. Head
      * identity next, then the roster — the claimed number is what resolves the key the proof is
      * checked against. Only then the proof itself.
      */
    private def admit(
        coilNum: Int,
        protocolVersion: Option[Int],
        auth: HandshakeAuth,
        nonce: HandshakeNonce,
        head: Option[HeadIdentity]
    ): Either[HandshakeRefusal, CoilPeerNumber] =
        ProtocolVersion.check(protocolVersion) match {
            case ProtocolVersion.Check.Incompatible(found, expected) =>
                Left(HandshakeRefusal.ProtocolVersionMismatch(found, expected))
            case ProtocolVersion.Check.Compatible
                if HeadIdentity.check(head, ownHead) != HeadIdentity.Check.Compatible =>
                Left(
                  HandshakeRefusal.WrongHead(
                    HeadIdentity.describe(HeadIdentity.check(head, ownHead))
                  )
                )
            case ProtocolVersion.Check.Compatible =>
                val coil = CoilPeerNumber(coilNum)
                // Both halves of "a coil this hub serves": an outbox to drain, and a roster key to
                // check the proof against. They are configured together, so a coil with one and not
                // the other is a config split, and refusing is the only safe reading.
                val hubbed =
                    Option.when(outboxes.contains(coil))(coil).flatMap(coilPeers.verificationKey)
                hubbed.toRight(HandshakeRefusal.NotHubbed(coilNum)).flatMap { vkey =>
                    HandshakeProof
                        .verify(
                          vkey,
                          HandshakeProof.Link.CoilToHub,
                          coilNum,
                          ProtocolVersion.current,
                          headParamsHash,
                          nonce,
                          auth
                        )
                        .map(_ => coil)
                }
        }

    private def serverHandler(wsb: WebSocketBuilder2[IO]): IO[org.http4s.Response[IO]] =
        for {
            nonce <- HandshakeNonce.random
            // Right: bound to a coil, drain its outbox. Left: refused, say why and close. Nothing
            // else ever reaches the send stream, so a socket that is neither simply carries the
            // challenge and dies on the server's idle timeout.
            verdictD <- Deferred[IO, Either[HandshakeRefusal, CoilPeerNumber]]
            sendStream: Stream[IO, WebSocketFrame] =
                Stream.emit(
                  WebSocketFrame.Text(CoilFrame.encode(CoilFrame.Challenge.own(nonce)))
                ) ++
                    Stream.eval(verdictD.get).flatMap {
                        case Right(coil) =>
                            NodeWsServer.withKeepAlive(keepAlivePing)(
                              Stream
                                  .fromQueueUnterminated(outboxes(coil))
                                  .map(line => WebSocketFrame.Text(line))
                            )
                        // Ending the stream is what closes the socket; the frames before it are
                        // what stop the coil reading that close as a network fault.
                        case Left(refusal) =>
                            Stream(
                              WebSocketFrame.Text(CoilFrame.encode(CoilFrame.Refused(refusal))),
                              NodeWsServer.closeFrame(HandshakeRefusal.describe(refusal))
                            )
                    }
            receivePipe: fs2.Pipe[IO, WebSocketFrame, Unit] = _.evalMap {
                case WebSocketFrame.Text(s, _) =>
                    CoilFrame.parse(s) match {
                        case Right(
                              CoilFrame.Handshake(coilNum, protocolVersion, auth, marks, head)
                            ) =>
                            val verdict =
                                admit(coilNum, protocolVersion, auth, nonce, head)
                            // One nonce, one handshake: a socket that already has a verdict
                            // keeps it, so a replayed handshake cannot re-bind an
                            // established session.
                            verdictD.complete(verdict).flatMap {
                                case false =>
                                    tracer.traceWith(ServerRepeatHandshake(coilNum))
                                case true =>
                                    verdict match {
                                        case Right(coil) =>
                                            // Bind the socket BEFORE announcing the link, so
                                            // the start point the liaison decides on has an
                                            // outbox to leave by.
                                            tracer.traceWith(ServerAccepted(coilNum)) >>
                                                toLiaison(coil, marks)
                                        case Left(refusal) =>
                                            tracer.traceWith(
                                              ServerRefusedHandshake(coilNum, refusal)
                                            )
                                    }
                            }
                        case Right(CoilFrame.Msg(payload)) =>
                            verdictD.tryGet.flatMap {
                                case Some(Right(coil)) => dispatchInbound(coil, payload)
                                case _                 => tracer.traceWith(ServerMsgBeforeHandshake)
                            }
                        case Right(_: CoilFrame.Challenge | _: CoilFrame.Refused) =>
                            // Both are hub→coil frames; a coil sending one is misbehaving.
                            tracer.traceWith(ServerUnexpectedFrame)
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
        coilPeers: CoilPeers,
        headParamsHash: Hash32,
        ownHead: HeadIdentity,
        tracer: ContraTracer[IO, HubWsTransportEvent],
        keepAlivePing: FiniteDuration = NodeWsServer.defaultKeepAlivePing,
    )(using CardanoNetwork.Section): IO[HubWsTransport] =
        for {
            outboxes <- coils
                .traverse(c => Queue.unbounded[IO, String].map(c -> _))
                .map(_.toMap)
            inboundRef <- Ref[IO].of(Map.empty[CoilPeerNumber, PeerLiaisonHubToCoil.Handle])
        } yield new HubWsTransport(
          outboxes,
          coilPeers,
          headParamsHash,
          inboundRef,
          ownHead,
          keepAlivePing,
          tracer
        )
}
