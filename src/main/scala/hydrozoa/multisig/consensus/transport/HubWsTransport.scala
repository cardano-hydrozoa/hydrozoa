package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import fs2.Stream
import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.BatchMessages.{Join, OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.peer.CoilPeerNumber
import hydrozoa.multisig.consensus.transport.HubWsTransportEvent.*
import org.http4s.HttpRoutes
import org.http4s.dsl.io.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scalus.cardano.ledger.Hash32

/** The hub side of a hub↔coil link, in the abstract. Concrete impls: [[HubWsTransport]] (real WS)
  * and [[InProcessHubCoilTransport.Hub]] (test harness).
  */
trait HubTransport {

    /** Wire a local [[PeerLiaisonHubToCoil]] handle as the inbound dispatch target for the given
      * coil peer. Must be called before that coil's link starts receiving traffic.
      */
    def register(coil: CoilPeerNumber, localLiaison: LiaisonProtocol.HubLiaisonHandle): IO[Unit]

    /** Stop dispatching inbound from [[coil]] to the liaison [[register]] wired for it, before that
      * liaison is stopped. The coil does not know this hub stopped it and keeps sending; from now
      * on its frames are dropped as expected rather than delivered to a stopped actor.
      */
    def unregister(coil: CoilPeerNumber): IO[Unit]

    /** Enqueue a hub→coil batch for delivery to [[coil]]. */
    def send(coil: CoilPeerNumber, request: Join.Answer | LiaisonProtocol.HubEmitted): IO[Unit]
}

/** The hub side of the hub→coil WS links: contributes the `/hub` route to the hub's shared
  * [[NodeWsServer]] and serves every coil peer the hub hubs. The hub runs no dialer — each coil
  * dials in and proves which coil peer it is, and the hub binds that socket to the coil's
  * [[CoilPeerNumber]], routes inbound batches to the liaison registered for that coil, and drains
  * that coil's outbox for outbound batches.
  *
  * ==Which liaison==
  *
  * Every liaison this transport routes to is **the hub's own actor, living in the hub's actor
  * system**. A hub runs one [[hydrozoa.multisig.consensus.liaison.PeerLiaisonHubToCoil]] per coil
  * peer it serves, and [[HeadMultisigRegimeManager]] spawns them all in its `preStart`. They are
  * keyed here by [[CoilPeerNumber]] because that is which link each one owns — not because they
  * belong to, or run on, the coil. The coil node has a liaison of its own,
  * [[hydrozoa.multisig.consensus.liaison.PeerLiaisonCoilToHub]], and nothing on this side waits for
  * it or can observe it.
  *
  * {{{
  *   HUB NODE (a head peer)                            COIL NODES
  *  ┌──────────────────────────────────┐
  *  │  PeerLiaisonHubToCoil(coil 1) ───┼── WS ──▶ coil 1 ── PeerLiaisonCoilToHub
  *  │  PeerLiaisonHubToCoil(coil 2) ───┼── WS ──▶ coil 2 ── PeerLiaisonCoilToHub
  *  │  PeerLiaisonHubToCoil(coil 3) ───┼── WS ──▶ coil 3 ── PeerLiaisonCoilToHub
  *  │                                  │
  *  │  one HubWsTransport, one bound   │         each coil runs exactly one
  *  │  port, one liaison per coil      │         liaison, toward its hub
  *  └──────────────────────────────────┘
  * }}}
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
    private val inboundRef: Ref[
      IO,
      Map[CoilPeerNumber, InboundRoute[LiaisonProtocol.HubLiaisonHandle]]
    ],
    private val liaisonsUp: Map[CoilPeerNumber, Deferred[IO, Unit]],
    private val ownHead: HeadIdentity,
    private val keepAlivePing: FiniteDuration,
    private val liaisonWaitReport: FiniteDuration,
    private val tracer: ContraTracer[IO, HubWsTransportEvent],
)(using CardanoNetwork.Section)
    extends HubTransport {

    /** Routes inbound from `coil` to this hub's liaison for it, and opens that link.
      *
      * A hub runs one [[hydrozoa.multisig.consensus.liaison.PeerLiaisonHubToCoil]] per coil it
      * serves, and `coil` names which link each one owns. The accept path waits on this before it
      * hands over `coil`'s position: a coil sends that once per dial, so one delivered with nothing
      * registered is never resent and that coil never gets an answer (see [[awaitLiaison]]).
      */
    override def register(
        coil: CoilPeerNumber,
        localLiaison: LiaisonProtocol.HubLiaisonHandle
    ): IO[Unit] =
        inboundRef.update(_.updated(coil, InboundRoute.Live(localLiaison))) >>
            liaisonsUp.get(coil).traverse_(_.complete(()).void)

    override def unregister(coil: CoilPeerNumber): IO[Unit] =
        inboundRef.update(_.updated(coil, InboundRoute.Closed)) >>
            tracer.traceWith(LiaisonUnregistered(coil))

    override def send(
        coil: CoilPeerNumber,
        request: Join.Answer | LiaisonProtocol.HubEmitted
    ): IO[Unit] = {
        val line = CoilFrame.encode(CoilFrame.Msg(request))
        outboxes.get(coil) match {
            case Some(q) => q.offer(line)
            case None    => tracer.traceWith(NoOutboxForCoil(coil))
        }
    }

    private def dispatchInbound(coil: CoilPeerNumber, payload: CoilFrame.Wire): IO[Unit] =
        payload match {
            // Only the coil-emitted subset is valid inbound here.
            case p @ (_: Population.Get | _: OwnHardAck.New) => toLiaison(coil, p)
            case other =>
                tracer.traceWith(UnexpectedInboundWire(coil, other))
        }

    /** Route a frame from `coil` to this hub's liaison for it.
      *
      * The accept path holds a coil's handshake until that liaison is registered, and holds it on
      * the socket's own receive pipe, so every later frame on the link waits behind it and finds
      * the liaison too. [[NoLiaisonForInbound]] therefore reports a broken invariant, not a coil
      * that merely dialled in early. After the handoff the route is [[InboundRoute.Closed]] and
      * frames are dropped as expected — the coil does not know this hub stopped its liaison and
      * keeps pulling until it hands off itself.
      */
    private def toLiaison(
        coil: CoilPeerNumber,
        request: LiaisonProtocol.FromCoil
    ): IO[Unit] =
        inboundRef.get.map(_.get(coil)).flatMap {
            case Some(InboundRoute.Live(liaison)) => liaison ! request
            case Some(InboundRoute.Closed)        => tracer.traceWith(InboundAfterUnregister(coil))
            case None                             => tracer.traceWith(NoLiaisonForInbound(coil))
        }

    /** Block until this hub has spawned and registered **its own** liaison for `coil` — the
      * `PeerLiaisonHubToCoil` that owns this link. Nothing on the coil node is waited for.
      *
      * This server binds before [[HeadMultisigRegimeManager]] spawns those liaisons, so a coil
      * dialling into the gap is ordinary rather than a fault. Its handshake carries the position
      * the hub answers, a coil sends that position exactly once per dial, and a healthy link is
      * never redialed — so a position delivered while no liaison is registered is never resent, the
      * hub never answers, and that coil waits in join mode for good.
      *
      * ==Why it waits here, and not earlier==
      *
      * It runs after the handshake is accepted, not before. Acceptance is a verdict on the coil's
      * cryptographic proof and this node's roster, and no local wiring should be able to change it
      * — gating acceptance also hangs any caller that exercises accept and refuse without standing
      * up liaisons at all. What waits is the delivery of the position.
      *
      * ==Why the receive pipe is the right place to wait==
      *
      * A WebSocket connection has one receive pipe and processes one frame at a time, in arrival
      * order. Pausing inside the handshake therefore does not drop the frames behind it: they stay
      * queued in the socket, unread, and are delivered after the position once the wait releases.
      * That is what makes the position reach the liaison before any pull that followed it, with no
      * lock and no holding slot. The pipe is per-socket, so waiting on one coil's link leaves every
      * other coil's link running.
      *
      * {{{
      *  time │ this hub's receive pipe for coil N          route for coil N
      *  ─────┼──────────────────────────────────────────────────────────────
      *    1  │ handshake → verify proof → ACCEPT ✓          (none)
      *       │            → awaitLiaison  ⏸ BLOCKED
      *    2  │ pull #1 ─┐
      *    3  │ pull #2 ─┤  unread, queued in the socket
      *    4  │          │   register(coil N, L) ──────────▶ Live(L)
      *       │  ────────┴──▶ L ← position   (first, always)
      *    5  │               L ← pull #1
      *    6  │               L ← pull #2
      * }}}
      *
      * Unbounded on purpose: the backstop is the socket's idle timeout, which closes a connection
      * whose position never moves, and the coil's dialer redials. Past [[liaisonWaitReport]] the
      * wait is reported as [[AwaitingLiaison]], so a node stuck before registration does not read
      * as an unreachable hub.
      */
    private def awaitLiaison(coil: CoilPeerNumber): IO[Unit] =
        liaisonsUp.get(coil).traverse_ { up =>
            up.get.timeoutTo(
              liaisonWaitReport,
              tracer.traceWith(AwaitingLiaison(coil, liaisonWaitReport)) >> up.get
            )
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
                            // established session. `tryGet` then `complete` is not a race:
                            // this pipe is the only thing that completes `verdictD`, and it
                            // takes one frame at a time.
                            verdictD.tryGet.flatMap {
                                case Some(_) =>
                                    tracer.traceWith(ServerRepeatHandshake(coilNum))
                                case None =>
                                    // Traced BEFORE it is completed, because completing it is
                                    // what acts on it. A refusal goes straight out and the
                                    // socket closes, and the connection can be torn down, and
                                    // this fiber with it, before a trace placed after it runs:
                                    // a refusal on the wire that nobody logged.
                                    verdict.fold(
                                      refusal =>
                                          tracer.traceWith(
                                            ServerRefusedHandshake(coilNum, refusal)
                                          ),
                                      _ => tracer.traceWith(ServerAccepted(coilNum))
                                    ) >>
                                        verdictD.complete(verdict) >>
                                        // Bind the socket BEFORE announcing the link, so the
                                        // start point the liaison decides on has an outbox to
                                        // leave by. The announcement waits for the liaison: it
                                        // is sent once per dial, and this pipe must not run on
                                        // past it, or a later pull would reach the liaison
                                        // before the position it is pulling from.
                                        verdict.traverse_(coil =>
                                            awaitLiaison(coil) >> toLiaison(coil, marks)
                                        )
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

    /** How long a coil may sit in the accept path with no liaison registered for it before the hub
      * says so. The regime manager registers them in its `preStart`, within a second or two of the
      * server binding, so passing this means the node is stuck somewhere earlier.
      */
    val defaultLiaisonWaitReport: FiniteDuration = 5.seconds

    /** Allocate the hub-side coil transport: one outbox per hubbed coil peer + an empty inbound
      * map. The caller mounts [[routes]] on the hub's [[NodeWsServer]] and [[register]]s this hub's
      * liaison for each coil once it is spawned.
      */
    def create(
        coils: List[CoilPeerNumber],
        coilPeers: CoilPeers,
        headParamsHash: Hash32,
        ownHead: HeadIdentity,
        tracer: ContraTracer[IO, HubWsTransportEvent],
        keepAlivePing: FiniteDuration = NodeWsServer.defaultKeepAlivePing,
        liaisonWaitReport: FiniteDuration = HubWsTransport.defaultLiaisonWaitReport,
    )(using CardanoNetwork.Section): IO[HubWsTransport] =
        for {
            outboxes <- coils
                .traverse(c => Queue.unbounded[IO, String].map(c -> _))
                .map(_.toMap)
            inboundRef <- Ref[IO].of(
              Map.empty[CoilPeerNumber, InboundRoute[LiaisonProtocol.HubLiaisonHandle]]
            )
            // One per hubbed coil, allocated up front: `register` completes its coil's, and the
            // server's accept path waits on it. A coil this hub does not serve has no entry, and
            // is refused by `admit` long before anything would wait.
            liaisonsUp <- coils.traverse(c => Deferred[IO, Unit].map(c -> _)).map(_.toMap)
        } yield new HubWsTransport(
          outboxes,
          coilPeers,
          headParamsHash,
          inboundRef,
          liaisonsUp,
          ownHead,
          keepAlivePing,
          liaisonWaitReport,
          tracer
        )
}
