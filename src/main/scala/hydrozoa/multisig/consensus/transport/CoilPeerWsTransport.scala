package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.{Deferred, FiberIO, IO, Ref, Resource}
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.QuietRelease
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.BatchMessages.{Join, OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, PeerWallet}
import hydrozoa.multisig.consensus.transport.CoilPeerWsTransportEvent.*
import org.http4s.Uri
import org.http4s.client.websocket.{WSClient, WSFrame, WSRequest}
import scala.concurrent.duration.*
import scalus.cardano.ledger.Hash32

/** The coil side of a hub↔coil link, in the abstract. Concrete impls: [[CoilPeerWsTransport]] (real
  * WS) and [[InProcessHubCoilTransport.Coil]] (test harness).
  */
trait CoilTransport {

    /** Wire the local [[PeerLiaisonCoilToHub]] as the inbound dispatch target. Must be called
      * before the link starts receiving traffic.
      */
    def register(localLiaison: LiaisonProtocol.CoilLiaisonHandle): IO[Unit]

    /** Stop dispatching inbound to the liaison [[register]] wired, before that liaison is stopped.
      * The hub does not know this coil stopped it and keeps sending; from now on its frames are
      * dropped as expected rather than delivered to a stopped actor.
      */
    def unregister: IO[Unit]

    /** Enqueue a coil→hub batch for delivery to the hub. */
    def send(request: LiaisonProtocol.CoilEmitted): IO[Unit]

    /** Announce where this coil stands, as its liaison enters join mode.
      *
      * A dialing transport sends its marks in the handshake on every dial and has nothing to learn
      * here. A directly-wired one never dials, so this is the only point at which it can be told.
      */
    def announceMarks(marks: Join.Connected): IO[Unit]
}

/** The coil side of the hub→coil WS link: a coil peer runs no server, it dials its single hub's
  * `/hub` endpoint and keeps the link alive with reconnect-on-drop. It answers the hub's
  * [[CoilFrame.Challenge]] with a [[CoilFrame.Handshake]] proving this coil's [[CoilPeerNumber]]
  * under `ownWallet`, and the hub binds the socket to that number.
  *
  * Outbound is the coil-emitted subset ([[Population.Get]] / [[OwnHardAck.New]]); inbound is the
  * hub-emitted subset ([[Join.Offer]], [[Population.New]], [[OwnHardAck.Get]]), routed to the local
  * [[PeerLiaisonCoilToHub]].
  */
final class CoilPeerWsTransport private (
    private val ownCoilNum: CoilPeerNumber,
    private val ownWallet: PeerWallet,
    private val headParamsHash: Hash32,
    private val ownMarks: IO[Join.Connected],
    private val ownHead: HeadIdentity,
    private val outbox: Queue[IO, String],
    private val inboundRef: Ref[IO, CoilPeerWsTransport.Inbound],
    private val tracer: ContraTracer[IO, CoilPeerWsTransportEvent],
)(using CardanoNetwork.Section)
    extends CoilTransport {
    import CoilPeerWsTransport.Inbound

    /** Hands the liaison any join answer that arrived before it, then routes inbound to it.
      *
      * One compare-and-set takes the held answer and publishes the handle together, so a frame
      * being routed concurrently either finds the handle or holds its answer where this will see it
      * — never both.
      */
    override def register(localLiaison: LiaisonProtocol.CoilLiaisonHandle): IO[Unit] =
        inboundRef
            .modify(s => (Inbound(Some(InboundRoute.Live(localLiaison)), None), s.heldAnswer))
            .flatMap(_.traverse_(localLiaison ! _))

    override def unregister: IO[Unit] =
        inboundRef.update(_.copy(route = Some(InboundRoute.Closed))) >>
            tracer.traceWith(LiaisonUnregistered)

    override def send(request: LiaisonProtocol.CoilEmitted): IO[Unit] =
        outbox.offer(CoilFrame.encode(CoilFrame.Msg(request)))

    /** No-op: this transport announces its marks in the handshake it sends on every dial, read
      * fresh from the store at that moment ([[ownMarks]]), which is strictly better than a value
      * captured earlier by a caller.
      */
    override def announceMarks(marks: Join.Connected): IO[Unit] = IO.unit

    /** Route a hub frame to the registered liaison.
      *
      * Before registration, a join answer is **held** (the latest one) rather than dropped. The hub
      * answers once per dial and a healthy link is never redialed, while a cold coil waits for its
      * answer indefinitely, so a dropped answer is never replaced and the coil never boots. The
      * dialer can be up before the node has built and registered its liaison, so this ordering does
      * occur. Pull traffic is dropped: it answers or asks for a pull no current liaison made, and
      * the pullers resend.
      *
      * An answer is held the same way after the liaison was unregistered, since a later register
      * would otherwise wait on an answer it never saw. Pull traffic then is the hub not knowing
      * this coil handed off, and is dropped as expected rather than as a fault.
      *
      * The decision is one compare-and-set on [[Inbound]] and the send is made after it, so a frame
      * that arrives while [[register]] is publishing its handle is either delivered or held for it,
      * never lost between the two. The send order is not preserved: a serving frame routed
      * concurrently may reach the liaison before the held answer does, which join mode is built for
      * — it ignores serving traffic until the answer seats it.
      */
    private def toLiaison(request: LiaisonProtocol.FromHub): IO[Unit] =
        inboundRef.modify { s =>
            s.route match {
                case Some(InboundRoute.Live(liaison)) => (s, liaison ! request)
                case route =>
                    request match {
                        case answer: (Join.Offer | Join.NoOffer) =>
                            (s.copy(heldAnswer = Some(answer)), tracer.traceWith(JoinAnswerHeld))
                        case _ =>
                            route match {
                                case Some(InboundRoute.Closed) =>
                                    (s, tracer.traceWith(InboundAfterUnregister))
                                case _ => (s, tracer.traceWith(NoLiaisonForInbound))
                            }
                    }
            }
        }.flatten

    private def dispatchInbound(payload: CoilFrame.Wire): IO[Unit] =
        payload match {
            // The answer goes to the liaison like any other inbound frame: it is in join mode
            // waiting for exactly this, and a later one reaching it in regular mode is a late
            // redial it declines.
            case a @ (_: Join.Offer | _: Join.NoOffer) => toLiaison(a)
            // Only the hub-emitted subset is valid inbound here.
            case p @ (_: Population.New | _: OwnHardAck.Get) => toLiaison(p)
            case other => tracer.traceWith(UnexpectedInboundWire(other))
        }

    private def onLine(s: String): IO[Unit] =
        CoilFrame.parse(s) match {
            case Right(CoilFrame.Msg(payload)) => dispatchInbound(payload)
            // The hub's verdict on the handshake this attempt just sent: it refused, and the socket
            // is about to close. The dialer redials regardless — a refusal describes this attempt,
            // not the hub — so the trace is what tells an operator to go fix a config.
            case Right(CoilFrame.Refused(refusal)) => tracer.traceWith(DialerRefused(refusal))
            // A challenge is answered before the duplex starts, and a hub sends exactly one; a
            // second is the hub misbehaving, not a renegotiation.
            case Right(_: CoilFrame.Challenge) => tracer.traceWith(DialerLateChallenge)
            case Right(_: CoilFrame.Handshake) => IO.unit
            case Left(err)                     => tracer.traceWith(DecodeError(err))
        }

    /** The hub's opening challenge, or `None` if the hub opened with something else. */
    private def challenge(line: String): Option[CoilFrame.Challenge] =
        CoilFrame.parse(line).toOption.collect { case c: CoilFrame.Challenge => c }

    /** How long one dial attempt may sit in the WebSocket **handshake** before it is abandoned.
      * Long enough not to give up on an ordinarily slow hub, short enough that a stalled one does
      * not stop this peer reconnecting. Bounds the handshake alone: an established connection is
      * unbounded, and [[WsDuplex.defaultReadIdleTimeout]] is what catches a half-open one.
      */
    private val handshakeBudget: FiniteDuration = 30.seconds

    /** How long one dial attempt waits for the hub's [[CoilFrame.Challenge]] once the WebSocket
      * handshake has completed.
      *
      * The hub emits the challenge as the first frame of an accepted socket, so anything past a
      * round trip means it is not answering. Must stay **under** [[handshakeBudget]]: this wait
      * happens after the claim on `handshook` is still open, so an attempt that outlived the budget
      * would sit here holding an open socket while the loop had already redialed.
      */
    private val challengeBudget: FiniteDuration = 10.seconds

    /** How long teardown waits for the dialer to acknowledge cancellation before proceeding. */
    private val dialerCancelBudget: FiniteDuration = 5.seconds

    private def dialerLoop(client: WSClient[IO], hubUri: Uri): IO[Nothing] = {
        val request = WSRequest(hubUri)

        // Low-level `connect`, not `connectHighLevel`: the dialer needs to see the hub's keep-alive
        // Ping to know the link is alive (see WsDuplex).
        // `handshook` arbitrates between this attempt and the loop's budget: whoever completes it
        // first decides whether the socket is used or dropped. `complete` has exactly one winner,
        // so a handshake landing on the deadline cannot leave both a live connection and a redial.
        //
        // The hub speaks first: its challenge is what this attempt's proof is signed over, so the
        // socket is claimed only once there is a nonce to answer. An attempt that never gets one
        // returns and `use` closes the socket — which is also what reclaims the demand
        // `WsDuplex.firstLine` leaks when its deadline cancels a read.
        def once(handshook: Deferred[IO, Unit]): IO[Unit] =
            QuietRelease(client.connect(request)).use { conn =>
                WsDuplex.firstLine(conn, challengeBudget).map(_.flatMap(challenge)).flatMap {
                    case None => tracer.traceWith(DialerNoChallenge(hubUri, challengeBudget))
                    case Some(CoilFrame.Challenge(nonce, protocolVersion)) =>
                        ProtocolVersion.check(protocolVersion) match {
                            // Refuse before answering. Signing a proof for a hub this coil cannot
                            // talk to is wasted work, and deciding here is what lets the coil name
                            // both versions against a hub that never says why — which is what a
                            // hub too old to send `Refused` does.
                            case ProtocolVersion.Check.Incompatible(found, expected) =>
                                tracer.traceWith(
                                  DialerRefusedChallenge(
                                    HandshakeRefusal.ProtocolVersionMismatch(found, expected)
                                  )
                                )
                            case ProtocolVersion.Check.Compatible =>
                                handshook.complete(()).flatMap {
                                    case true =>
                                        // Read the marks per dial, not once at construction: a
                                        // redial after a long drop must claim where the coil
                                        // stands NOW, or the hub decides the start point from a
                                        // stale position.
                                        tracer.traceWith(DialerConnected(hubUri)) >>
                                            ownMarks
                                                .map(marks =>
                                                    CoilFrame.encode(
                                                      CoilFrame.Handshake.own(
                                                        ownCoilNum.convert,
                                                        ownWallet,
                                                        headParamsHash,
                                                        nonce,
                                                        marks,
                                                        ownHead
                                                      )
                                                    )
                                                )
                                                .flatMap(line => conn.send(WSFrame.Text(line))) >>
                                            WsDuplex.run(conn, outbox, onLine)
                                    // Lost the claim: the budget expired and the loop has already
                                    // redialed. Return instead, so `use` closes this socket rather
                                    // than leaving a second live connection draining the shared
                                    // outbox.
                                    case false => tracer.traceWith(DialerHandshakeLate(hubUri))
                                }
                        }
                }
            }

        def attempt(handshook: Deferred[IO, Unit]): IO[Unit] =
            (once(handshook) >> tracer.traceWith(DialerDisconnected(hubUri)))
                .handleErrorWith(e => tracer.traceWith(DialerFailed(e)))

        // Wait out a connection this loop owns. `onCancel` is what keeps teardown closing the
        // socket: the attempt runs on its own fiber, so cancelling this loop cancels the JOIN and
        // not the attempt behind it, and a live `WsDuplex.run` would otherwise outlive
        // `startDialer`'s release with its connection still open. Safe to cancel here and only
        // here — past the claim the uncancelable acquire is long finished.
        def own(f: FiberIO[Unit]): IO[Unit] = f.join.void.onCancel(f.cancel)

        // Bound the HANDSHAKE, and only the handshake. `JdkWSClient` builds its socket inside
        // `Resource.make`'s acquire, which is uncancelable, and `fromCompletableFuture` is
        // cooperative-only — so a hub that accepts the TCP connection and never answers the
        // handshake blocks the attempt forever, the loop never iterates, and the peer stops
        // reconnecting entirely: not slow to recover, never retrying.
        //
        // What must NOT be bounded is what comes after. `WsDuplex.run` occupies the attempt for the
        // whole life of a healthy link — the hub pings well inside the read deadline, so it never
        // returns on its own — and timing the attempt as a whole therefore abandons a *working*
        // connection every `handshakeBudget` and dials a second one on top of it.
        //
        // A stalled attempt is abandoned, not cancelled: the acquire it is stuck in cannot be
        // interrupted. It disowns itself through `handshook` if it ever does complete.
        val bounded: IO[Unit] =
            for {
                handshook <- Deferred[IO, Unit]
                f <- attempt(handshook).start
                _ <- IO.race(IO.race(handshook.get, f.join), IO.sleep(handshakeBudget)).flatMap {
                    // Connected inside the budget: this attempt owns the dialer until the link
                    // ends, and waiting on it is the point.
                    case Left(Left(_)) => own(f)
                    // Ended before it ever connected — refused, DNS, TLS. Redial.
                    case Left(Right(_)) => IO.unit
                    case Right(_) =>
                        handshook.complete(()).flatMap {
                            case true =>
                                tracer.traceWith(DialerHandshakeStalled(hubUri, handshakeBudget))
                            // It handshook on the deadline and claimed first after all, so it owns
                            // the link and there is nothing to redial.
                            case false => own(f)
                        }
                }
            } yield ()

        (bounded >> IO.sleep(1.second)).foreverM
    }

    /** Launch the hub dialer fiber; torn down when the resource is released. The hub URI is passed
      * at dial-start time so the caller can discover the hub's OS-assigned port after binding.
      */
    def startDialer(client: WSClient[IO], hubUri: Uri): Resource[IO, Unit] =
        Resource
            .make(dialerLoop(client, hubUri).start)(fiber =>
                // `cancel` waits for the fiber to finalize and is itself uncancelable, so a dialer
                // stuck in an uncancelable handshake acquire would hang teardown — the same shape as
                // the boot barriers. Run the cancel on its own fiber and bound the JOIN, which IS
                // cancelable. A healthy dialer sits in `IO.sleep` and completes this immediately.
                fiber.cancel.start
                    .flatMap(_.join.timeoutTo(dialerCancelBudget, IO.unit))
                    .void
            )
            .void
}

object CoilPeerWsTransport {

    /** Where this link's inbound goes, and the join answer it is holding for a liaison not yet
      * registered. The liaison in question is **this coil node's own**
      * [[hydrozoa.multisig.consensus.liaison.PeerLiaisonCoilToHub]]; a coil runs exactly one, and
      * [[hydrozoa.multisig.CoilMultisigRegimeManager]] spawns it in `preStart`.
      *
      * ==Why the answer is held rather than waited for==
      *
      * The hub's answer arrives on an already-established socket, so unlike the hub — which reads a
      * coil's position off a handshake it is still processing, and can simply pause there — this
      * end has nothing in flight to pause. The dialer opens the link before the regime manager has
      * finished building actors, and the hub answers a join exactly once per dial, so the answer is
      * kept until there is a liaison to give it to.
      *
      * ==Why route and held answer are one value==
      *
      * Two fibers touch this state: a reader routing an inbound frame, and
      * [[CoilPeerWsTransport.register]] publishing a liaison. Held in two separate refs, they
      * interleave into a lost update:
      *
      * {{{
      *   reader fiber (an answer arrives)      register fiber
      *   ────────────────────────────────      ──────────────────────────────
      *   read route       → empty
      *                                         read heldAnswer → None    ← looks, finds nothing
      *                                         set  route      → Live(L)
      *   set  heldAnswer  → Some(answer)                                 ← stores, too late
      *
      *   result: a live liaison, and an answer nobody will ever hand it.
      * }}}
      *
      * As one value, a routing decision and a `register` are each a single compare-and-set, and
      * that interleaving cannot be expressed: `register` publishes the route and takes the held
      * answer in the same swap, so whichever fiber wins, the answer lands.
      *
      * Send order is not preserved — a serving frame routed concurrently may reach the liaison
      * ahead of the held answer. Join mode is built for that: it ignores serving traffic until the
      * answer seats it, and the hub's puller retransmits whatever it dropped.
      */
    final case class Inbound(
        route: Option[InboundRoute[LiaisonProtocol.CoilLiaisonHandle]],
        heldAnswer: Option[Join.Answer]
    )

    /** @param ownWallet
      *   this coil peer's signing wallet — the same key `coilPeers` lists for `ownCoilNum`, and the
      *   one its hard acks are signed with.
      * @param headParamsHash
      *   this node's digest over the whole head config, asserted in every handshake so the hub
      *   refuses a coil from another head, or one that disagrees about this one, at connect.
      * @param ownMarks
      *   where this coil stands, re-read on every dial. See [[CoilFrame.Handshake]].
      */
    def create(
        ownCoilNum: CoilPeerNumber,
        ownWallet: PeerWallet,
        headParamsHash: Hash32,
        ownMarks: IO[Join.Connected],
        ownHead: HeadIdentity,
        tracer: ContraTracer[IO, CoilPeerWsTransportEvent],
    )(using CardanoNetwork.Section): IO[CoilPeerWsTransport] =
        for {
            outbox <- Queue.unbounded[IO, String]
            inboundRef <- Ref[IO].of(Inbound(None, None))
        } yield new CoilPeerWsTransport(
          ownCoilNum,
          ownWallet,
          headParamsHash,
          ownMarks,
          ownHead,
          outbox,
          inboundRef,
          tracer
        )
}
