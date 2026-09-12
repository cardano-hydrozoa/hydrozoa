package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.{Deferred, FiberIO, IO, Ref, Resource}
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.QuietRelease
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.liaison.BatchMessages.{OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonCoilToHub}
import hydrozoa.multisig.consensus.peer.CoilPeerNumber
import hydrozoa.multisig.consensus.transport.CoilPeerWsTransportEvent.*
import org.http4s.Uri
import org.http4s.client.websocket.{WSClient, WSFrame, WSRequest}
import scala.concurrent.duration.*

/** The coil side of a hub↔coil link, in the abstract. Concrete impls: [[CoilPeerWsTransport]] (real
  * WS) and [[InProcessHubCoilTransport.Coil]] (test harness).
  */
trait CoilTransport {

    /** Wire the local [[PeerLiaisonCoilToHub]] as the inbound dispatch target. Must be called
      * before the link starts receiving traffic.
      */
    def register(localLiaison: PeerLiaisonCoilToHub.Handle): IO[Unit]

    /** Enqueue a coil→hub batch for delivery to the hub. */
    def send(request: LiaisonProtocol.HubToCoilRequest): IO[Unit]
}

/** The coil side of the hub→coil WS link: a coil peer runs no server, it dials its single hub's
  * `/hub` endpoint and keeps the link alive with reconnect-on-drop. It identifies itself with
  * [[CoilFrame.Hello]] so the hub binds the socket to this coil's [[CoilPeerNumber]].
  *
  * Outbound is the coil-emitted subset ([[Population.Get]] / [[OwnHardAck.New]]); inbound is the
  * hub-emitted subset ([[Population.New]] / [[OwnHardAck.Get]]), routed to the local
  * [[PeerLiaisonCoilToHub]].
  */
final class CoilPeerWsTransport private (
    private val ownCoilNum: CoilPeerNumber,
    private val outbox: Queue[IO, String],
    private val inboundRef: Ref[IO, Option[PeerLiaisonCoilToHub.Handle]],
    private val tracer: ContraTracer[IO, CoilPeerWsTransportEvent],
)(using CardanoNetwork.Section)
    extends CoilTransport {

    override def register(localLiaison: PeerLiaisonCoilToHub.Handle): IO[Unit] =
        inboundRef.set(Some(localLiaison))

    override def send(request: LiaisonProtocol.HubToCoilRequest): IO[Unit] =
        CoilFrame.fromWire(request) match {
            case Some(wire) => outbox.offer(CoilFrame.encode(CoilFrame.Msg(wire)))
            case None       => tracer.traceWith(DroppingNonWireRequest(request))
        }

    private def dispatchInbound(payload: CoilFrame.Wire): IO[Unit] =
        payload match {
            // Only the hub-emitted subset is valid inbound here.
            case p @ (_: Population.New | _: OwnHardAck.Get) =>
                inboundRef.get.flatMap {
                    case Some(liaison) => liaison ! p
                    case None          => tracer.traceWith(NoLiaisonForInbound)
                }
            case other => tracer.traceWith(UnexpectedInboundWire(other))
        }

    private def onLine(s: String): IO[Unit] =
        CoilFrame.parse(s) match {
            case Right(CoilFrame.Msg(payload)) => dispatchInbound(payload)
            case Right(CoilFrame.Hello(_))     => IO.unit
            case Left(err)                     => tracer.traceWith(DecodeError(err))
        }

    /** How long one dial attempt may sit in the WebSocket **handshake** before it is abandoned.
      * Long enough not to give up on an ordinarily slow hub, short enough that a stalled one does
      * not stop this peer reconnecting. Bounds the handshake alone: an established connection is
      * unbounded, and [[WsDuplex.defaultReadIdleTimeout]] is what catches a half-open one.
      */
    private val handshakeBudget: FiniteDuration = 30.seconds

    /** How long teardown waits for the dialer to acknowledge cancellation before proceeding. */
    private val dialerCancelBudget: FiniteDuration = 5.seconds

    private def dialerLoop(client: WSClient[IO], hubUri: Uri): IO[Nothing] = {
        val request = WSRequest(hubUri)

        // Low-level `connect`, not `connectHighLevel`: the dialer needs to see the hub's keep-alive
        // Ping to know the link is alive (see WsDuplex).
        // `handshook` arbitrates between this attempt and the loop's budget: whoever completes it
        // first decides whether the socket is used or dropped. `complete` has exactly one winner,
        // so a handshake landing on the deadline cannot leave both a live connection and a redial.
        def once(handshook: Deferred[IO, Unit]): IO[Unit] =
            QuietRelease(client.connect(request)).use { conn =>
                val helloLine = CoilFrame.encode(CoilFrame.Hello(ownCoilNum.convert))
                handshook.complete(()).flatMap {
                    case true =>
                        tracer.traceWith(DialerConnected(hubUri)) >>
                            conn.send(WSFrame.Text(helloLine)) >>
                            WsDuplex.run(conn, outbox, onLine)
                    // Lost the claim: the budget expired and the loop has already redialed. Return
                    // instead, so `use` closes this socket rather than leaving a second live
                    // connection draining the shared outbox.
                    case false => tracer.traceWith(DialerHandshakeLate(hubUri))
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

    def create(
        ownCoilNum: CoilPeerNumber,
        tracer: ContraTracer[IO, CoilPeerWsTransportEvent],
    )(using CardanoNetwork.Section): IO[CoilPeerWsTransport] =
        for {
            outbox <- Queue.unbounded[IO, String]
            inboundRef <- Ref[IO].of(Option.empty[PeerLiaisonCoilToHub.Handle])
        } yield new CoilPeerWsTransport(ownCoilNum, outbox, inboundRef, tracer)
}
