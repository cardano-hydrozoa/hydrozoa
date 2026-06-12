package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.liaison.BatchMessages.{OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonCoilToHub}
import hydrozoa.multisig.consensus.peer.CoilPeerNumber
import org.http4s.Uri
import org.http4s.client.websocket.{WSClient, WSFrame, WSRequest}
import scala.concurrent.duration.*

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
    private val hubUri: Uri,
    private val outbox: Queue[IO, String],
    private val inboundRef: Ref[IO, Option[PeerLiaisonCoilToHub.Handle]],
)(using CardanoNetwork.Section) {

    private val logger = Logging.loggerIO(s"CoilPeerWsTransport.c${ownCoilNum.convert}")

    /** Wire the local PeerLiaisonCoilToHub as the inbound dispatch target. Must be called before
      * the link starts receiving traffic.
      */
    def register(localLiaison: PeerLiaisonCoilToHub.Handle): IO[Unit] =
        inboundRef.set(Some(localLiaison))

    /** Enqueue a coil→hub batch for delivery to the hub. */
    def send(request: LiaisonProtocol.HubToCoilRequest): IO[Unit] =
        CoilFrame.fromWire(request) match {
            case Some(wire) => outbox.offer(CoilFrame.encode(CoilFrame.Msg(wire)))
            case None       => logger.warn(s"send: dropping non-wire request to hub: $request")
        }

    private def dispatchInbound(payload: CoilFrame.Wire): IO[Unit] =
        payload match {
            // Only the hub-emitted subset is valid inbound here.
            case p @ (_: Population.New | _: OwnHardAck.Get) =>
                inboundRef.get.flatMap {
                    case Some(liaison) => liaison ! p
                    case None          => logger.warn("inbound from hub but no liaison registered")
                }
            case other => logger.warn(s"unexpected coil-bound wire from hub: $other")
        }

    private def onLine(s: String): IO[Unit] =
        CoilFrame.parse(s) match {
            case Right(CoilFrame.Msg(payload)) => dispatchInbound(payload)
            case Right(CoilFrame.Hello(_))     => IO.unit
            case Left(err) =>
                logger.warn(s"failed to decode coil frame from hub: ${err.getMessage}")
        }

    private def dialerLoop(client: WSClient[IO]): IO[Nothing] = {
        val request = WSRequest(hubUri)

        def once: IO[Unit] =
            client.connectHighLevel(request).use { conn =>
                val helloLine = CoilFrame.encode(CoilFrame.Hello(ownCoilNum.convert))
                logger.info(s"dialer: connected to hub at $hubUri") >>
                    conn.send(WSFrame.Text(helloLine)) >>
                    WsDuplex.run(conn, outbox, onLine)
            }

        val attempt: IO[Unit] =
            once.handleErrorWith(e => logger.warn(s"dialer to hub failed: ${e.getMessage}"))

        (attempt >> IO.sleep(1.second)).foreverM
    }

    /** Launch the hub dialer fiber; torn down when the resource is released. */
    def startDialer(client: WSClient[IO]): Resource[IO, Unit] =
        Resource.make(dialerLoop(client).start)(_.cancel).void
}

object CoilPeerWsTransport {

    def create(
        ownCoilNum: CoilPeerNumber,
        hubUri: Uri,
    )(using CardanoNetwork.Section): IO[CoilPeerWsTransport] =
        for {
            outbox <- Queue.unbounded[IO, String]
            inboundRef <- Ref[IO].of(Option.empty[PeerLiaisonCoilToHub.Handle])
        } yield new CoilPeerWsTransport(ownCoilNum, hubUri, outbox, inboundRef)
}
