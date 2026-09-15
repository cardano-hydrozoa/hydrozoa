package hydrozoa.multisig.consensus.transport

import cats.effect.{IO, Resource}
import cats.syntax.semigroupk.*
import com.comcast.ip4s.{Host, Port}
import fs2.Stream
import fs2.io.net.SocketOption
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.transport.NodeWsServerEvent.Bound
import java.nio.charset.StandardCharsets.UTF_8
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Server
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}

/** The single WebSocket server a peer binds, shared by every WS link the peer runs. A pure head
  * peer mounts only the head-mesh route ([[WsPeerTransport.routes]]); a hub head peer mounts that
  * **plus** the hub→coil route ([[HubWsTransport.routes]]) on the same port; a coil peer runs no
  * server at all (it only dials its hub).
  *
  * Each contributor is a `WebSocketBuilder2 => HttpRoutes`; they are combined with `<+>` so the
  * paths (`/head`, `/hub`) coexist on one Ember server.
  */
object NodeWsServer {

    /** Keep-alive ping interval, comfortably below `resource`'s default `idleTimeout` so the
      * ping/pong round-trip resets the read-idle timer before it fires. The transports always ping
      * at this interval (prod and tests alike, so tests match production).
      */
    val defaultKeepAlivePing: FiniteDuration = 10.seconds

    def resource(
        bindHost: Host,
        bindPort: Port,
        routes: List[WebSocketBuilder2[IO] => HttpRoutes[IO]],
        tracer: ContraTracer[IO, NodeWsServerEvent],
        idleTimeout: Duration = 20.seconds,
    ): Resource[IO, Server] =
        EmberServerBuilder
            .default[IO]
            .withHost(bindHost)
            .withPort(bindPort)
            // Close a WS socket after `idleTimeout` of I/O inactivity (Ember's own default is 60s).
            // A short 20s window is fine because the transports send sub-`idleTimeout` keep-alive
            // pings, so an idle-but-live link stays open while a genuinely dead peer is dropped
            // within the window.
            .withIdleTimeout(idleTimeout)
            // Don't wait for open connections to drain on shutdown — by the time the Resource is
            // released the protocol is complete and there is nothing left to deliver.
            .withShutdownTimeout(Duration.Zero)
            // TCP_NODELAY: Ember writes a WS frame's header and payload as separate socket writes,
            // and the mesh traffic is request/reply-shaped (brief out, acks back). With Nagle on,
            // that write-write-read pattern can stall each reply up to a delayed-ACK timeout
            // (~40ms measured on loopback) whenever the link is otherwise quiet. The frames are
            // small and latency-sensitive, so disable coalescing.
            .withAdditionalSocketOptions(List(SocketOption.noDelay(true)))
            .withHttpWebSocketApp(wsb =>
                routes
                    .map(_(wsb))
                    .reduceOption(_ <+> _)
                    .getOrElse(HttpRoutes.empty[IO])
                    .orNotFound
            )
            .build
            .evalTap(_ => tracer.traceWith(Bound(bindHost, bindPort)))

    /** WebSocket close status for a refused handshake: **1008**, policy violation. The peer met the
      * WebSocket protocol and then failed ours, which is exactly what 1008 is for.
      */
    val policyViolation: Int = 1008

    /** A close frame carrying `reason`, to end a send stream on a refusal.
      *
      * RFC 6455 caps a close frame's payload at 125 bytes, two of which are the status code, so a
      * long reason is truncated rather than dropped — the structured refusal rides its own frame
      * ahead of this one, and this is the part a peer's own WebSocket stack surfaces.
      */
    def closeFrame(reason: String): WebSocketFrame =
        WebSocketFrame
            .Close(policyViolation, truncateToCloseReason(reason))
            .getOrElse(WebSocketFrame.Close())

    /** The longest prefix of `reason` that fits a close frame's payload, cut on a character
      * boundary. Counted in **encoded bytes**, not characters: the refusal strings carry arrows and
      * other non-ASCII, and a character-count cut would build a frame the encoder then rejects
      * whole.
      */
    private def truncateToCloseReason(reason: String): String = {
        val bytes = reason.getBytes(UTF_8)
        if bytes.length <= closeReasonBudget then reason
        else {
            // Back off any UTF-8 continuation bytes so the cut lands on a character boundary.
            val end = (closeReasonBudget to 0 by -1)
                .find(i => i == 0 || (bytes(i) & 0xc0) != 0x80)
                .getOrElse(0)
            new String(bytes, 0, end, UTF_8)
        }
    }

    /** A close frame's payload is 125 bytes, two of which carry the status code. */
    private val closeReasonBudget: Int = 123

    /** Merge periodic `Ping` frames into a WS server send stream so an idle-but-live link isn't
      * dropped by `resource`'s `idleTimeout` (nor by NAT/proxy idle timeouts on the path) during
      * the long L1-bound quiet periods the liaison protocols have. The peer auto-responds with
      * `Pong`, which resets the server's read-idle timer. Pick `pingEvery < idleTimeout`.
      */
    def withKeepAlive(
        pingEvery: FiniteDuration
    )(send: Stream[IO, WebSocketFrame]): Stream[IO, WebSocketFrame] =
        send.merge(Stream.awakeEvery[IO](pingEvery).as(WebSocketFrame.Ping()))
}
