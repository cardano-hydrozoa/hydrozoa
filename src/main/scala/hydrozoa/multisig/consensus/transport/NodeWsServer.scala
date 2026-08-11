package hydrozoa.multisig.consensus.transport

import cats.effect.{IO, Resource}
import cats.syntax.semigroupk.*
import com.comcast.ip4s.{Host, Port}
import fs2.Stream
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.transport.NodeWsServerEvent.Bound
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
            .withHttpWebSocketApp(wsb =>
                routes
                    .map(_(wsb))
                    .reduceOption(_ <+> _)
                    .getOrElse(HttpRoutes.empty[IO])
                    .orNotFound
            )
            .build
            .evalTap(_ => tracer.traceWith(Bound(bindHost, bindPort)))

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
