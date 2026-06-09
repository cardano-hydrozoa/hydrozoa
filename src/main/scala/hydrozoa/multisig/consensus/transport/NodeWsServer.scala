package hydrozoa.multisig.consensus.transport

import cats.effect.{IO, Resource}
import cats.syntax.semigroupk.*
import com.comcast.ip4s.{Host, Port}
import hydrozoa.lib.logging.Logging
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Server
import org.http4s.server.websocket.WebSocketBuilder2

/** The single WebSocket server a peer binds, shared by every WS link the peer runs. A pure head
  * peer mounts only the head-mesh route ([[PeerWsTransport.routes]]); a hub head peer mounts that
  * **plus** the hub→coil route ([[HubWsTransport.routes]]) on the same port; a coil peer runs no
  * server at all (it only dials its hub).
  *
  * Each contributor is a `WebSocketBuilder2 => HttpRoutes`; they are combined with `<+>` so the
  * paths (`/peer`, `/coil`) coexist on one Ember server.
  */
object NodeWsServer {

    def resource(
        bindHost: Host,
        bindPort: Port,
        routes: List[WebSocketBuilder2[IO] => HttpRoutes[IO]]
    ): Resource[IO, Server] = {
        val logger = Logging.loggerIO(s"NodeWsServer.$bindHost:$bindPort")
        EmberServerBuilder
            .default[IO]
            .withHost(bindHost)
            .withPort(bindPort)
            // Ember closes idle WS sockets after 60s by default. The liaison protocols' own
            // retransmit timers self-heal a stalled link, so in production we'd set this to
            // `Duration.Inf`; we leave the default in place so tests exercise reconnect handling.
            // TODO: surface as a config parameter (default `Inf` for production; short in tests).
            .withHttpWebSocketApp(wsb =>
                routes
                    .map(_(wsb))
                    .reduceOption(_ <+> _)
                    .getOrElse(HttpRoutes.empty[IO])
                    .orNotFound
            )
            .build
            .evalTap(_ => logger.info(s"WS server bound at ws://$bindHost:$bindPort"))
    }
}
