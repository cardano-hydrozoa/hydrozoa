package hydrozoa.multisig.server

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.EventSequencer
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.http4s.server.middleware.CORS
import org.typelevel.log4cats.Logger

/** HTTP server for Hydrozoa L2 event submission from end-users (or a proxy -- load-balancer,
  * unified API)
  */
object HydrozoaServer {

    private given logger: Logger[IO] = Logging.loggerIO("HydrozoaServer")

    /** Configuration for the HTTP server */
    // TODO: shall we include it into node config?
    final case class Config(
        host: Host = host"0.0.0.0",
        port: Port = port"8080"
    )

    /** Create and start the HTTP server
      *
      * @param eventSequencer
      *   Handle to the EventSequencer actor
      * @param config
      *   Server configuration
      * @return
      *   Resource that manages the server lifecycle
      */
    def create(
        eventSequencer: EventSequencer.Handle,
        headConfig: HeadConfig,
        config: Config = Config()
    ): Resource[IO, Server] =
        for {
            hydrozoaRoutes <- Resource.eval(HydrozoaRoutes(eventSequencer, headConfig))
            server <- EmberServerBuilder
                .default[IO]
                .withHost(config.host)
                .withPort(config.port)
                .withHttpApp(CORS.policy.withAllowOriginAll(hydrozoaRoutes.routes.orNotFound))
                .withHttpApp(hydrozoaRoutes.routes.orNotFound)
                .build
                .evalTap { _ =>
                    logger.info(
                      s"Hydrozoa HTTP server started at http://${config.host}:${config.port}"
                    )
                }
        } yield server

    /** Run the server (for standalone use)
      *
      * Note: In production, this would be integrated into HydrozoaNode
      */
    def run(
        eventSequencer: EventSequencer.Handle,
        headConfig: HeadConfig,
        config: Config = Config()
    ): IO[Nothing] = {
        create(eventSequencer, headConfig, config)
            .use(_ => IO.never)
    }
}
