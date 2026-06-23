package hydrozoa.multisig.server

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.{BlockWeaver, RequestSequencer}
import hydrozoa.multisig.server.HydrozoaHttpEvent.ServerStarted
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.http4s.server.middleware.CORS

/** HTTP server for Hydrozoa L2 event submission from end-users (or a proxy -- load-balancer,
  * unified API)
  */
object HydrozoaServer {

    /** Configuration for the HTTP server */
    // TODO: shall we include it into node config?
    final case class Config(
        host: Host = host"0.0.0.0",
        port: Port = port"8080",
        adminUsername: String,
        adminPassword: String
    )

    /** Create and start the HTTP server.
      *
      * @param requestSequencer
      *   Handle to the RequestSequencer actor
      * @param blockWeaver
      *   Handle to the BlockWeaver actor
      * @param headConfig
      *   Head configuration
      * @param config
      *   Server configuration
      * @param tracer
      *   sink for HTTP-server and per-route events
      * @return
      *   Resource that manages the server lifecycle
      */
    def create(
        requestSequencer: RequestSequencer.Handle,
        blockWeaver: BlockWeaver.Handle,
        headConfig: HeadConfig,
        config: Config,
        tracer: ContraTracer[IO, HydrozoaHttpEvent]
    ): Resource[IO, Server] =
        for {
            hydrozoaRoutes <- Resource.eval(
              HydrozoaRoutes(requestSequencer, blockWeaver, headConfig, config, tracer)
            )
            server <- EmberServerBuilder
                .default[IO]
                .withHost(config.host)
                .withPort(config.port)
                .withHttpApp(CORS.policy.withAllowOriginAll(hydrozoaRoutes.routes.orNotFound))
                .build
                .evalTap(_ => tracer.traceWith(ServerStarted(config.host, config.port)))
        } yield server

    /** Run the server (for standalone use)
      *
      * Note: In production, this would be integrated into HydrozoaNode
      */
    def run(
        requestSequencer: RequestSequencer.Handle,
        blockWeaver: BlockWeaver.Handle,
        headConfig: HeadConfig,
        config: Config,
        tracer: ContraTracer[IO, HydrozoaHttpEvent]
    ): IO[Nothing] = {
        create(requestSequencer, blockWeaver, headConfig, config, tracer)
            .use(_ => IO.never)
    }
}
