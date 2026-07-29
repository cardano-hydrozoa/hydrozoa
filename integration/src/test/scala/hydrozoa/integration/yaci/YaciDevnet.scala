package hydrozoa.integration.yaci

import cats.effect.{IO, Resource}
import com.dimafeng.testcontainers.GenericContainer
import org.testcontainers.containers.wait.strategy.Wait
import scala.concurrent.Await
import scala.concurrent.duration.*
import sttp.client4.*
import sttp.model.StatusCode

/** A Testcontainers-managed Yaci DevKit devnet.
  *
  * Runs `bloxbean/yaci-cli` with `create-node -o --start` as the container's main process (it
  * blocks, keeping the container alive), waits until the admin API reports `initialized` and the
  * Yaci Store (Blockfrost API) serves, and yields a [[DevKit]] handle bound to the container's
  * mapped Blockfrost (8080) and admin (10000) ports.
  *
  * Requires Docker on the host, so the suites using it are excluded from the default test run (see
  * build.sbt).
  */
object YaciDevnet {

    /** Yaci DevKit publishes no `latest` tag, and only `0.10.x` has a multi-arch manifest; pin it.
      */
    val defaultImageTag: String = "0.10.6"

    private val blockfrostPort = 8080
    private val adminPort = 10000

    /** Acquire a running devnet as a [[DevKit]] handle; the container is stopped on release. */
    def resource(
        imageTag: String = defaultImageTag,
        startupTimeout: FiniteDuration = 3.minutes,
    ): Resource[IO, DevKit] =
        Resource
            .make(IO.blocking {
                val c = GenericContainer(
                  dockerImage = s"bloxbean/yaci-cli:$imageTag",
                  exposedPorts = Seq(blockfrostPort, adminPort),
                  // `yaci_store_enabled` is what brings up the Blockfrost API on 8080; without it
                  // only the node + admin API start. `native` mode matches the DevKit compose.
                  env = Map(
                    "yaci_store_enabled" -> "true",
                    "yaci_cli_mode" -> "native",
                    "yaci_store_mode" -> "native",
                  ),
                  command = Seq("create-node", "-o", "--start"),
                  waitStrategy = Wait
                      .forHttp("/local-cluster/api/admin/devnet/status")
                      .forPort(adminPort)
                      .forResponsePredicate(_.contains("initialized"))
                      .withStartupTimeout(java.time.Duration.ofMillis(startupTimeout.toMillis)),
                )
                // The image's default CMD is `sleep infinity`; override the entrypoint to the CLI
                // launcher so `command` above starts the devnet instead.
                c.container.withCreateContainerCmdModifier { cmd =>
                    val _ = cmd.withEntrypoint("/app/yaci-cli.sh")
                }
                c.start()
                c
            })(c => IO.blocking(c.stop()))
            .evalMap { c =>
                val host = c.container.getHost
                val devKit = DevKit(
                  blockfrostApiBaseUri =
                      s"http://$host:${c.container.getMappedPort(blockfrostPort)}/api/v1",
                  yaciApiBaseUri = sttp.model.Uri.unsafeParse(
                    s"http://$host:${c.container.getMappedPort(adminPort)}/local-cluster/api"
                  ),
                )
                awaitBlockfrost(devKit, startupTimeout).as(devKit)
            }

    /** The container wait gates on the admin `initialized` status; the Yaci Store (which backend
      * queries hit) comes up a few seconds later, so poll it before yielding the handle.
      */
    private def awaitBlockfrost(devKit: DevKit, timeout: FiniteDuration): IO[Unit] =
        def loop(remaining: FiniteDuration): IO[Unit] =
            blockfrostReady(devKit).flatMap {
                case true => IO.unit
                case false if remaining <= Duration.Zero =>
                    IO.raiseError(
                      new RuntimeException(s"Yaci Blockfrost API not ready within $timeout")
                    )
                case false => IO.sleep(2.seconds) *> loop(remaining - 2.seconds)
            }
        loop(timeout)

    private def blockfrostReady(devKit: DevKit): IO[Boolean] =
        IO.blocking {
            val resp = Await.result(
              basicRequest
                  .get(sttp.model.Uri.unsafeParse(s"${devKit.blockfrostApiBaseUri}/blocks/latest"))
                  .send(backend),
              6.seconds
            )
            resp.code == StatusCode.Ok
        }.handleError(_ => false)
}
