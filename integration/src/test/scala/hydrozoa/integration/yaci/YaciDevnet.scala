package hydrozoa.integration.yaci

import cats.effect.{IO, Resource}
import com.dimafeng.testcontainers.GenericContainer
import org.testcontainers.containers.wait.strategy.Wait
import scala.concurrent.Await
import scala.concurrent.duration.*
import sttp.client4.*
import sttp.model.{StatusCode, Uri}

/** A Testcontainers-managed Yaci DevKit devnet.
  *
  * Starts `bloxbean/yaci-cli`, creates and starts a fresh single-node devnet, waits until it
  * reports `initialized`, and yields a [[DevKit]] handle bound to the container's mapped Blockfrost
  * (8080) and admin (10000) ports.
  *
  * NB: authored against the Yaci DevKit docs and unvalidated against a live Docker daemon here; the
  * container lifecycle (idle `sleep infinity` image + `create-node -o --start` exec + status poll)
  * may need tuning on first real run. Requires Docker on the host, so the suites using it are
  * excluded from the default test run (see build.sbt).
  */
object YaciDevnet {

    /** Yaci DevKit publishes no `latest` tag; pin the last non-beta release. */
    val defaultImageTag: String = "0.11.0"

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
                  // The image leaves the CLI idle (`sleep infinity`) with nothing bound on
                  // 8080/10000 until a devnet is created, so a port-listening wait would hang;
                  // emit a marker and gate on it instead. The devnet is created in `startDevnet`.
                  command = Seq("sh", "-c", "echo yaci-devkit-up && sleep infinity"),
                  waitStrategy = Wait.forLogMessage(".*yaci-devkit-up.*", 1),
                )
                c.start()
                c
            })(c => IO.blocking(c.stop()))
            .evalMap { c =>
                val host = c.container.getHost
                val devKit = DevKit(
                  blockfrostApiBaseUri =
                      s"http://$host:${c.container.getMappedPort(blockfrostPort)}/api/v1",
                  yaciApiBaseUri = Uri.unsafeParse(
                    s"http://$host:${c.container.getMappedPort(adminPort)}/local-cluster/api"
                  ),
                )
                startDevnet(c) *> awaitInitialized(devKit, startupTimeout).as(devKit)
            }

    /** Create and start a fresh devnet inside the otherwise-idle CLI container. */
    private def startDevnet(c: GenericContainer): IO[Unit] =
        IO.blocking {
            val res =
                c.container.execInContainer("/app/yaci-cli.sh", "create-node", "-o", "--start")
            if res.getExitCode != 0 then
                throw new RuntimeException(
                  s"yaci-cli create-node failed (exit ${res.getExitCode}): ${res.getStderr}"
                )
        }

    /** Poll the admin `devnet/status` endpoint until it reports `initialized`. */
    private def awaitInitialized(devKit: DevKit, timeout: FiniteDuration): IO[Unit] =
        def loop(remaining: FiniteDuration): IO[Unit] =
            statusInitialized(devKit).flatMap {
                case true => IO.unit
                case false if remaining <= Duration.Zero =>
                    IO.raiseError(
                      new RuntimeException(s"Yaci devnet not initialized within $timeout")
                    )
                case false => IO.sleep(2.seconds) *> loop(remaining - 2.seconds)
            }
        loop(timeout)

    private def statusInitialized(devKit: DevKit): IO[Boolean] =
        IO.blocking {
            val resp = Await.result(
              basicRequest.get(uri"${devKit.yaciApiBaseUri}/admin/devnet/status").send(backend),
              10.seconds
            )
            resp.code == StatusCode.Ok && resp.body.fold(_ => false, _.contains("initialized"))
        }
}
