package hydrozoa.integration.yaci

import cats.effect.{IO, Resource}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scalus.testing.yaci.{YaciConfig, YaciContainer}
import sttp.client4.*
import sttp.model.{StatusCode, Uri}

/** A Testcontainers-managed Yaci DevKit devnet, acquired via scalus-testkit's
  * [[scalus.testing.yaci.YaciContainer]] (a thin wrapper over Bloxbean's `YaciCardanoContainer`).
  * Yields a [[DevKit]] handle bound to the container's mapped Blockfrost + admin URLs; the
  * container is released on close.
  *
  * Requires Docker on the host, so the suites using it are excluded from the default test run (see
  * build.sbt).
  */
object YaciDevnet {

    /** Acquire a running devnet as a [[DevKit]] handle. */
    def resource(
        config: YaciConfig = YaciConfig(),
        startupTimeout: FiniteDuration = 3.minutes,
    ): Resource[IO, DevKit] =
        Resource
            .make(IO.blocking(YaciContainer.acquire(config))) { _ =>
                IO.blocking(YaciContainer.release())
            }
            .evalMap { c =>
                // Bloxbean's URL helpers hardcode `localhost` and include a trailing `/`; strip
                // the slash so `DevKit`'s string-interp URL builders don't produce `//`.
                val devKit = DevKit(
                  blockfrostApiBaseUri = c.getYaciStoreApiUrl.stripSuffix("/"),
                  yaciApiBaseUri = Uri.unsafeParse(c.getLocalClusterApiUrl.stripSuffix("/")),
                )
                awaitBlockfrost(devKit, startupTimeout).as(devKit)
            }

    /** Bloxbean's `start()` gates on the admin API; the Yaci Store (Blockfrost) comes up a few
      * seconds later, so poll it before yielding the handle.
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
                  .get(Uri.unsafeParse(s"${devKit.blockfrostApiBaseUri}/blocks/latest"))
                  .send(backend),
              6.seconds
            )
            resp.code == StatusCode.Ok
        }.handleError(_ => false)
}
