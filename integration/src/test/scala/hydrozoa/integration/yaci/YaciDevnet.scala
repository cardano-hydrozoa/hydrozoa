package hydrozoa.integration.yaci

import cats.effect.{IO, Resource}
import scalus.testing.yaci.{YaciConfig, YaciContainer}
import sttp.model.Uri

/** A Testcontainers-managed Yaci DevKit devnet, acquired via scalus-testkit's
  * [[scalus.testing.yaci.YaciContainer]] (a thin wrapper over Bloxbean's `YaciCardanoContainer`).
  * Yields a [[DevKit]] handle bound to the container's mapped Blockfrost + admin URLs; the
  * container is released on close.
  *
  * `YaciContainer.acquire` internally runs `awaitStoreSync` (up to `config.startupTimeoutSeconds`)
  * before returning, so the handle is ready for use immediately.
  *
  * Requires Docker on the host, so the suites using it are excluded from the default test run (see
  * build.sbt).
  */
object YaciDevnet {

    /** Acquire a running devnet as a [[DevKit]] handle. */
    def resource(
        config: YaciConfig = YaciConfig(startupTimeoutSeconds = 600L),
    ): Resource[IO, DevKit] =
        Resource
            .make(IO.blocking(YaciContainer.acquire(config))) { _ =>
                IO.blocking(YaciContainer.release())
            }
            .map(mkDevKit)

    /** Acquire a shared JVM-wide devnet [[DevKit]] without a release hook. Intended for callers
      * whose driver can't compose a `Resource` across iterations — the ScalaCheck-driven MBT
      * suites, whose `initEnv: PropertyM[IO, Env]` returns a bare `Env` (no bracket). `scalus`'
      * [[YaciContainer]] is a JVM-wide singleton, so every call returns the same container; the
      * testcontainers Ryuk sidecar reaps it at JVM exit.
      */
    def acquireShared(
        config: YaciConfig = YaciConfig(startupTimeoutSeconds = 600L),
    ): IO[DevKit] =
        IO.blocking {
            // testcontainers finds its DockerClientProviderStrategy impls via a ServiceLoader on
            // the thread context classloader. This runs inside ScalaCheck property evaluation,
            // whose worker thread's context loader can't see those services, so discovery finds
            // none and throws "Could not find a valid Docker environment". (The probe suites avoid
            // this: a ScalaTest suite inits Docker from a normal thread first.) Pin the context
            // loader to this class's loader, which has testcontainers on its classpath.
            Thread.currentThread().setContextClassLoader(getClass.getClassLoader)
            YaciContainer.acquire(config)
        }.map(mkDevKit)

    private def mkDevKit(c: com.bloxbean.cardano.yaci.test.YaciCardanoContainer): DevKit =
        // Bloxbean's URL helpers hardcode `localhost` and include a trailing `/`; strip the slash
        // so `DevKit`'s string-interp URL builders don't produce `//`.
        DevKit(
          blockfrostApiBaseUri = c.getYaciStoreApiUrl.stripSuffix("/"),
          yaciApiBaseUri = Uri.unsafeParse(c.getLocalClusterApiUrl.stripSuffix("/")),
        )
}
