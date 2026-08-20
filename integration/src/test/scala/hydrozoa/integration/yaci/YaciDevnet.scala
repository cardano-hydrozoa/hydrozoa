package hydrozoa.integration.yaci

import cats.effect.{IO, Resource}
import java.lang.management.ManagementFactory
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
        // DIAGNOSTIC (revert after the RBR MBT Yaci Docker triage): the yaci probes reach Docker
        // but this shared-container path reports "Could not find a valid Docker environment". Docker
        // resolution depends on the integration project's forked-JVM flags (esp. `-Dapi.version` —
        // see build.sbt); the CI Yaci step showed this failure with no testcontainers log output at
        // all, so print to stderr — bypassing logback — to establish (1) that we reached this call,
        // (2) whether the forked JVM actually carries the flags, and (3) the real underlying cause.
        IO.blocking {
            System.err.println(
              "[yaci-diag] acquireShared: forked-JVM input args = " +
                  ManagementFactory.getRuntimeMXBean.getInputArguments
            )
            System.err.println(
              "[yaci-diag] acquireShared: api.version=" + sys.props.get("api.version") +
                  " DOCKER_HOST=" + sys.env.get("DOCKER_HOST") +
                  " logback.configurationFile=" + sys.props.get("logback.configurationFile")
            )
            try YaciContainer.acquire(config)
            catch {
                case t: Throwable =>
                    System.err.println(
                      "[yaci-diag] acquireShared: YaciContainer.acquire threw " +
                          t.getClass.getName + ": " + t.getMessage
                    )
                    t.printStackTrace()
                    throw t
            }
        }.map(mkDevKit)

    private def mkDevKit(c: com.bloxbean.cardano.yaci.test.YaciCardanoContainer): DevKit =
        // Bloxbean's URL helpers hardcode `localhost` and include a trailing `/`; strip the slash
        // so `DevKit`'s string-interp URL builders don't produce `//`.
        DevKit(
          blockfrostApiBaseUri = c.getYaciStoreApiUrl.stripSuffix("/"),
          yaciApiBaseUri = Uri.unsafeParse(c.getLocalClusterApiUrl.stripSuffix("/")),
        )
}
