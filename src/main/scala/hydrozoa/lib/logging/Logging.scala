package hydrozoa.lib.logging

import cats.effect.IO
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Internal SLF4J / log4cats adapter used by [[Slf4jTracer.sink]] to produce the underlying SLF4J
  * `Logger` instances. **Not for direct use.** Production code holds a `ContraTracer[F, X]` (either
  * a typed `XYZEvent` ADT or the generic [[Slf4jMsg]]) and reaches SLF4J through
  * [[Slf4jTracer.sink]] — see `docs/logging-tracing.md`.
  *
  * The SLF4J logger name controls the Logback logger hierarchy (e.g.
  * `"hydrozoa.multisig.CardanoLiaison"` is filtered by `<logger name="hydrozoa" .../>` in
  * `logback.xml`).
  */
object Logging {

    /** log4cats SLF4J adapter used by [[Slf4jTracer.sink]]. */
    def loggerIO(name: String): Logger[IO] =
        Slf4jLogger.getLoggerFromName[IO](name)
}
