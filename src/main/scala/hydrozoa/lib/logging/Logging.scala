package hydrozoa.lib.logging

import cats.effect.IO
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Internal SLF4J / log4cats adapter used by [[Slf4jTracer.sink]] to produce the underlying SLF4J
  * `Logger` instances. **Not for direct use.** Production code holds a `ContraTracer[F, X]` (either
  * a typed `XYZEvent` ADT or the generic [[Slf4jMsg]]) and reaches SLF4J through
  * [[Slf4jTracer.sink]] — see `docs/spec/logging-tracing.md`.
  *
  * The SLF4J logger name controls the Logback logger hierarchy (e.g.
  * `"hydrozoa.multisig.CardanoLiaison"` is filtered by `<logger name="hydrozoa" .../>` in
  * `logback.xml`).
  */
object Logging {

    /** log4cats SLF4J adapter used by [[Slf4jTracer.sink]]. */
    def loggerIO(name: String): Logger[IO] =
        Slf4jLogger.getLoggerFromName[IO](name)

    /** A synchronous logger, for the few places that have no effect context to log in.
      *
      * A supervision decider is one: it is a pure `Throwable => Directive` the actor library calls
      * on the failure path, so there is no `IO` to sequence a log into. Everything else should use
      * [[loggerIO]] or a tracer.
      */
    def loggerSync(name: String): org.slf4j.Logger =
        org.slf4j.LoggerFactory.getLogger(name)
}
