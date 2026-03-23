package hydrozoa.lib.logging

import cats.effect.IO
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Structured logging for Hydrozoa backed by log4cats / SLF4J / Logback.
  *
  * The logger name controls the Logback logger hierarchy (e.g. `"hydrozoa.multisig.CardanoLiaison"`
  * is filtered by `<logger name="hydrozoa" .../>` in logback.xml).
  *
  * Usage for pure code (non-IO):
  * {{{
  *   val logger = Logging.logger(getClass)
  *   logger.info("message")
  * }}}
  *
  * Usage for IO-based code (actors, effects):
  * {{{
  *   val logger = Logging.loggerIO("ConsensusActor")
  *   logger.info("actor started")   // IO[Unit]
  * }}}
  */
object Logging {

    /** Create a plain SLF4J logger for non-IO code. */
    def logger(clazz: Class[?]): org.slf4j.Logger =
        org.slf4j.LoggerFactory.getLogger(clazz)

    /** Create a plain SLF4J logger for non-IO code. */
    def logger(name: String): org.slf4j.Logger =
        org.slf4j.LoggerFactory.getLogger(name)

    /** Create a log4cats IO-based logger for actors and other IO code. */
    def loggerIO(name: String): Logger[IO] =
        Slf4jLogger.getLoggerFromName[IO](name)
}
