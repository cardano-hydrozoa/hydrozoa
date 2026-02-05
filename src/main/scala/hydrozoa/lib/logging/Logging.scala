package hydrozoa.lib.logging

import cats.effect.IO
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Structured logging for Hydrozoa backed by log4cats / SLF4J / Logback.
  *
  * One [[Logging]] instance per actor system. Each actor calls `logging.logger(name)` to obtain its
  * [[Logger[IO]]]. The name controls the Logback logger hierarchy (e.g.
  * `"hydrozoa.multisig.CardanoLiaison"` is filtered by `<logger name="hydrozoa" .../>` in
  * logback.xml).
  *
  * Usage:
  * {{{
  *   val logging <- Logging.create
  *   val logger  = logging.logger("ConsensusActor")
  *   logger.info("actor started")   // IO[Unit]
  * }}}
  */
final class Logging private[logging] () {
    def logger(name: String): Logger[IO] =
        Slf4jLogger.getLoggerFromName[IO](name)
}

object Logging {
    def create: IO[Logging] = IO.pure(new Logging())
}
