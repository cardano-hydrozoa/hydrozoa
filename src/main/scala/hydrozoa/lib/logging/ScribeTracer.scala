package hydrozoa.lib.logging

import cats.effect.IO
import scribe.{Level as SLevel, Logger as SLogger}

/** PoC scribe backend behind the `ContraTracer` (spike — see `design/logging-scribe.md`). A drop-in
  * alternative to [[Slf4jTracer.sink]]: same `ContraTracer[IO, LogEvent]` surface, the same
  * level-gate (force `render` only past an `isEnabled` check), but the check reimplements against
  * scribe with **no SLF4J**. Not wired into production; exercised by the load-test harness.
  */
object ScribeTracer:

    /** Level-gated scribe sink. `render.value` is forced only when the routing key's scribe logger
      * has the event's level enabled.
      */
    val sink: ContraTracer[IO, LogEvent] = ContraTracer
        .emit((ev: LogEvent) =>
            IO {
                val r = ev.render.value
                val lg = SLogger(ev.routingKey.getOrElse("hydrozoa"))
                val msg = renderMsg(r)
                ev.level match
                    case Level.Trace => lg.trace(msg)
                    case Level.Debug => lg.debug(msg)
                    case Level.Info  => lg.info(msg)
                    case Level.Warn  => lg.warn(msg)
                    case Level.Error => r.cause.fold(lg.error(msg))(t => lg.error(msg, t))
                ()
            }
        )
        .squelchUnlessM(ev => isEnabled(ev.routingKey.getOrElse("hydrozoa"), ev.level))

    /** Runtime, per-trace level check — reflects config changes, no SLF4J. */
    private def isEnabled(name: String, level: Level): IO[Boolean] = IO {
        SLogger.get(name).getOrElse(SLogger.root).includes(toScribe(level))
    }

    private def toScribe(l: Level): SLevel = l match
        case Level.Trace => SLevel.Trace
        case Level.Debug => SLevel.Debug
        case Level.Info  => SLevel.Info
        case Level.Warn  => SLevel.Warn
        case Level.Error => SLevel.Error

    private def renderMsg(r: Rendered[Map[String, String]]): String =
        val prefix =
            if r.ctx.isEmpty then ""
            else "[" + r.ctx.map((k, v) => s"$k=$v").mkString(" ") + "] "
        prefix + r.msg
