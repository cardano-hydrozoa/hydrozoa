package hydrozoa.lib.logging

import cats.Eval
import cats.effect.IO
import org.typelevel.log4cats.Logger // scalafix:ok DisableSyntax
import org.typelevel.log4cats.slf4j.Slf4jLogger // scalafix:ok DisableSyntax

enum Level:
    case Trace, Debug, Info, Warn, Error

/** A [[LogEventTyped]]'s deferred payload: the message, context, and cause. Held behind an `Eval`
  * so none of it — the message interpolation, the context values, `toString` on domain objects — is
  * computed unless the event's level is enabled (see [[Slf4jTracer.levelGated]]). The event's
  * [[LogEventTyped.level]] and [[LogEventTyped.routingKey]] stay eager because the gate needs them.
  */
final case class Rendered[A](msg: String, ctx: A, cause: Option[Throwable] = None)

case class LogEventTyped[A](
    level: Level,
    /** SLF4J logger name used to route to the correct Logback appender/level config. `None` means
      * "fall back to the default Hydrozoa logger." For typed events that always emit through the
      * same logger, `EventFormat.humanFormat` fills in `Some(...)` per event variant.
      */
    routingKey: Option[String],
    render: Eval[Rendered[A]]
)

type LogEvent = LogEventTyped[Map[String, String]]

object LogEvent {

    /** `msg` is by-name and captured into the deferred [[LogEventTyped.render]], so the
      * interpolation (and any `toString` inside it) runs only if the level is enabled — at every
      * call site, `From.*` and direct `LogEvent(...)` alike.
      */
    def apply(
        level: Level,
        msg: => String,
        ctx: Map[String, String] = Map.empty,
        cause: Option[Throwable] = None,
        routingKey: Option[String] = None
    ): LogEventTyped[Map[String, String]] =
        LogEventTyped(level, routingKey, Eval.later(Rendered(msg, ctx, cause)))

    /** Partially-applied factory: fixes [[ctx]] and [[routingKey]] for a set of related events.
      * Extra context pairs passed as varargs are merged with the base [[ctx]].
      */
    final class From(val ctx: Map[String, String], val routingKey: Option[String]):
        def trace(msg: => String, extra: (String, String)*): LogEvent =
            LogEvent(Level.Trace, msg, ctx ++ extra, routingKey = routingKey)
        def debug(msg: => String, extra: (String, String)*): LogEvent =
            LogEvent(Level.Debug, msg, ctx ++ extra, routingKey = routingKey)
        def info(msg: => String, extra: (String, String)*): LogEvent =
            LogEvent(Level.Info, msg, ctx ++ extra, routingKey = routingKey)
        def warn(msg: => String, extra: (String, String)*): LogEvent =
            LogEvent(Level.Warn, msg, ctx ++ extra, routingKey = routingKey)
        def error(msg: => String): LogEvent =
            LogEvent(Level.Error, msg, ctx, routingKey = routingKey)

    object From:
        def apply(ctx: Map[String, String], routingKey: String): From =
            new From(ctx, Some(routingKey))
        def forPeer(actorName: String, peerNum: Int): From =
            From(Map("peer" -> peerNum.toString), s"$actorName.$peerNum")
}

/** A contravariant logger: a function that emits a [[LogEvent]] into IO. Every actor and every
  * piece of infrastructure that wants to log holds one of these (or, more often, a typed
  * `ContraTracer[IO, MyEvent]` plus an `MyEventFormat.humanFormat` that lowers events into
  * [[LogEvent]]). The SLF4J back-end lives at [[Slf4jTracer.sink]].
  */
type Slf4jTracer = ContraTracer[IO, LogEvent]

object Slf4jTracer:

    /** SLF4J sink, **gated on the target logger's level**: an event whose level is disabled never
      * forces its [[LogEventTyped.render]] and never reaches SLF4J. Build per-component tracers
      * with `Slf4jTracer.sink.contramap(MyEventFormat.humanFormat(...))`. Compose capture sinks
      * with `|+|` — the gate lives only in this leg, so capture legs always fire.
      */
    val sink: ContraTracer[IO, LogEvent] = ContraTracer
        .emit((ev: LogEvent) =>
            val r = ev.render.value // forced only past the gate
            val lg = loggerIO(ev.routingKey.getOrElse("hydrozoa"))
            val msg = renderMsg(r)
            ev.level match
                case Level.Trace => lg.trace(msg)
                case Level.Debug => lg.debug(msg)
                case Level.Info  => lg.info(msg)
                case Level.Warn  => lg.warn(msg)
                case Level.Error => r.cause.fold(lg.error(msg))(lg.error(_)(msg))
        )
        .levelGated

    extension (t: ContraTracer[IO, LogEvent])
        /** Squelch `t` — **without forcing the event's `render`** — when the routing key's logger
          * has the event's level disabled. Squelching drops everything downstream (arrow semantics:
          * a squelching branch runs no effect), so no message, context, or cause is evaluated. Only
          * the eager `level`/`routingKey` and the `isEnabled` check run.
          */
        def levelGated: ContraTracer[IO, LogEvent] =
            t.squelchUnlessM(ev => isEnabled(ev.routingKey.getOrElse("hydrozoa"), ev.level))

    /** Whether `name`'s logger has `level` enabled. Re-queried per trace (LoggerFactory caches the
      * logger) so a runtime Logback level change is honored.
      */
    private def isEnabled(name: String, level: Level): IO[Boolean] = IO {
        val lg = org.slf4j.LoggerFactory.getLogger(name) // scalafix:ok DisableSyntax
        level match
            case Level.Trace => lg.isTraceEnabled
            case Level.Debug => lg.isDebugEnabled
            case Level.Info  => lg.isInfoEnabled
            case Level.Warn  => lg.isWarnEnabled
            case Level.Error => lg.isErrorEnabled
    }

    /** The one SLF4J bridge in the codebase. Everything else logs through a `ContraTracer` and
      * reaches SLF4J via [[sink]]; the `DisableSyntax` rule in `.scalafix.conf` keeps it that way.
      */
    private def loggerIO(name: String): Logger[IO] = Slf4jLogger.getLoggerFromName[IO](name)

    private def renderMsg(r: Rendered[Map[String, String]]): String =
        val prefix =
            if r.ctx.isEmpty then ""
            else "[" + r.ctx.map((k, v) => s"$k=$v").mkString(" ") + "] "
        prefix + r.msg
