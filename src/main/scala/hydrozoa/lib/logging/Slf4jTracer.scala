package hydrozoa.lib.logging

import cats.Eval
import cats.effect.IO
import org.typelevel.log4cats.Logger // scalafix:ok DisableSyntax
import org.typelevel.log4cats.slf4j.Slf4jLogger // scalafix:ok DisableSyntax

enum Level:
    case Trace, Debug, Info, Warn, Error

/** A [[LogEventTyped]]'s deferred payload. See [[LogEvent.deferred]]
  */
final case class Rendered[A] private[logging] (msg: String, ctx: A, cause: Option[Throwable] = None)

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

    /** Wrap a by-name message (plus context and cause) in the deferred [[Rendered]] payload.
      */
    private[logging] def deferred[A](
        msg: => String,
        ctx: A,
        cause: Option[Throwable] = None
    ): Eval[Rendered[A]] =
        Eval.later(Rendered(msg, ctx, cause))

    def apply(
        level: Level,
        msg: => String,
        ctx: Map[String, String] = Map.empty,
        cause: Option[Throwable] = None,
        routingKey: Option[String] = None
    ): LogEventTyped[Map[String, String]] =
        LogEventTyped(level, routingKey, deferred(msg, ctx, cause))

    /** Partially-applied factory: fixes [[ctx]] and [[routingKey]] for a set of related events.
      * Extra context pairs passed as varargs are merged with the base [[ctx]].
      */
    final class From(val ctx: Map[String, String], val routingKey: Option[String]):
        // `msg` is by-name and lives in the deferred `Rendered`, so the message — the expensive
        // vector, where any domain `toString` sits — is not built unless the level is enabled. The
        // `extra` context is evaluated eagerly, but it only ever carries cheap primitives
        // (block/stack numbers, request ids); deferring it too would buy nothing.
        private def helper(
            loggingLevel: Level,
            msg: => String,
            extra: (String, String)*
        ): LogEvent =
            LogEventTyped(loggingLevel, routingKey, deferred(msg, ctx ++ extra))

        def trace(msg: => String, extra: (String, String)*): LogEvent =
            helper(Level.Trace, msg, extra*)
        def debug(msg: => String, extra: (String, String)*): LogEvent =
            helper(Level.Debug, msg, extra*)
        def info(msg: => String, extra: (String, String)*): LogEvent =
            helper(Level.Info, msg, extra*)
        def warn(msg: => String, extra: (String, String)*): LogEvent =
            helper(Level.Warn, msg, extra*)
        def error(msg: => String): LogEvent =
            helper(Level.Error, msg)

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
