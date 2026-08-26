package hydrozoa.lib.logging

import cats.Eval
import cats.effect.IO
import java.util.concurrent.ConcurrentHashMap
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
      * forces its [[LogEventTyped.render]] and never reaches SLF4J. The render is handed to
      * log4cats **by-name**, and its `contextLog` (`F.delay(if (isEnabled) log())`) forces it only
      * when the routing key's logger has the level enabled — so a single check gates it, with no
      * separate `ContraTracer` gate to run its own `isEnabled` and re-look-up the logger. Build
      * per-component tracers with `Slf4jTracer.sink.contramap(MyEventFormat.humanFormat(...))`;
      * compose capture sinks with `|+|` — they are separate legs, so a disabled slf4j leg never
      * squelches them.
      */
    val sink: ContraTracer[IO, LogEvent] = ContraTracer.emit { (ev: LogEvent) =>
        val lg = loggerIO(ev.routingKey.getOrElse("hydrozoa"))
        def msg = renderMsg(ev.render.value) // by-name: forced only when the level is enabled
        ev.level match
            case Level.Trace => lg.trace(msg)
            case Level.Debug => lg.debug(msg)
            case Level.Info  => lg.info(msg)
            case Level.Warn  => lg.warn(msg)
            // Error carries a cause, which lives in the (now forced) render; error is effectively
            // always enabled, so forcing it here rather than by-name costs nothing in practice.
            case Level.Error =>
                val r = ev.render.value
                r.cause.fold(lg.error(renderMsg(r)))(lg.error(_)(renderMsg(r)))
    }

    /** log4cats loggers cached per routing key: `getLoggerFromName` allocates a fresh wrapper on
      * every call, and routing keys are a small fixed set, so memoise to keep the sink from
      * allocating one per emit. This is the one SLF4J bridge in the codebase; the `DisableSyntax`
      * rule in `.scalafix.conf` keeps it that way.
      */
    private lazy val loggers = new ConcurrentHashMap[String, Logger[IO]]()
    private def loggerIO(name: String): Logger[IO] =
        loggers.computeIfAbsent(name, n => Slf4jLogger.getLoggerFromName[IO](n))

    private def renderMsg(r: Rendered[Map[String, String]]): String =
        if r.ctx.isEmpty then r.msg
        else "[" + r.ctx.map((k, v) => s"$k=$v").mkString(" ") + "] " + r.msg
