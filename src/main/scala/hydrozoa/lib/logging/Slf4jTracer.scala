package hydrozoa.lib.logging

import cats.effect.IO

enum Level:
    case Trace, Debug, Info, Warn, Error

case class LogEventTyped[A](
    level: Level,
    msg: String,
    ctx: A,
    cause: Option[Throwable] = None,
    /** SLF4J logger name used to route to the correct Logback appender/level config. `None` means
      * "fall back to the default Hydrozoa logger." For typed events that always emit through the
      * same logger, `EventFormat.humanFormat` fills in `Some(...)` per event variant.
      */
    routingKey: Option[String] = None
)

type LogEvent = LogEventTyped[Map[String, String]]

object LogEvent {
    def apply(
        level: Level,
        msg: String,
        ctx: Map[String, String] = Map.empty,
        cause: Option[Throwable] = None,
        routingKey: Option[String] = None
    ): LogEventTyped[Map[String, String]] = LogEventTyped(level, msg, ctx, cause, routingKey)

    /** Partially-applied factory: fixes [[ctx]] and [[routingKey]] for a set of related events.
      * Extra context pairs passed as varargs are merged with the base [[ctx]].
      */
    final class From(val ctx: Map[String, String], val routingKey: Option[String]):
        def trace(msg: String, extra: (String, String)*): LogEvent =
            LogEvent(Level.Trace, msg, ctx ++ extra, routingKey = routingKey)
        def debug(msg: String, extra: (String, String)*): LogEvent =
            LogEvent(Level.Debug, msg, ctx ++ extra, routingKey = routingKey)
        def info(msg: String, extra: (String, String)*): LogEvent =
            LogEvent(Level.Info, msg, ctx ++ extra, routingKey = routingKey)
        def warn(msg: String, extra: (String, String)*): LogEvent =
            LogEvent(Level.Warn, msg, ctx ++ extra, routingKey = routingKey)
        def error(msg: String): LogEvent =
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

    /** SLF4J sink — routes [[LogEvent]] to the correct logger via [[LogEvent.routingKey]]. Build
      * per-component tracers with `Slf4jTracer.sink.contramap(MyEventFormat.humanFormat(...))`.
      */
    val sink: ContraTracer[IO, LogEvent] = ContraTracer.emit((ev: LogEvent) =>
        val lg = Logging.loggerIO(ev.routingKey.getOrElse("hydrozoa"))
        val msg = renderMsg(ev)
        ev.level match
            case Level.Trace => lg.trace(msg)
            case Level.Debug => lg.debug(msg)
            case Level.Info  => lg.info(msg)
            case Level.Warn  => lg.warn(msg)
            case Level.Error => ev.cause.fold(lg.error(msg))(lg.error(_)(msg))
    )

    private def renderMsg(ev: LogEvent): String =
        val prefix =
            if ev.ctx.isEmpty then ""
            else "[" + ev.ctx.map((k, v) => s"$k=$v").mkString(" ") + "] "
        prefix + ev.msg
