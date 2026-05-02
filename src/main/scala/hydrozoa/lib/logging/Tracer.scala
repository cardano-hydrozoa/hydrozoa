package hydrozoa.lib.logging

import cats.effect.{IO, IOLocal}

enum Level:
    case Debug, Info, Warn, Error

case class LogEvent(
    level: Level,
    msg: String,
    ctx: Map[String, String] = Map.empty,
    cause: Option[Throwable] = None,
    logger: String = "gummiworm"
)

/** Contravariant logger: a function that emits a [[LogEvent]] into IO.
  *
  * The [[LogEvent.logger]] field routes to the correct SLF4J logger, preserving Logback hierarchy
  * and per-component level config. [[LogEvent.ctx]] carries ambient key-value context.
  *
  * Both are set via [[scoped]] / [[scopedCtx]]; the ambient instance lives in [[IOLocal]].
  *
  * Note: [[hydrozoa.lib.tracing.ProtocolTracer]] is a future merge candidate.
  */
type Tracer = LogEvent => IO[Unit]

extension (t: Tracer)
    def withCtx(f: Map[String, String] => Map[String, String]): Tracer =
        ev => t(ev.copy(ctx = f(ev.ctx)))

object Tracer:

    private val base: Tracer = ev =>
        val lg = Logging.loggerIO(ev.logger)
        val prefix =
            if ev.ctx.isEmpty then ""
            else "[" + ev.ctx.map((k, v) => s"$k=$v").mkString(" ") + "] "
        val msg = prefix + ev.msg
        ev.level match
            case Level.Debug => lg.debug(msg)
            case Level.Info  => lg.info(msg)
            case Level.Warn  => lg.warn(msg)
            case Level.Error => ev.cause.fold(lg.error(msg))(lg.error(_)(msg))

    /** Seeds the local with [[base]] pre-configured to route to [[name]] unless overridden. */
    def makeLocal(name: String): IO[IOLocal[Tracer]] =
        IOLocal(ev => base(ev.copy(logger = if ev.logger == "gummiworm" then name else ev.logger)))

    /** Enriches the ambient tracer for the duration of [[fa]], then restores. Use to set
      * [[LogEvent.logger]] (component routing) or [[LogEvent.ctx]] (ambient context).
      */
    def scoped[A](f: LogEvent => LogEvent)(fa: IO[A])(using local: IOLocal[Tracer]): IO[A] =
        local.get.flatMap(t =>
            local.getAndSet(ev => t(f(ev))).flatMap(old => fa.guarantee(local.set(old)))
        )

    /** Convenience: add key-value pairs to [[LogEvent.ctx]] only. */
    def scopedCtx[A](kvs: (String, String)*)(fa: IO[A])(using IOLocal[Tracer]): IO[A] =
        scoped(ev => ev.copy(ctx = ev.ctx ++ kvs))(fa)

    def debug(msg: String)(using local: IOLocal[Tracer]): IO[Unit] =
        local.get.flatMap(_(LogEvent(Level.Debug, msg)))

    def info(msg: String)(using local: IOLocal[Tracer]): IO[Unit] =
        local.get.flatMap(_(LogEvent(Level.Info, msg)))

    def warn(msg: String)(using local: IOLocal[Tracer]): IO[Unit] =
        local.get.flatMap(_(LogEvent(Level.Warn, msg)))

    def error(msg: String, cause: Option[Throwable] = None)(using
        local: IOLocal[Tracer]
    ): IO[Unit] =
        local.get.flatMap(_(LogEvent(Level.Error, msg, cause = cause)))
