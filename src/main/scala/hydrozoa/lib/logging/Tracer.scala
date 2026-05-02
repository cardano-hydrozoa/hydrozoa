package hydrozoa.lib.logging

import cats.effect.{IO, IOLocal}
import cats.syntax.all.*

enum Level:
    case Trace, Debug, Info, Warn, Error

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

// Lightweight Writer monad for logging in pure code.
// Pure functions return Traced[A] instead of A; IO callers emit events via logWith;
// pure callers (e.g. test models) use .value to ignore them.
type Traced[+A] = (A, List[LogEvent])

extension (t: Tracer)
    def withCtx(f: Map[String, String] => Map[String, String]): Tracer =
        ev => t(ev.copy(ctx = f(ev.ctx)))

extension [A](traced: Traced[A])
    def value: A = traced._1
    def logWith(using local: IOLocal[Tracer]): IO[A] =
        traced._2.traverse_(e => local.get.flatMap(_(e))).as(traced._1)

object Tracer:

    private val base: Tracer = ev =>
        val lg = Logging.loggerIO(ev.logger)
        val prefix =
            if ev.ctx.isEmpty then ""
            else "[" + ev.ctx.map((k, v) => s"$k=$v").mkString(" ") + "] "
        val msg = prefix + ev.msg
        ev.level match
            case Level.Trace => lg.trace(msg)
            case Level.Debug => lg.debug(msg)
            case Level.Info  => lg.info(msg)
            case Level.Warn  => lg.warn(msg)
            case Level.Error => ev.cause.fold(lg.error(msg))(lg.error(_)(msg))

    /** Seeds the local with [[base]] pre-configured to route to [[name]] unless overridden. */
    def makeLocal(name: String): IO[IOLocal[Tracer]] =
        IOLocal(ev => base(ev.copy(logger = if ev.logger == "gummiworm" then name else ev.logger)))

    /** Permanently contramaps the ambient tracer — no scope, no restore.
      *
      * Use in actor `preStart` to fix [[LogEvent.logger]] for the actor's lifetime. For transient
      * per-message enrichment use [[scoped]] / [[scopedCtx]] instead.
      */
    def updateLocal(f: LogEvent => LogEvent)(using local: IOLocal[Tracer]): IO[Unit] =
        local.update(tracer => ev => tracer(f(ev)))

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

    def trace(msg: String)(using local: IOLocal[Tracer]): IO[Unit] =
        local.get.flatMap(_(LogEvent(Level.Trace, msg)))

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
