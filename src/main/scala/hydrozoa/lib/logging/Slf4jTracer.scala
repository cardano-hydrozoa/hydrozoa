package hydrozoa.lib.logging

import cats.effect.{ExitCode, IO, IOApp, IOLocal}
import cats.implicits.*
import cats.{effect, *}
import hydrozoa.lib.logging.Level.Info

import ContraTracerSyntax.*

enum Level:
    case Trace, Debug, Info, Warn, Error

// This is a log event at least has a typed context. Should we also have typed messages, or swap the (msg:String) for a
// polymorphic typed payload?
// TODO: Make this a trait or an abstact case class with an abstract `def toLogEvent` method
case class LogEventTyped[A](
    level: Level,
    msg: String,
    ctx: A,
    cause: Option[Throwable] = None,
    /** SLF4J logger name used for routing to the correct Logback appender/level config. [[None]]
      * means "use whatever routing the ambient [[IOLocal]]\[\[\[Slf4jTracer\]\]\] has set" — the
      * tracer installed by [[routeLocal]] fills it in. Set [[Some]] only in pure functions
      * (returning [[Traced]][A]) that need explicit sub-component routing independent of the
      * ambient actor context.
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
}

/** Contravariant logger: a function that emits a [[LogEvent]] into IO.
  *
  * [[LogEvent.routingKey]] routes to the correct SLF4J logger, preserving Logback hierarchy and
  * per-component level config. [[LogEvent.ctx]] carries ambient key-value context.
  *
  * Both are set via [[scoped]] / [[scopedCtx]]; the ambient instance lives in [[IOLocal]].
  */
type Slf4jTracer = ContraTracer[IO, LogEvent]

extension (t: Slf4jTracer)
    def withCtx(f: Map[String, String] => Map[String, String]): Slf4jTracer = {
        t.contramap(ev => ev.copy(ctx = f(ev.ctx)))
    }

object Slf4jTracer:

    /** SLF4J sink — routes [[LogEvent]] to the correct logger via [[LogEvent.routingKey]]. */
    val sink: ContraTracer[IO, LogEvent] = ContraTracer.emit((ev: LogEvent) =>
        val lg = Logging.loggerIO(ev.routingKey.getOrElse("hydrozoa"))
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
    )

    /** Creates a fresh [[IOLocal]] seeded with the base tracer. All routing is then set per-fiber
      * via [[routeLocal]] in each actor's preStartLocal.
      */
    def makeLocal: IO[IOLocal[Slf4jTracer]] = IOLocal(sink)

    /** Permanently sets the routing key for events with no explicit [[LogEvent.routingKey]] in this
      * fiber. Events with an explicit [[Some]] routing key pass through unchanged.
      *
      * Call from actor's `preStartLocal` (not `preStart` — `preStart` runs in the parent fiber and
      * would corrupt the parent's routing).
      */
    def routeLocal(name: String)(using local: IOLocal[Slf4jTracer]): IO[Unit] =
        updateLocal(ev => if ev.routingKey.isEmpty then ev.copy(routingKey = Some(name)) else ev)

    /** Permanently contramaps the ambient tracer — no scope, no restore.
      *
      * Prefer [[scoped]]/[[scopedCtx]] or [[routeLocal]] for setting the routing key. Use this only
      * for other permanent per-fiber enrichment.
      */
    def updateLocal(f: LogEvent => LogEvent)(using local: IOLocal[Slf4jTracer]): IO[Unit] =
        local.update(tracer => tracer.contramap(f))

    /** Convenience: permanently add key-value pairs to [[LogEvent.ctx]] only. */
    def updateLocalCtx(kvs: (String, String)*)(using local: IOLocal[Slf4jTracer]): IO[Unit] =
        updateLocal(ev => ev.copy(ctx = ev.ctx ++ kvs))

    /** Enriches the ambient tracer for the duration of [[fa]], then restores. Use to set
      * [[LogEvent.ctx]] (ambient context).
      */
    def scoped[A](f: LogEvent => LogEvent)(fa: IO[A])(using local: IOLocal[Slf4jTracer]): IO[A] =
        for {
            tracer <- local.get
            old <- local.getAndSet(tracer.contramap(f))
            res <- fa.guarantee(local.set(old))
        } yield res

    def contramapScoped[A, B](
        f: B => LogEvent
    )(fa: ContraTracer[IO, B] ?=> IO[A])(using local: IOLocal[Slf4jTracer]): IO[A] =
        for {
            tracer <- local.get
            res <- {
                given ContraTracer[IO, B] = tracer.contramap(f)
                fa.guarantee(local.set(tracer))
            }
        } yield res

    /** Convenience: add key-value pairs to [[LogEvent.ctx]] only. */
    def scopedCtx[A](kvs: (String, String)*)(fa: IO[A])(using IOLocal[Slf4jTracer]): IO[A] =
        scoped(ev => ev.copy(ctx = ev.ctx ++ kvs))(fa)

    private def levelMap(level: Level)(msg: => String)(using
        local: IOLocal[Slf4jTracer]
    ): IO[Unit] =
        local.get.flatMap(_.traceWith(LogEvent(level, msg)))

    def trace(msg: => String)(using local: IOLocal[Slf4jTracer]): IO[Unit] =
        levelMap(Level.Trace)(msg)

    def debug(msg: => String)(using local: IOLocal[Slf4jTracer]): IO[Unit] =
        levelMap(Level.Debug)(msg)

    def info(msg: => String)(using local: IOLocal[Slf4jTracer]): IO[Unit] =
        levelMap(Level.Info)(msg)

    def warn(msg: => String)(using local: IOLocal[Slf4jTracer]): IO[Unit] =
        levelMap(Level.Warn)(msg)

    def error(msg: => String, cause: Option[Throwable] = None)(using
        local: IOLocal[Slf4jTracer]
    ): IO[Unit] = levelMap(Level.Error)(msg)

// Lightweight Writer monad for logging in pure code.
// Pure functions return Traced[A] instead of A; IO callers emit events via logWith;
// pure callers (e.g. test models) use .value to ignore them.
type Traced[+A] = (A, List[LogEvent])

extension [A](traced: Traced[A])
    def value: A = traced._1
    def logWith(using local: IOLocal[Slf4jTracer]): IO[A] =
        traced._2.traverse_(e => local.get.flatMap(_.traceWith(e))).as(traced._1)

object TracerDemo extends IOApp {

    enum CtxKeys:
        case K1
        case K2
        case K3

    case class MockBlock(blockNumber: Int, kzg: String)

    case class BlockProductionCtx(ctx: Map[CtxKeys, String])

    def blockProductionContramap(
        blockProductionCtx: BlockProductionCtx
    )(mockBlock: MockBlock): LogEvent =
        LogEvent(
          level = Info,
          msg = "block produced",
          ctx = blockProductionCtx.ctx.map((k, v) => (k.toString, v)),
          cause = None,
          routingKey = None
        )

    override def run(args: List[String]): IO[ExitCode] =
        for {
            tracerLocal <- Slf4jTracer.makeLocal
            _ <- {
                given IOLocal[Slf4jTracer] = tracerLocal

                for {

                    _ <- Slf4jTracer.routeLocal("TracerDemo")

                    // This is wrong -- it shouldn't print, but it does. LoggerDemo doesn't have the same issue
                    _ <- Slf4jTracer.trace {
                        println("should not print when level is set to info")
                        "should not print when level is set to Info"
                    }

                    // String tracing
                    _ <- for {
                        _ <- Slf4jTracer.trace("trace level")
                        _ <- Slf4jTracer.info("info level")
                        _ <- Slf4jTracer.error("error level")
                    } yield ()

                    // LogEvent Tracing
                    _ <- for {
                        tracer <- tracerLocal.get
                        _ <- tracer.traceWith(
                          LogEvent(
                            Level.Error,
                            "direct log event logging with context",
                            Map("foo" -> "bar")
                          )
                        )
                    } yield ()

                    mockBlock = MockBlock(100, "abcd")
                    mockContext = BlockProductionCtx(
                      Map(
                        CtxKeys.K1 -> "set by the calling function and passed to the called function"
                      )
                    )

                    // Logging a typed event with a typed context
                    _ <- for {
                        tracer <- tracerLocal.get
                        _ <- tracer
                            .contramap(blockProductionContramap(mockContext))
                            .traceWith(mockBlock)
                    } yield ()

                    // contramap scoping -- bringing in a new Contratracer as a given instance into scope.
                    // This allows using [[ContraTracerSyntax]]
                    _ <- Slf4jTracer.contramapScoped(blockProductionContramap(mockContext))(
                      traceWith(mockBlock)
                    )

                } yield ()

            }

        } yield ExitCode.Success
}

/** Demo: `Logger` does not evaluate the argument regardless of log level. If you do a
  * `logger.trace` and set the log level to `info`, it will not print "non-cats side effect"
  */
object LoggerDemo extends IOApp {
    private val logger = Logging.loggerIO("LoggerDemo")

    override def run(args: List[String]): IO[ExitCode] =
        for {
            _ <- logger.trace {
                println("non-cats side effect")
                "logger.trace"
            }
        } yield ExitCode.Success

}
