package hydrozoa.lib.logging

import cats.Monad

/** A small generic message ADT for call sites that don't merit a typed event of their own —
  * typically app entry points (`hydrozoa.app.*`) and test scaffolding. Production actors and shared
  * infrastructure define their own `XYZEvent` ADT and pass `ContraTracer[IO, XYZEvent]`.
  *
  * Pair with [[Slf4jMsgFormat.humanFormat]] to lift into [[LogEvent]] under a fixed routing key.
  * The extension methods on `ContraTracer[F, Slf4jMsg]` give `log.info("…")` ergonomics for any
  * `F[_]: Monad` — typically `IO` (via [[Slf4jTracer.sink]]) or `cats.Id` (via the same `sink`
  * lifted through [[Slf4jTracer.ioToId]] for pure / synchronous call sites).
  */
sealed trait Slf4jMsg

object Slf4jMsg:
    final case class Trace(msg: String) extends Slf4jMsg
    final case class Debug(msg: String) extends Slf4jMsg
    final case class Info(msg: String) extends Slf4jMsg
    final case class Warn(msg: String) extends Slf4jMsg
    final case class Error(msg: String, cause: Option[Throwable] = None) extends Slf4jMsg

/** Lift a [[Slf4jMsg]] into a [[LogEvent]] under [[routingKey]]. */
object Slf4jMsgFormat:
    def humanFormat(routingKey: String)(m: Slf4jMsg): LogEvent = m match
        case Slf4jMsg.Trace(msg) =>
            LogEvent(Level.Trace, msg, routingKey = Some(routingKey))
        case Slf4jMsg.Debug(msg) =>
            LogEvent(Level.Debug, msg, routingKey = Some(routingKey))
        case Slf4jMsg.Info(msg) =>
            LogEvent(Level.Info, msg, routingKey = Some(routingKey))
        case Slf4jMsg.Warn(msg) =>
            LogEvent(Level.Warn, msg, routingKey = Some(routingKey))
        case Slf4jMsg.Error(msg, cause) =>
            LogEvent(Level.Error, msg, cause = cause, routingKey = Some(routingKey))

/** Logger-like extension on a `ContraTracer[F, Slf4jMsg]`. Build the tracer once at construction
  * time, then call `log.info / warn / error / debug / trace` at the call sites:
  *
  * {{{
  *   private val log: ContraTracer[IO, Slf4jMsg] =
  *       Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa.app.Main"))
  *
  *   log.info("Hello world")            // IO[Unit]
  *
  *   private val syncLog: ContraTracer[cats.Id, Slf4jMsg] =
  *       Slf4jTracer.sink
  *           .contramap(Slf4jMsgFormat.humanFormat("Stage1.Model"))
  *           .natTracer(Slf4jTracer.ioToId)
  *
  *   val _ = syncLog.debug("hello in Gen")  // Unit
  * }}}
  */
extension [F[_]: Monad](t: ContraTracer[F, Slf4jMsg])
    def trace(msg: => String): F[Unit] = t.traceWith(Slf4jMsg.Trace(msg))
    def debug(msg: => String): F[Unit] = t.traceWith(Slf4jMsg.Debug(msg))
    def info(msg: => String): F[Unit] = t.traceWith(Slf4jMsg.Info(msg))
    def warn(msg: => String): F[Unit] = t.traceWith(Slf4jMsg.Warn(msg))
    def error(msg: => String, cause: Option[Throwable] = None): F[Unit] =
        t.traceWith(Slf4jMsg.Error(msg, cause))
