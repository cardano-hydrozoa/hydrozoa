package hydrozoa.lib.logging

import cats.{Eval, Monad}

/** A small generic message ADT for call sites that don't merit a typed event of their own —
  * typically app entry points (`hydrozoa.app.*`) and test scaffolding. Production actors and shared
  * infrastructure define their own `XYZEvent` ADT and pass `ContraTracer[IO, XYZEvent]`.
  *
  * Pair with [[Slf4jMsgFormat.humanFormat]] to lift into [[LogEvent]] under a fixed routing key.
  * The extension methods on `ContraTracer[F, Slf4jMsg]` give `log.info("…")` ergonomics for any
  * `F[_]: Monad` — typically `IO` (via [[Slf4jTracer.sink]]). The message is held behind an `Eval`
  * so it is not rendered unless the level is enabled (see [[Slf4jTracer.levelGated]]).
  */
sealed trait Slf4jMsg

object Slf4jMsg:
    final case class Trace(msg: Eval[String]) extends Slf4jMsg
    final case class Debug(msg: Eval[String]) extends Slf4jMsg
    final case class Info(msg: Eval[String]) extends Slf4jMsg
    final case class Warn(msg: Eval[String]) extends Slf4jMsg
    final case class Error(msg: Eval[String], cause: Option[Throwable] = None) extends Slf4jMsg

/** Lift a [[Slf4jMsg]] into a [[LogEvent]] under [[routingKey]], keeping the message deferred. */
object Slf4jMsgFormat:
    def humanFormat(routingKey: String)(m: Slf4jMsg): LogEvent = m match
        case Slf4jMsg.Trace(msg) =>
            LogEventTyped(
              Level.Trace,
              Some(routingKey),
              msg.map(Rendered(_, Map.empty[String, String]))
            )
        case Slf4jMsg.Debug(msg) =>
            LogEventTyped(
              Level.Debug,
              Some(routingKey),
              msg.map(Rendered(_, Map.empty[String, String]))
            )
        case Slf4jMsg.Info(msg) =>
            LogEventTyped(
              Level.Info,
              Some(routingKey),
              msg.map(Rendered(_, Map.empty[String, String]))
            )
        case Slf4jMsg.Warn(msg) =>
            LogEventTyped(
              Level.Warn,
              Some(routingKey),
              msg.map(Rendered(_, Map.empty[String, String]))
            )
        case Slf4jMsg.Error(msg, cause) =>
            LogEventTyped(
              Level.Error,
              Some(routingKey),
              msg.map(Rendered(_, Map.empty[String, String], cause))
            )

/** Logger-like extension on a `ContraTracer[F, Slf4jMsg]`. Build the tracer once at construction
  * time, then call `log.info / warn / error / debug / trace` at the call sites:
  *
  * {{{
  *   private val log: ContraTracer[IO, Slf4jMsg] =
  *       Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("hydrozoa.app.Main"))
  *
  *   log.info("Hello world")            // IO[Unit]
  *
  * }}}
  */
extension [F[_]: Monad](t: ContraTracer[F, Slf4jMsg])
    def trace(msg: => String): F[Unit] = t.traceWith(Slf4jMsg.Trace(Eval.later(msg)))
    def debug(msg: => String): F[Unit] = t.traceWith(Slf4jMsg.Debug(Eval.later(msg)))
    def info(msg: => String): F[Unit] = t.traceWith(Slf4jMsg.Info(Eval.later(msg)))
    def warn(msg: => String): F[Unit] = t.traceWith(Slf4jMsg.Warn(Eval.later(msg)))
    def error(msg: => String, cause: Option[Throwable] = None): F[Unit] =
        t.traceWith(Slf4jMsg.Error(Eval.later(msg), cause))
