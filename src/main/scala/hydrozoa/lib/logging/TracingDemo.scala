package hydrozoa.lib.logging

import cats.Id
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.toContravariantOps

/** A tiny worked example showing the two flavors of [[ContraTracer]] usage in Hydrozoa, with the
  * key claim that **there is only one sink**: the IO-flavored `Slf4jTracer.sink`. Pure /
  * synchronous code paths obtain their `ContraTracer[Id, …]` by lifting that one sink through a
  * natural transformation `IO ~> Id` (literally `_.unsafeRunSync()`).
  *
  * Run with `sbt "runMain hydrozoa.lib.logging.TracingDemo"`.
  */
object TracingDemo extends IOApp:

    // ============================================================
    // 1. Typed event ADT + format — same shape as every other
    //    `XYZEvent` / `XYZEventFormat` pair in the tree.
    // ============================================================

    sealed trait DemoEvent
    object DemoEvent:
        final case class Started(label: String) extends DemoEvent
        final case class Finished(label: String, result: Int) extends DemoEvent

    object DemoEventFormat:
        def humanFormat(e: DemoEvent): LogEvent =
            val ev = LogEvent.From(Map.empty, "TracingDemo")
            import ev.*
            e match
                case DemoEvent.Started(label)          => info(s"[$label] started")
                case DemoEvent.Finished(label, result) => info(s"[$label] finished: $result")

    // ============================================================
    // 2. The one sink: the IO-flavored `Slf4jTracer.sink` lifted to
    //    the typed event channel via `contramap`.
    // ============================================================

    val ioTracer: ContraTracer[IO, DemoEvent] =
        Slf4jTracer.sink.contramap(DemoEventFormat.humanFormat)

    // ============================================================
    // 3. The Id-flavored tracer is the same IO tracer lifted through
    //    the shared `IO ~> Id` natural transformation living on
    //    [[Slf4jTracer.ioToId]]. Pure / synchronous callers
    //    (ScalaCheck `Gen` bodies, synchronous tx builders, …) take
    //    the Id tracer and emit synchronously via `traceWith`.
    // ============================================================

    val idTracer: ContraTracer[Id, DemoEvent] = ioTracer.natTracer(Slf4jTracer.ioToId)

    // ============================================================
    // 4. Two worker functions — one IO, one pure — both emit the
    //    same typed events to the same SLF4J back-end.
    // ============================================================

    def ioWork(tracer: ContraTracer[IO, DemoEvent]): IO[Int] =
        for
            _ <- tracer.traceWith(DemoEvent.Started("IO"))
            result = (1 to 10).sum
            _ <- tracer.traceWith(DemoEvent.Finished("IO", result))
        yield result

    /** A synchronous worker — the `tracer.traceWith(…)` calls return `Unit` because `F = Id`, so
      * this entire function is a plain non-`IO` expression. The events still reach SLF4J because
      * the Id tracer is the IO sink natural-transformed through `_.unsafeRunSync()`.
      */
    def pureWork(tracer: ContraTracer[Id, DemoEvent]): Int =
        val _: Unit = tracer.traceWith(DemoEvent.Started("pure"))
        val result = (1 to 10).sum
        val _: Unit = tracer.traceWith(DemoEvent.Finished("pure", result))
        result

    // ============================================================
    // 5. Run both and print their results. Inspect the SLF4J output
    //    (root logger or `TracingDemo` logger) to see the four lines.
    // ============================================================

    override def run(args: List[String]): IO[ExitCode] =
        for
            ioResult <- ioWork(ioTracer)
            _ <- IO.println(s"IO   result: $ioResult")
            pureResult = pureWork(idTracer)
            _ <- IO.println(s"pure result: $pureResult")
        yield ExitCode.Success
