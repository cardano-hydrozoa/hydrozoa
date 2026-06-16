package hydrozoa.lib.logging

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import cats.~>
import hydrozoa.lib.logging.ContraTracer.nullTracer
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer

/** Didactic walkthrough of `TracerA` and `ContraTracer`.
  *
  * ==The two layers==
  *
  * {{{
  *   ContraTracer[M, A]
  *     wraps
  *   TracerA[M, A, Unit]
  *     which is one of:
  *
  *     Emitting(emits, noEmits)
  *       emits  : Kleisli[M, A, X]   <-- runs on traceWith
  *       noEmits: Kleisli[M, X, Unit] <-- DROPPED by runTracerA
  *
  *     Squelching(k)
  *       k: Kleisli[M, A, B]          <-- ENTIRE arrow dropped by runTracerA
  * }}}
  *
  * Only `Emitting` produces visible effects when run. `Squelching` is a "ghost" arrow: it still
  * composes with neighbours, but at `runTracerA` time it is replaced by `arr (const ())`. That
  * collapse is why the library can offer lazy combinators (`contramapM`, `traceMaybe`, ...) that
  * only pay their cost when some sink downstream is actually emitting.
  *
  * ==Composition algebra under `>>>`==
  *
  * {{{
  *   g (upstream)     f (downstream)    g >>> f       intuition
  *   ------------     --------------    -----------   ---------------------
  *   Squelching       Squelching        Squelching    no emit anywhere
  *   Emitting         Squelching        Emitting      emit, drop the tail
  *   Squelching       Emitting          Emitting      squelch absorbed
  *                                                     into emit's input
  *   Emitting         Emitting          Emitting      both emits preserved
  * }}}
  *
  * ==Fan-out via Semigroup `combine`==
  *
  * {{{
  *                  x : A
  *                    |
  *           (xt.use &&& yt.use)    -- duplicates the input
  *                    |
  *                +---+---+
  *                v       v
  *              xt.use  yt.use      -- both sinks see the same A
  *                |       |
  *                v       v
  *              (Unit, Unit) --discard--> Unit
  * }}}
  *
  * ==Branching via `traceMaybe` (ArrowChoice `|||`)==
  *
  * {{{
  *                  b : B
  *                    |
  *                classify  (Squelching: runs only if downstream emits)
  *                    |
  *             Either[Unit, A]
  *               /         \
  *           Left(())     Right(a)
  *               |             |
  *           squelch        this.use   <-- only emits on Right
  *               |             |
  *               v             v
  *              Unit          Unit
  * }}}
  *
  * Each test below is self-contained. Read them in order; the comments tell you what the test is
  * demonstrating and which diagram it maps to.
  */
class ContraTracerDemo extends AnyFunSuite:

    // --- helpers ------------------------------------------------------------

    /** A leaf sink that appends every emitted string to `buf`. */
    private def bufferSink(buf: ListBuffer[String]): ContraTracer[IO, String] =
        ContraTracer.emit[IO, String](s => IO { val _ = buf += s })

    /** Pattern-matches the underlying TracerA constructor — useful for verifying the composition
      * algebra above without running anything.
      */
    private def tag[A](t: ContraTracer[IO, A]): String = t.runTracer match
        case TracerA.Emitting(_, _) => "Emitting"
        case TracerA.Squelching(_)  => "Squelching"

    // --- Demo 1: leaf emit --------------------------------------------------

    test("Demo 1 — leaf emit reaches the sink") {
        // ContraTracer.emit lifts a `String => IO[Unit]` into an Emitting tracer.
        // traceWith runs the underlying Kleisli on the given input.
        val buf = ListBuffer.empty[String]
        val sink = bufferSink(buf)

        sink.traceWith("hello").unsafeRunSync()

        assert(buf.toList == List("hello"))
        assert(tag(sink) == "Emitting")
    }

    // --- Demo 2: nullTracer -------------------------------------------------

    test("Demo 2 — nullTracer is silent (the Monoid identity)") {
        // nullTracer is built from TracerA.squelch — the whole arrow is dropped
        // by runTracerA, so traceWith is a no-op. It is also `Monoid.empty`.
        val buf = ListBuffer.empty[String]
        val sink: ContraTracer[IO, String] = nullTracer

        sink.traceWith("ignored").unsafeRunSync()

        assert(buf.isEmpty)
        assert(tag(sink) == "Squelching")
    }

    // --- Demo 3: contramap --------------------------------------------------

    test("Demo 3 — contramap adapts the input type") {
        // Contravariant.contramap turns a ContraTracer[M, A] into a
        // ContraTracer[M, B] using a pure `B => A`. Internally this is
        //     compute(f) >>> tracer.use   (i.e. Squelching >>> Emitting)
        // which the composition table tells us is Emitting.
        val buf = ListBuffer.empty[String]
        val stringSink = bufferSink(buf)
        val intSink: ContraTracer[IO, Int] = stringSink.contramap(_.toString)

        intSink.traceWith(42).unsafeRunSync()

        assert(buf.toList == List("42"))
        assert(tag(intSink) == "Emitting") // Squelching >>> Emitting = Emitting
    }

    // --- Demo 4: Semigroup combine -----------------------------------------

    test("Demo 4 — Semigroup combine fans out to both sinks") {
        // tr1 |+| tr2 == ContraTracer((tr1.use &&& tr2.use) >>> arrDiscard).
        // Both sinks see the same input, and the (Unit, Unit) pair is dropped.
        val bufA = ListBuffer.empty[String]
        val bufB = ListBuffer.empty[String]
        val fanout = bufferSink(bufA) |+| bufferSink(bufB)

        fanout.traceWith("once").unsafeRunSync()

        assert(bufA.toList == List("once"))
        assert(bufB.toList == List("once"))
    }

    // --- Demo 5: laziness proof  (Learn by Doing) --------------------------

    test("Demo 5 — laziness proof (your contribution)") {
        // We instrument the upstream stage's effect with a counter so we can
        // observe *whether the body actually ran*. The Emitting/Squelching
        // tag controls whether `runTracerA` keeps or drops the whole arrow.
        val upstreamSideEffects = new AtomicInteger(0)
        val sinkBuf = ListBuffer.empty[String]

        val liveSink: ContraTracer[IO, String] =
            ContraTracer.emit(s => IO { val _ = sinkBuf += s })

        // Stage that adapts `Int => String` via an IO effect. Crucially, this
        // is built from `TracerA.effect(...)` which is *Squelching*. When
        // composed with a Squelching downstream the whole chain stays
        // Squelching and `runTracerA` drops it. When composed with an
        // Emitting downstream, the squelch is absorbed and the chain emits.
        def withUpstream(downstream: ContraTracer[IO, String]): ContraTracer[IO, Int] =
            downstream.contramapM[Int](i =>
                IO { val _ = upstreamSideEffects.incrementAndGet(); i.toString }
            )

        // TODO(human): build the live and null chains and prove the laziness
        //   1) val liveChain = withUpstream(liveSink)
        //   2) val nullChain = withUpstream(nullTracer[IO, String])
        //   3) run each with `.traceWith(42).unsafeRunSync()`
        //   4) assert sinkBuf == List("42") and upstreamSideEffects == 1
        //      after the live chain ALONE
        //   5) reset counter (or remember the value) and run the null chain
        //   6) assert that upstreamSideEffects did NOT advance — the
        //      contramapM body was discarded entirely
        //   Optionally check `tag(liveChain) == "Emitting"` and
        //   `tag(nullChain) == "Squelching"` to tie back to the algebra.
        ???
    }

    // --- Demo 6: traceAll over a Foldable ----------------------------------

    test("Demo 6 — traceAll fans out across a Foldable") {
        // traceAll(f: B => T[A]) = traceTraversable contramap f.
        // For each element of the resulting collection, the sink fires once.
        val buf = ListBuffer.empty[String]
        val sink = bufferSink(buf)

        // Pass each element of a List[String] to `sink`.
        val listSink: ContraTracer[IO, List[String]] = sink.traceAll(identity)
        listSink.traceWith(List("a", "b", "c")).unsafeRunSync()

        assert(buf.toList == List("a", "b", "c"))

        // The Foldable variant has an explicit Squelching short-circuit:
        // a null upstream collapses to nullTracer rather than iterating.
        val nullList: ContraTracer[IO, List[String]] =
            nullTracer[IO, String].traceAll(identity)
        assert(tag(nullList) == "Squelching")
    }

    // --- Demo 7: composition algebra --------------------------------------

    test("Demo 7 — the composition table holds in practice") {
        // Verify each row of the algebra by pattern-matching the resulting
        // TracerA constructor after composing in the four configurations.
        val buf = ListBuffer.empty[String]
        val emittingSink: ContraTracer[IO, String] = bufferSink(buf)
        val squelchingSink: ContraTracer[IO, String] = nullTracer

        // contramap is `Squelching >>> tracer`, so its output tag mirrors the
        // tag of `tracer`. Use this to construct each cell of the table.
        val sqOverSq: ContraTracer[IO, Int] = squelchingSink.contramap(_.toString)
        val sqOverEm: ContraTracer[IO, Int] = emittingSink.contramap(_.toString)

        assert(tag(sqOverSq) == "Squelching") // Sq >>> Sq = Sq
        assert(tag(sqOverEm) == "Emitting") // Sq >>> Em = Em

        // For the "Emitting upstream" rows, build an emitting stage via
        // ContraTracer.emit and `|+|` with a downstream sink — combine
        // produces Emitting whenever *either* branch is Emitting.
        val emittingStage: ContraTracer[IO, String] =
            ContraTracer.emit(_ => IO.unit)

        assert(tag(emittingStage |+| squelchingSink) == "Emitting") // Em + Sq
        assert(tag(emittingStage |+| emittingSink) == "Emitting") // Em + Em
    }

    // --- Demo 8 (bonus): natTracer ----------------------------------------

    test("Demo 8 — natTracer changes the underlying monad") {
        // natTracer applies a natural transformation `M ~> N` to every
        // Kleisli inside the arrow. Here we lift an IO tracer through the
        // identity natural transformation IO ~> IO just to show the
        // mechanics; in real code you might lift IO into a Kleisli/Reader
        // stack with IO at the base.
        val buf = ListBuffer.empty[String]
        val sink = bufferSink(buf)
        val ioToIo: IO ~> IO = new (IO ~> IO) { def apply[A](fa: IO[A]): IO[A] = fa }
        val lifted: ContraTracer[IO, String] = sink.natTracer(ioToIo)

        lifted.traceWith("through-nat").unsafeRunSync()

        assert(buf.toList == List("through-nat"))
    }
