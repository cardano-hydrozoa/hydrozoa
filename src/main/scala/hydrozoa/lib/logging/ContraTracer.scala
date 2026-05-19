package hydrozoa.lib.logging

import cats.*
import cats.arrow.*
import cats.syntax.all.*
import hydrozoa.lib.logging.ContraTracer.nullTracer
import hydrozoa.lib.logging.TracerA.Squelching

/** Ported from https://github.com/avieth/contra-tracer/blob/master/src/Control/Tracer.hs
  *
  * Why I ported (this will eventually go into a PR comment, just jotting down here for now): The
  * regular [[Tracer]] is a fine format for logging out to log4cats -- does accomplish the
  * _contextual_ logging goals. But it's too closely bound to the message format, particularly with
  * respect with the `ctx : Map[String,String]` parameter.
  *
  * The primary issue I was running into when trying to integrate the original `Tracer` is getting
  * messages like:
  *
  * 09:24:00.520 INFO ConsensusActor.1 [ackId=(0,56) argumentType=AckBlock peer=1 ackType=minor
  * ack={"minor":{"ackId":[0,56]},"blockNum":47} blockNumber=47] block confirmed: block=47
  * type=minor v9.1
  *
  * Where things have just been lumped into the context using different keys (block, blockNum,
  * ack.blockNum) and then put into the log message as well.
  *
  * The most obvious solution is to go from a stringly-typed context to a more strongly typed
  * context, where we're not adding random strings into the context, but we're adding actual types
  * like `Block`. Then `Block` can have a `BlockContext` trait that can map down onto to the `(String, String)` format,
  *  and something like `AckBlock` can extend the `BlockContext` trait -- this give us something like
  *
  *    block.toContext.toString
  *      ==
  *    ("blockNumber" -> block.blockNumber.toString, "kzgCommitment" -> block.kzgCommitment.toString)
  *
  *   And if we use these traits diligently, then we don't risk logging the block number 3 different times by adding
  *   it to the context under different keys.
  *
  *   Basically, within a given actor, I believe:
  *   - Events should be enumerated and typed
  *   - Things that _may_ be in the context should be enumerated and typed to avoid duplicated context keys. This
  *     is especially useful for functions that might get fed a particular context key from a calling function that they
  *     also should add if its not already there.
  *   - We should use ContraTracer to contramap these onto the more general LogEvent type.
  *
  *   We don't care much about these other parts yet, but:
  *   - This also gives us more flexibility to log in different ways -- to files, to a remote log capture system, to
  *     journald
  *   - This sets us up for metrics/telemetry by changing the `emit` effect, and allows to combine these with loggers
  *     via the semi-group instance
  *   - Sets us up to fix the bug where `Tracer`, as written, evaluates the argument regardless of trace level
  *   - Sets us up for an asynchronous `asyncJsonToDiskTracer[A : Codec[A]] : ContraTracer[IO, A]` that spawns a fiber to
  *     do expensive serialization and IO (writting to disk, etc.)
  *
  * @param runTracer
  * @tparam M
  * @tparam A
  */
case class ContraTracer[M[_], A](runTracer: TracerA[M, A, Unit]) {
    import TracerA.given
    import ContraTracer.given

    /** Run a tracer with a given input.
      */
    def traceWith(a: A)(using Monad[M]): M[Unit] = this.runTracer.runTracerA.run(a)

    /** -- | Inverse of 'arrow'. Useful when writing arrow tracers which use a -- contravariant
      * tracer (the newtype in this module).
      */
    def use: TracerA[M, A, Unit] = runTracer

    /** Run a tracer only for the Just variant of a Maybe. If it's Nothing, the 'nullTracer' is used
      * (no output). The arrow representation allows for proper laziness: if the tracer parameter
      * does not produce any tracing effects, then the predicate won't even be evaluated. Contrast
      * with the simple contravariant representation as
      * @a
      *   -> m ()@, in which the predicate _must_ be forced no matter what, because it's impossible
      *   to know a priori whether that function will not produce any tracing effects.
      */
    def traceMaybe[B](f: B => Option[A])(using Monad[M]): ContraTracer[M, B] = {
        def classify: TracerA[M, B, Either[Unit, A]] =
            Arrow[[X, Y] =>> TracerA[M, X, Y]].lift((x: B) =>
                f(x).map(Right(_)).getOrElse(Left(()))
            )
        ContraTracer(classify >>> (TracerA.squelch ||| this.use))
    }

    /** A monadic version of `traceMaybe`.
      */
    def traceMaybeM[B](f: B => M[Option[A]])(using Monad[M]): ContraTracer[M, B] = {
        def classify: TracerA[M, B, Either[Unit, A]] =
            TracerA.effect((x: B) => f(x).map(_.map(Right(_)).getOrElse(Left(()))))
        ContraTracer(classify >>> (TracerA.squelch ||| this.use))
    }

    /** Uses 'traceMaybe' to give a tracer which emits only if a predicate is true.
      */
    def squelchUnless(p: (A => Boolean))(using Monad[M]): ContraTracer[M, A] =
        traceMaybe(a => Some(a).filter(p))

    /** A monadic version of `squelchUnless`. */
    def squelchUnlessM(p: A => M[Boolean])(using Monad[M]): ContraTracer[M, A] =
        traceMaybeM(a => p(a).map(prop => if prop then Some(a) else None))

    def traceTraversable[T[_]: Foldable](using Monad[M]): ContraTracer[M, T[A]] = runTracer match {
        case Squelching(_) => nullTracer
        case _ => ContraTracer(TracerA.emit((t: T[A]) => Foldable[T].traverse_(t)(this.traceWith)))
    }

    def traceAll[T[_]: Foldable, B](f: (B => T[A]))(using Monad[M]): ContraTracer[M, B] =
        Contravariant[[X] =>> ContraTracer[M, X]].contramap(this.traceTraversable)(f)

    /** A contravariant transformation of a tracer using a Kleisli arrow
      * @param f
      *   Kleisli arrow which is evaluated only if a downstream tracer emits
      */
    def contramapM[B](f: B => M[A])(using Monad[M]): ContraTracer[M, B] = ContraTracer(
      TracerA.effect(f) >>> this.use
    )

    /** Use a natural transformation to change the @m@ type. This is useful, for instance, to use
      * concrete IO tracers in monad transformer stacks that have IO as their base.
      */
    def natTracer[N[_], S](h: M ~> N) = ContraTracer(TracerA.nat(h)(this.use))
}

object ContraTracer {
    import TracerA.given

    given [M[_]: Monad]: Contravariant[[X] =>> ContraTracer[M, X]] with {
        override def contramap[A, B](tracer: ContraTracer[M, A])(f: B => A): ContraTracer[M, B] =
            ContraTracer(Arrow[[X, Y] =>> TracerA[M, X, Y]].lift(f) >>> tracer.use)
    }

    /** tr1.combine(tr2) will run tr1 and then tr2 with the same input.
      */
    given [M[_]: Monad, S]: Semigroup[ContraTracer[M, S]] with {
        override def combine(x: ContraTracer[M, S], y: ContraTracer[M, S]): ContraTracer[M, S] = {
            def const[A, B](a: A)(b: => B): A = a
            def discard(tunit: (Unit, Unit)): Unit = const(())(tunit)
            def arrDiscard = Arrow[[X, Y] =>> TracerA[M, X, Y]].lift(discard)
            ContraTracer((x.use &&& y.use) >>> arrDiscard)
        }
    }

    given [M[_]: Monad, S]: Monoid[ContraTracer[M, S]] with {
        override def empty: ContraTracer[M, S] = nullTracer

        override def combine(x: ContraTracer[M, S], y: ContraTracer[M, S]): ContraTracer[M, S] = {
            def const[A, B](a: A)(b: => B): A = a
            def discard(tunit: (Unit, Unit)): Unit = const(())(tunit)
            def arrDiscard = Arrow[[X, Y] =>> TracerA[M, X, Y]].lift(discard)
            ContraTracer((x.use &&& y.use) >>> arrDiscard)
        }
    }

    /** -- | Make an emitting tracer from a callback. -- mkTracer :: Applicative m => (a -> m ()) ->
      * Tracer m a mkTracer = Tracer . Arrow.emit
      */
    def apply[M[_]: Applicative, A](f: A => M[Unit]): ContraTracer[M, A] = ContraTracer(
      TracerA.emit(f)
    )

    /** -- | A tracer which does nothing. nullTracer :: Monad m => Tracer m a nullTracer = Tracer
      * Arrow.squelch
      */
    def nullTracer[M[_]: Monad, A]: ContraTracer[M, A] = ContraTracer(TracerA.squelch)

    /** def -- | Create a simple contravariant tracer which runs a given side-effect. emit ::
      * Applicative m => (a -> m ()) -> Tracer m a emit f = Tracer (Arrow.emit f)
      */
    def emit[M[_]: Applicative, A](f: A => M[Unit]): ContraTracer[M, A] = ContraTracer(
      TracerA.emit(f)
    )
}

/** Syntax for using an ambient ContraTracer from a `given`
  */
object ContraTracerSyntax {

    def traceWith[M[_]: Monad, A](a: A)(using ct: ContraTracer[M, A]): M[Unit] =
        ct.runTracer.runTracerA.run(a)

    def use[M[_], A](using ct: ContraTracer[M, A]): TracerA[M, A, Unit] = ct.runTracer

    def traceMaybe[M[_]: Monad, A, B](f: B => Option[A])(using
        ct: ContraTracer[M, A]
    ): ContraTracer[M, B] =
        ct.traceMaybe(f)
//
//        /** A monadic version of `traceMaybe`.
//         */
//        def traceMaybeM[B](f: B => M[Option[A]])(using Monad[M]): ContraTracer[M, B] = {
//            def classify: TracerA[M, B, Either[Unit, A]] =
//                TracerA.effect((x: B) => f(x).map(_.map(Right(_)).getOrElse(Left(()))))
//
//            ContraTracer(classify >>> (TracerA.squelch ||| this.use))
//        }
//
//        /** Uses 'traceMaybe' to give a tracer which emits only if a predicate is true.
//         */
//        def squelchUnless(p: (A => Boolean))(using Monad[M]): ContraTracer[M, A] =
//            traceMaybe(a => Some(a).filter(p))
//
//        /** A monadic version of `squelchUnless`. */
//        def squelchUnlessM(p: A => M[Boolean])(using Monad[M]): ContraTracer[M, A] =
//            traceMaybeM(a => p(a).map(prop => if prop then Some(a) else None))
//
//        def traceTraversable[T[_] : Foldable](using Monad[M]): ContraTracer[M, T[A]] = runTracer match {
//            case Squelching(_) => nullTracer
//            case _ => ContraTracer(TracerA.emit((t: T[A]) => Foldable[T].traverse_(t)(this.traceWith)))
//        }
//
//        def traceAll[T[_] : Foldable, B](f: (B => T[A]))(using Monad[M]): ContraTracer[M, B] =
//            Contravariant[[X] =>> ContraTracer[M, X]].contramap(this.traceTraversable)(f)
//
//        /** A contravariant transformation of a tracer using a Kleisli arrow
//         *
//         * @param f
//         * Kleisli arrow which is evaluated only if a downstream tracer emits
//         */
//        def contramapM[B](f: B => M[A])(using Monad[M]): ContraTracer[M, B] = ContraTracer(
//            TracerA.effect(f) >>> this.use
//        )
//
//        /** Use a natural transformation to change the @m@ type. This is useful, for instance, to use
//         * concrete IO tracers in monad transformer stacks that have IO as their base.
//         */
//        def natTracer[N[_], S](h: M ~> N) = ContraTracer(TracerA.nat(h)(this.use))
//    }
}
