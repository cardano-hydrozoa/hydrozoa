package hydrozoa.lib.logging

import cats.*
import cats.arrow.*
import cats.data.*
import cats.syntax.all.*

//-- | Formal representation of a tracer arrow as a Kleisli arrow over some
//    -- monad, but tagged so that we know whether it has any effects which will emit
//    -- a trace.
//    data TracerA m a b where
//    -- | An emitting part, followed by a non-emitting part.
//-- The non-emitting part is there so that later emitting parts can be
//    -- tacked-on later.
//Emitting   :: Kleisli m a x -> Kleisli m x b -> TracerA m a b
//    -- | No emitting. There may be side-effects, but they are assumed to be
//    -- benign and will be discarded by 'runTracerA'.
//Squelching :: Kleisli m a b                  -> TracerA m a b

sealed trait TracerA[M[_], A, B]

private def const[A, B](a: A)(b: => B): A = a

extension [M[_], A](t: TracerA[M, A, Unit])

    //    -- | The resulting Kleisli arrow includes all of the effects required to do
    //    -- the emitting part.
    // runTracerA :: Monad m => TracerA m a () -> Kleisli m a ()
    // runTracerA (Emitting emits _noEmits) = emits >>> arr (const ())
    // runTracerA (Squelching     _       ) =           arr (const ())
    def runTracerA(using monadM: Monad[M]): Kleisli[M, A, Unit] = {
        def arrConst = Kleisli(x => monadM.pure(const(())(x)))
        t match {
            case TracerA.Emitting(emits, _noEmits) => emits >>> arrConst
            case TracerA.Squelching(_)             => arrConst

        }
    }

object TracerA:
    case class Emitting[M[_], A, X, B](emitter: Kleisli[M, A, X], nonEmitter: Kleisli[M, X, B])
        extends TracerA[M, A, B]
    case class Squelching[M[_], A, B](squelching: Kleisli[M, A, B]) extends TracerA[M, A, B]

    //
    //    -- | Ignore the input and do not emit. The name is intended to lead to clear
    //    -- and suggestive arrow expressions.
    //    squelch :: Applicative m => TracerA m a ()
    // squelch = compute (const ())
    //
    def squelch[M[_]: Applicative, A]: TracerA[M, A, Unit] = compute(_ => ())

    //    -- | Do an emitting effect. Contrast with 'effect' which does not make the
    //    -- tracer an emitting tracer.
    def emit[M[_], A](f: A => M[Unit])(using appM: Applicative[M]): TracerA[M, A, Unit] = Emitting(
      emitter = Kleisli(f),
      nonEmitter = Kleisli(const(appM.unit))
    )

    //    -- | Do a non-emitting effect. This effect will only be run if some part of
    //    -- the tracer downstream emits (see 'emit').
    def effect[M[_], A, B](f: A => M[B]): TracerA[M, A, B] = Squelching(Kleisli(f))

    //    -- | Pure computation in a tracer: no side effects or emits.
    def compute[M[_], A, B](f: A => B)(using appM: Applicative[M]): TracerA[M, A, B] =
        effect(x => appM.pure(f(x)))

    private object TracerAOps {
        def compose[M[_]: Monad, A, B, C](
            f: TracerA[M, B, C],
            g: TracerA[M, A, B]
        ): TracerA[M, A, C] =
            (f, g) match {
                case (Squelching(l), Squelching(r)) => Squelching(r.andThen(l))

                case (Squelching(l), Emitting(re, rp)) =>
                    //  Crucial: the squelching parts stay together. Could also have written
                    //             = Emitting   (rp . re)      l
                    //  but that would miss opportunities to skip doing work.
                    Emitting(re, rp.andThen(l))

                case (Emitting(le, lp), Squelching(r)) =>
                    //   Contrast with the above clause: here the emitting part comes _after_ the
                    //    squelching part, so the squelching part becomes part of the emitting part.
                    Emitting(r.andThen(le), lp)
                case (Emitting(le, lp), Emitting(re, rp)) =>
                    Emitting(re.andThen(rp).andThen(le), lp)
            }

        def lift[M[_]: Monad, A, B](f: A => B): TracerA[M, A, B] = compute[M, A, B](f)

        // This type checks, but I'm not totally certain its correct.
        // The manual implementation would look like:
        //   (Kleisli((a, c) => s.run(a).map((_, c)))
        def first[M[_]: Monad, A, B, C](fa: TracerA[M, A, B]): TracerA[M, (A, C), (B, C)] = {
            fa match {
                case Squelching(s)  => Squelching(s.first)
                case Emitting(e, p) => Emitting(e.first, p.first)
            }
        }
    }

    given [M[_]: Monad]: Category[[X, Y] =>> TracerA[M, X, Y]] with {
        def id[A]: TracerA[M, A, A] = compute(x => x)

        def compose[A, B, C](f: TracerA[M, B, C], g: TracerA[M, A, B]): TracerA[M, A, C] =
            TracerAOps.compose(f, g)
    }

    given [M[_]: Monad]: Arrow[[X, Y] =>> TracerA[M, X, Y]] with {
        def lift[A, B](f: A => B): TracerA[M, A, B] = TracerAOps.lift(f)

        def compose[A, B, C](f: TracerA[M, B, C], g: TracerA[M, A, B]): TracerA[M, A, C] =
            compose(f, g)

        // This type checks, but I'm not totally certain its correct.
        // The manual implementation would look like:
        //   (Kleisli((a, c) => s.run(a).map((_, c)))
        def first[A, B, C](fa: TracerA[M, A, B]): TracerA[M, (A, C), (B, C)] = TracerAOps.first(fa)
    }

    given [M[_]: Monad]: ArrowChoice[[X, Y] =>> TracerA[M, X, Y]] with {
        def lift[A, B](f: A => B): TracerA[M, A, B] = TracerAOps.lift(f)

        def choose[A, B, C, D](
            f: TracerA[M, A, C]
        )(g: TracerA[M, B, D]): TracerA[M, Either[A, B], Either[C, D]] = (f, g) match {
            case (Squelching(l), Squelching(r)) => Squelching(l +++ r)
            case (Squelching(l), Emitting(re, rp)) =>
                Emitting(Category[[X, Y] =>> Kleisli[M, X, Y]].id +++ re, l +++ rp)
            case (Emitting(le, lp), Squelching(r)) =>
                Emitting(le +++ Category[[X, Y] =>> Kleisli[M, X, Y]].id, lp +++ r)
            case (Emitting(le, lp), Emitting(re, rp)) => Emitting(le +++ re, lp +++ rp)
        }

        def compose[A, B, C](f: TracerA[M, B, C], g: TracerA[M, A, B]): TracerA[M, A, C] =
            TracerAOps.compose(f, g)

        def first[A, B, C](fa: TracerA[M, A, B]): TracerA[M, (A, C), (B, C)] = TracerAOps.first(fa)
    }

    def nat[M[_], N[_], A, B](h: M ~> N)(t: TracerA[M, A, B]): TracerA[N, A, B] = t match {
        case Squelching(k)  => Squelching(k.mapK(h))
        case Emitting(k, l) => Emitting(k.mapK(h), l.mapK(h))
    }
