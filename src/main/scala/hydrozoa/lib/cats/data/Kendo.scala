package hydrozoa.lib.cats.data

import cats.*
import cats.data.*
import cats.syntax.all.*

/** A `Kendo` is a Kliesli Endomorphism.
  *
  * The following types are isomorphic:
  *   - Kendo[F[_], A]
  *   - StateT[F, A, Unit]
  *   - A => F[A]
  *
  * Kendos are particularly useful in two situations:
  *   - Builder monads that validate-as-they-build. It allows applying an endomorphism to a type and
  *     to possible return an error at each step.
  *   - "Execution" monads that start with an initial state and apply transactions or computations
  *     to that state that may fail or have other effects.
  */
object Kendo {
    type Kendo[F[_], A] = Kleisli[F, A, A]

    /** Combines a list of Kleisli endomorphisms into a single Kleisli endomorphism. I.e., given a
      * list of function f1, f2, ..., fn, each of type `A => F[B]`` for some monad `F[_]`, this is
      * the result of
      *
      * `x => for { r1 <- f1(x); r2 <- f2(r1); (...); rn <- fn(xnMinus1)} yield rn `
      *
      * @param kendos
      * @tparam F
      * @tparam A
      * @return
      */
    def kendoFold[F[_], A](kendos: Seq[Kendo[F, A]])(implicit monadF: Monad[F]): Kendo[F, A] = {
        val kPure: Kendo[F, A] = Kleisli(monadF.pure)
        val foldFunc = (soFar: Kendo[F, A], next: Kendo[F, A]) => soFar.andThen(next)
        kendos.foldLeft(kPure)(foldFunc)
    }

    /** Like [[kendoFold]], but returns a computation that will (effectfully) produce all list of
      * all intermediate results. Note that this means if your underlying monad is Either, you will
      * get a Either[Error, List[A]] -- it will return the first error encountered and no (partial)
      * list of results.
      */
    def kendoScan[F[_], A](
        kendos: Seq[Kendo[F, A]]
    )(implicit monadF: Monad[F]): Kleisli[F, A, Seq[A]] = {
        val kPure: Kendo[F, A] = Kleisli(monadF.pure)
        val foldFunc = (soFar: Kendo[F, A], next: Kendo[F, A]) => soFar.andThen(next)
        kendos.scan(kPure)(foldFunc).sequence
    }

    /** If the monad underlying your Kendo is a bifunctor where one side designates an error (such
      * as Either), you might want to observe the partial results. This function is like
      * [[kendoScan]], but will return an error alongside the partial results if it is not
      * successful.
      *
      * @param kendos
      *   A list of Kleislis parameterized over a monad F that can throw errors of type E and return
      *   values of type A, such as Either[E,A]
      * @tparam F
      *   A monad that can throw errors
      * @tparam E
      *   The error type throwable by the monad
      * @tparam A
      *   the return type of the monad
      * @return
      *   If an error is encountered, the actual error encountered and a sequence of partial
      *   results. Otherwise, a complete list of results.
      */
    def kendoObserve[F[_, _], E, A](kendos: Seq[Kendo[[X] =>> F[E, X], A]])(implicit
        monadF: Monad[[X] =>> F[(E, NonEmptyVector[A]), X]],
        bifunctorF: Bifunctor[F]
    ): Kleisli[[X] =>> F[(E, Vector[A]), X], A, Vector[A]] = {

        // Take something like
        //    `A => Either[E,A]`
        // and turn it into
        //    `NonEmptyVector[A]` => Either[(E, NonEmptyVector[A]), NonEmptyVector[A]]
        // such that the "input" is always the last entry in the vector.
        def liftKendo(
            kendo: Kendo[[X] =>> F[E, X], A]
        ): Kendo[[X] =>> F[(E, NonEmptyVector[A]), X], NonEmptyVector[A]] =
            Kleisli((partialResults: NonEmptyVector[A]) => {
                val previousResult = partialResults.last
                kendo
                    .run(previousResult)
                    .bimap(
                      error => (error, partialResults),
                      thisRes => partialResults.append(thisRes)
                    )
            })

        Kleisli((initial: A) =>
            kendoFold(kendos.map(liftKendo))
                .run(NonEmptyVector.one(initial))
                .bimap(_.map(_.toVector), _.toVector)
        )
    }
}
