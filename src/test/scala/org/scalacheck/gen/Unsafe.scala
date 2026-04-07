package org.scalacheck.gen

import cats.Monad
import cats.syntax.all.*
import org.scalacheck
import org.scalacheck.Gen
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed

object Unsafe {

    /** Promotes a monadic generator to a generator of monadic values */
    def promote[M[_], A](m: M[Gen[A]])(using monadM: Monad[M]): Gen[M[A]] = {
        delay.map(m.map)
    }

    /** Randomly generates a function of type `Gen[A] => A`, which you can then use to evaluate
      * generators \Mostly useful in implementing 'promote'.
      */
    // NOTE (Peter, 2025-12-17): this differs from the haskell version, because scalacheck doesn't expose the internals
    // in the same way.
    //
    // I'm _pretty_ sure that its equivalent, though.
    def delay[A]: Gen[Gen[A] => A] =
        // do I need to slide the seed here?
        Gen.gen((p: Parameters, sd: Seed) =>
            Gen.r(r = Some(genA => genA.pureApply(p, sd)), sd = sd)
        )
}
