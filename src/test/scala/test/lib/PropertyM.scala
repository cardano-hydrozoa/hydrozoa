package test.lib

import cats.*
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import org.scalacheck.Prop.{collect, forAll, propBoolean, undecided}
import org.scalacheck.util.Pretty
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scala.sys.process.*
import test.lib.Gen.Unsafe.promote

object PropertyMTest extends Properties("PropertyM") {

    import PropertyM.*

    // This property demonstrates calling out to an external process.
    // Its more interesting in Haskell, because it is _necessarily_ in IO in haskell, whereas regular scala(check)
    // lets you do arbitrary side effects.
    //
    // However, we do wrap the IO in `IO.blocking`, which means it can then be bound in a PropertyM for/yield via `run`
    val _ = property("`factor` cli utility works") = {

        def factor(n: Int): IO[Array[Int]] = {
            def parse(s: String): Array[Int] = s.split("\\s+").tail.map(_.toInt)

            IO.blocking(("factor " ++ n.toString).!!).map(parse)
        }

        monadicIO(
          forAllM(
            gen = Gen.posNum[Int],
            k = n =>
                for {
                    factors <- run(factor(n))
                    _ <- PropertyM.assert(factors.fold(1)(_ * _) == n)
                } yield true
          )
        )
    }

    // This demonstrates two things:
    // - Use of "pick" to avoid nested forAlls; we can bind generated values in the PropertyM monad
    // - That we can do arbitrary effects (IO) in between calls to the generators
    val _ = property("commutativity of integer addition") = monadicIO(for {
        int1 <- pick[IO, Int](Arbitrary.arbitrary[Int])
        _ <- run(IO.println("Run some IO in between the calls."))
        int2 <- pick[IO, Int](Arbitrary.arbitrary[Int])
        _ <- run(IO.println(s"($int1, $int2)"))
        _ <- assert(int1 + int2 == int2 + int1)
    } yield true)

    // This demonstrates what it looks like when we generate multiple arguments. They are reported
    // correctly in the error message.
    val _ = property("multiple arguments generated are reported correctly") = monadicIO(
      for {
          _ <- pick[IO, Int](Arbitrary.arbitrary[Int])
          _ <- pick[IO, String](Gen.asciiPrintableStr)
      } yield false
    )

    // This demonstrates running multiple PropertyM's within the monad.
    // Note that the labels appear to print in reverse order:
    //
    // """
    //  ! PropertyM.assertWith Example: Falsified after 0 passed tests.
    //  > Labels of failing property:
    //  Assertion failed
    //  Failed: My third predicate
    //  Passed: My second predicate
    //  Passed: My first predicate
    // """
    val _ = property("assertWith Example") = monadicIO(
      for {
          _ <- assertWith[IO](true, "My first predicate")
          _ <- assertWith[IO](true, "My second predicate")
          _ <- assertWith(false, "My third predicate")
      } yield true
    )

    // Demo: collect the generation statistics for a single generated value
    val _ = property("monitor example 1") = monadicIO(for {
        e <- pick[IO, Int](Gen.choose(0, 10))
        _ <- monitor(collect(e))
    } yield true)

    // Demo: add "Failure!" as a message to the counter example.
    val _ = property("monitor example 2") = {
        // "counterexample" from quickcheck, add a message to a failing property.
        def counterexample(msg: String)(prop: Prop): Prop = msg |: prop

        monadicIO(
          for {
              _ <- monitor(counterexample("Failure!"))
          } yield false
        )
    }

    // Demo: using "stop" to halt execution
    val _ = property("stop example") = {
        monadicIO(
          for {
              _ <- stop[IO, Boolean, Unit](true)
          } yield false // usually this would cause the property to fail, but we called "stop"
        )
    }

    // Using "pre". This demonstrates that test cases are skipped if the pre-condition isn't satisfied.
    // If you examine the output from this function, you'll see that many integer values were generated,
    // all odd ones were discarded (and don't count towards "passed" tests), and that the test eventually
    // fails
    val _ = property("`pre` demo") = {
        monadicIO(
          for {
              int <- pick[IO, Int](
                Gen.frequency(
                  (10, Gen.const(0)),
                  (10, Gen.const(1)),
                  (1, Gen.const(2))
                )
              )
              _ <- monitor[IO](collect(int))
              _ <- pre[IO](int % 2 == 0)
              _ <- assert(int == 0)
          } yield true
        )
    }

    // `monadicIO` should catch otherwise-unhandled exceptions and turn them into property failures.
    val _ = property("demo: thrown exceptions") = {
        monadicIO(
          for {
              _ <- run(
                throw new RuntimeException("This should just fail the test, not crash the suite")
              )
          } yield true
        )
    }

}

object PropertyM:
    /** Like 'assert' but allows caller to specify an explicit message to show on failure.
      */
    def assertWith[M[_]](condition: Boolean, msg: String)(using
        monadM: Monad[M]
    ): PropertyM[M, Unit] = {
        val prefix = if condition then "Passed: " else "Failed: "
        for {
            _ <- monitor(prop => (prefix ++ msg) |: prop)
            _ <- assert(condition)
        } yield ()
    }

    /** Allows embedding non-monadic properties into monadic ones.
      */
    def assert[M[_]](bool: Boolean)(using Monad[M]): PropertyM[M, Unit] = {
        // CHECK ME: I assume this is what we want, but I'm not positive.
        // In quickcheck, unit is `Testable`, but in scalacheck it isn't.
        given propUnit: (Unit => Prop) = _ => Prop(false)

        if bool then monadForPropM.pure(()) else fail_("Assertion failed")
    }

    /** Short-circuit execution and fail with the given message.
      */
    def fail_[M[_], A](s: String)(using monadM: Monad[M], toProp: A => Prop): PropertyM[M, A] =
        stop(s |: false)

    // FIXME: Error messages appear to be reported in reverse chronological order?

    /** Stop execution by short-circuiting and exiting with this prop
      */
    def stop[M[_], P, A](prop: P)(using toProp: P => Prop, monadM: Monad[M]): PropertyM[M, A] =
        PropertyM(_ => Gen.const(monadM.pure(toProp(prop))))

    // NOTE (from the quickcheck version): should think about strictness/exceptions here

    /** Allows making observations about the test data
      */
    def monitor[M[_]](f: Prop => Prop)(using monadM: Monad[M]): PropertyM[M, Unit] =
        PropertyM(k => k(()).map((m: M[Prop]) => m.map(f)))

    // -- should be called lift?

    /** Tests preconditions. Unlike 'assert' this does not cause the property to fail, rather it
      * discards them just like using the implication combinator 'Test.QuickCheck.Property.==>'.
      *
      * This allows representing the <https://en.wikipedia.org/wiki/Hoare_logic Hoare triple>
      *
      * > {p} x ← e{q}
      *
      * as
      * ```
      * pre p
      * x <- run e
      * assert q
      * ```
      */
    def pre[M[_]](condition: Boolean)(using Monad[M]): PropertyM[M, Unit] =
        if condition then monadForPropM.pure(()) else stop(undecided)

    /** The <https://en.wikipedia.org/wiki/Predicate_transformer_semantics#Weakest_preconditions
      * weakest precondition> > wp(x ← e, p) can be expressed as in code as `wp(e, (x => p))`.
      */
    def wp[M[_], A, B](m: M[A], k: A => PropertyM[M, B])(using monadM: Monad[M]): PropertyM[M, B] =
        run(m) >>= k

    /** | The lifting operation of the property monad. Allows embedding monadic 'IO'-actions in properties.
      */
    def run[M[_], A](m: M[A])(using monadM: Monad[M]): PropertyM[M, A] =
        PropertyM(x => promote(x).map(k => m >>= k))

    /** Quantification in monadic properties to 'pick', with a notation similar to 'forAll'. Note:
      * values generated by 'forAllM' do not shrink.
      */
    def forAllM[M[_], A, B](gen: Gen[A], k: A => PropertyM[M, B])(using
        monadM: Monad[M],
        pp: A => Pretty
    ): PropertyM[M, B] =
        pick(gen) >>= k

    /** Quantification in a monadic property, fits better with /do-notation/ than 'forAllM'. Note:
      * values generated by 'pick' do not shrink.
      */
    def pick[M[_], A](gen: Gen[A])(using monadM: Monad[M], pp: A => Pretty): PropertyM[M, A] =
        PropertyM(k =>
            for {
                a <- gen
                mp <- k(a)
            } yield for {
                p <- mp
            } yield forAll(Gen.const(a))(_ => p)
        )

    // ===================================
    // run functions
    // ===================================

    /** Runs the property monad for 'IO'-computations. */
    def monadicIO[A](prop: PropertyM[IO, A])(using toProp: A => Prop, ioRuntime: IORuntime): Prop =
        // NOTE: I'm pretty sure this is what we want -- Prop.secure evaluates its argument only at the time
        // it is actually referenced, so we should be catching the exception here
        monadic(runner = (ioProp: IO[Prop]) => Prop.secure(ioProp.unsafeRunSync()), m = prop)

    /** Given an arbitrary monadic runner and a PropertyM, return a Prop */
    def monadic[M[_], A](runner: M[Prop] => Prop, m: PropertyM[M, A])(using
        toProp: A => Prop,
        monadM: Monad[M]
    ): Prop =
        Prop.secure(monadic1(m).map(runner).sample.get)

    def monadic1[M[_], A](
        m: PropertyM[M, A]
    )(using toProp: A => Prop, monadM: Monad[M]): Gen[M[Prop]] =
        m.unPropertyM(prop => monadM.pure(toProp(prop)))

    // ===================================
    // Type class stuff
    // ===================================

    // What is the correct way to make these instances available? just as implicit defs?

    implicit def functorForPropM[M[_]]: Functor[[A] =>> PropertyM[M, A]] =
        new Functor[[A] =>> PropertyM[M, A]] {
            override def map[A, B](fa: PropertyM[M, A])(f: A => B): PropertyM[M, B] = fa.map(f)
        }

    implicit def applicativeForPropM[M[_]](using Monad[M]): Applicative[[A] =>> PropertyM[M, A]] =
        new Applicative[[A] =>> PropertyM[M, A]] {
            override def pure[A](x: A): PropertyM[M, A] = PropertyM(k => k(x))

            override def ap[A, B](mf: PropertyM[M, A => B])(mx: PropertyM[M, A]): PropertyM[M, B] =
                mf.bind(f => mx.bind(x => pure(f(x))))
        }

    implicit def monadForPropM[M[_]](using Monad[M]): Monad[[A] =>> PropertyM[M, A]] =
        new Monad[[A] =>> PropertyM[M, A]] {
            override def pure[A](x: A): PropertyM[M, A] = PropertyM(k => k(x))

            override def flatMap[A, B](fa: PropertyM[M, A])(
                f: A => PropertyM[M, B]
            ): PropertyM[M, B] = fa.bind(f)

            // FIXME: I don't know much about this.
            override def tailRecM[A, B](a: A)(f: A => PropertyM[M, Either[A, B]]): PropertyM[M, B] =
                ???
        }

/** The property monad is really a monad transformer that can contain
  * monadic computations in the monad @m@ it is parameterized by:
  *
  * @m@ - the @m@-computations that may be performed within @PropertyM@
  *
  *     Elements of @PropertyM m a@ may mix property operations and @m@-computations.
  */
case class PropertyM[M[_], A](unPropertyM: (A => Gen[M[Prop]]) => Gen[M[Prop]])(using
    monad: Monad[M]
) {
    def bind[B](f: A => PropertyM[M, B]): PropertyM[M, B] =
        PropertyM(k => this.unPropertyM(a => f(a).unPropertyM(k)))

    def map[B](f: A => B): PropertyM[M, B] =
        PropertyM(unPropertyM = (k: B => Gen[M[Prop]]) => this.unPropertyM((a: A) => k(f(a))))

}

// ===================================
// From here on out, its other haskell stuff that we don't currently need. I'm keeping it around for now because
// I've only barely glanced at it; maybe there's some other stuff in scalacheck/cats that corresponds to this
// and might be useful
// ===================================

//#ifndef NO_MONADFAIL
//instance Monad m => Fail.MonadFail (PropertyM m) where
//  fail = fail_
//#endif
//
//#ifndef NO_TRANSFORMERS
//instance MonadTrans PropertyM where
//  lift = run
//
//instance MonadIO m => MonadIO (PropertyM m) where
//  liftIO = run . liftIO
//#endif

//#ifndef NO_ST_MONAD
//-- | Runs the property monad for 'ST'-computations.
//--
//-- @
//-- -- Your mutable sorting algorithm here
//-- sortST :: Ord a => [a] -> 'Control.Monad.ST.ST' s (MVector s a)
//-- sortST = 'Data.Vector.thaw' . 'Data.Vector.fromList' . 'Data.List.sort'
//--
//-- prop_sortST xs = monadicST $ do
//--   sorted  \<- run ('Data.Vector.freeze' =<< sortST xs)
//--   assert ('Data.Vector.toList' sorted == sort xs)
//-- @
//--
//-- >>> quickCheck prop_sortST
//-- +++ OK, passed 100 tests.
//--
//monadicST :: Testable a => (forall s. PropertyM (ST s) a) -> Property
//monadicST m = property (runSTGen (monadic' m))
//
//runSTGen :: (forall s. Gen (ST s a)) -> Gen a
//runSTGen f = do
//  Capture eval <- capture
//  return (runST (eval f))
//#endif
//
//-- Exceptions
//
//
//#ifndef NO_EXCEPTIONS
//
//-- | Evaluate the value to Weak Head Normal Form (WHNF) and fail if it does not result in
//-- an expected exception being thrown.
//assertException ::
//     E.Exception exc
//  => (exc -> Bool) -- ^ Return `True` if that is the exception that was expected
//  -> a -- ^ Value that should result in an exception, when evaluated to WHNF
//  -> Property
//assertException isExc value = assertExceptionIO isExc (return value)
//
//
//-- | Make sure that a specific exception is thrown during an IO action. The result is
//-- evaluated to WHNF.
//assertExceptionIO ::
//     E.Exception exc
//  => (exc -> Bool) -- ^ Return `True` if that is the exception that was expected
//  -> IO a -- ^ An action that should throw the expected exception
//  -> Property
//assertExceptionIO isExc action =
//  monadicIO $ do
//    hasFailed <-
//      run
//        (E.catch
//           (do res <- action
//               res `seq` return False)
//           (return . isExc))
//    assert hasFailed
//
//#ifndef NO_DEEPSEQ
//
//-- | Same as `assertException`, but evaluate the value to Normal Form (NF) and fail if it
//-- does not result in an expected exception being thrown.
//assertDeepException ::
//     (NFData a, E.Exception exc)
//  => (exc -> Bool) -- ^ Return True if that is the exception that was expected
//  -> a -- ^ Value that should result in an exception, when fully evaluated to NF
//  -> Property
//assertDeepException isExc value = assertException isExc (rnf value)
//
//-- | Make sure that a specific exception is thrown during an IO action. The result is
//-- evaluated to NF.
//assertDeepExceptionIO ::
//     (NFData a, E.Exception exc)
//  => (exc -> Bool) -- ^ Return True if that is the exception that was expected
//  -> IO a -- ^ An action that should throw the expected exception
//  -> Property
//assertDeepExceptionIO isExc action = assertExceptionIO isExc (fmap rnf action)
//
//#endif
//#endif
//
