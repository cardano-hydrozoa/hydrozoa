package test

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import org.scalacheck.PropertyM.{monadForPropM, monadicIO}
import org.scalacheck.{Gen, Prop, PropertyM}

private type PT[A] = PropertyM[IO, A]
private type RT[R, A] = ReaderT[PT, R, A]

/** Describes a computation that:
  *   - Has access to some test environment
  *   - Accepts continuations (within [[PropertyM]])
  *   - Can perform [[IO]]
  *   - Can generate values (via the [[Gen]] within [[PropertyM]]
  *   - Returns values of type A
  */
// See JointLedgerTest for an example of how this is intended to be used
case class TestM[R, A](unTestM: RT[R, A]) {
    def map[B](f: A => B): TestM[R, B] = TestM(this.unTestM.map(f))
    def flatMap[B](f: A => TestM[R, B]): TestM[R, B] = TestM(
      this.unTestM.flatMap(a => f(a).unTestM)
    )
}

given testMMonad[R]: Monad[[A] =>> TestM[R, A]] = new Monad[[A] =>> TestM[R, A]] {
    def pure[A](a: A): TestM[R, A] = TestM(Kleisli.pure(a))
    def flatMap[A, B](fa: TestM[R, A])(f: A => TestM[R, B]): TestM[R, B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => TestM[R, Either[A, B]]): TestM[R, B] = ???
}

object TestM {

    /** Get the instantiated TestR test environment
      */
    def ask[R]: TestM[R, R] = TestM(Kleisli.ask)

    def asks[R, A](f: R => A): TestM[R, A] =
        for {
            env <- ask
        } yield f(env)

    def pick[R, A](gen: Gen[A]): TestM[R, A] = TestM(Kleisli.liftF(PropertyM.pick(gen)))

    def pure[R, A](a: A): TestM[R, A] = TestM(Kleisli.pure(a))

    def fail[R, A](msg: String): TestM[R, A] = TestM(Kleisli.liftF(PropertyM.fail_(msg)))

    def assertWith[R](condition: Boolean, msg: String): TestM[R, Unit] =
        TestM(Kleisli.liftF(PropertyM.assertWith(condition, msg)))

    def assert[R](condition: Boolean): TestM[R, Unit] = TestM(
      Kleisli.liftF(PropertyM.assert(condition))
    )

    /** Given a computation of type [[TestM]] that returns a value that can be implicitly turned
      * into a [[Prop]], run the computation.
      * @param testM
      *   The computation to run
      * @param initializer
      *   the computation that generates and sets up the [[R]] environment passed to [[TestM]].
      *   Defaults to a (sensibly) randomly generated environment.
      * @param toProp
      *   The implicit function that transforms the result of the computation into a [[Prop]]
      * @param ioRuntime
      *   The implicit IO runtime in which [[IO]] effects can be executed
      * @return
      */
    def run[R, A](testM: TestM[R, A], initializer: PT[R])(using
        toProp: A => Prop,
        ioRuntime: IORuntime
    ): Prop = {

        monadicIO(
          // This runs the initialization within the `PropertyM` first, in order to give the computation in `TestM`
          // access to the fully-initialized environment
          for {
              env <- initializer
              res <- testM.unTestM.run(env)
          } yield res
        )
    }

    // ===================================
    // Lifts
    // ===================================

    def lift[R, A](e: IO[A]): TestM[R, A] =
        TestM(Kleisli.liftF(PropertyM.run(e)))

    def lift[R, A](propertyM: PropertyM[IO, A]): TestM[R, A] = TestM(Kleisli.liftF(propertyM))

    def failLeft[R, E, A](e: Either[E, A]): TestM[R, A] = e match {
        case Left(e)  => fail(e.toString)
        case Right(x) => pure(x)
    }
}

/** NOTE: This is not quite:
  *   https://typelevel.org/cats/guidelines.html#partially-applied-type
  * The idea there is for typeclasses where you want to be able to pass directly _only_ the non-inferrable type(s).
  * This is _sort of_ like that, but the intention is to instantiate a Value Class with the type parameter fixed, so
  * you don't have to pass it every time. I.e.:
  *
  *  | this | cats "partially applied type" | Raw TestM  |
  *  -----------------------------------------------------
  *  | pick |          pick[R]              | pick[R, A] |
  *
  *  The way to use it is:
  *
  *  ```
  *  val myTest = TestMFixedEnv[MyTestEnvType]
  *  import myTest.*
  *  ```
  */
// This might be able to be done better with the partially-applied-type-parameter pattern, but I'm not sure.
// This does what I want for now
final class TestMFixedEnv[R](dummy: Boolean = true) {

    def ask: TestM[R, R] = TestM.ask

    def asks[A](f: R => A): TestM[R, A] = TestM.asks[R, A](f)

    def pick[A](gen: Gen[A]): TestM[R, A] = TestM.pick[R, A](gen)

    def pure[A](a: A): TestM[R, A] = TestM.pure[R, A](a)

    def fail[A](msg: String): TestM[R, A] = TestM.fail[R, A](msg)

    def assertWith(condition: Boolean, msg: => String): TestM[R, Unit] =
        TestM.assertWith[R](condition, msg)

    def assert(condition: Boolean): TestM[R, Unit] = TestM.assert(condition)

    def run[A](testM: TestM[R, A], initializer: PT[R])(using
        toProp: A => Prop,
        ioRuntime: IORuntime
    ): Prop = TestM.run[R, A](testM, initializer)

    def lift[A](e: IO[A]): TestM[R, A] = TestM.lift(e)

    def lift[A](propertyM: PropertyM[IO, A]): TestM[R, A] = TestM.lift(propertyM)

    def failLeft[E, A](e: Either[E, A]): TestM[R, A] = TestM.failLeft(e)
}
