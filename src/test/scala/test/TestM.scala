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
  *   - Has access to some [[TestR]] environment
  *   - Accepts continuations (within [[PropertyM]])
  *   - Can perform [[IO]]
  *   - Can generate values (via the [[Gen]] within [[PropertyM]]
  *   - Returns values of type A
  * @param unTestM
  * @tparam A
  */
// See JointLedgerTest for an example of how this is intended to be used
case class TestM[R, A](unTestM: RT[R, A]) {
    def map[B](f: A => B): TestM[R, B] = TestM(this.unTestM.map(f))
    def flatMap[B](f: A => TestM[R, B]): TestM[R, B] = TestM(
      this.unTestM.flatMap(a => f(a).unTestM)
    )
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

    /** Given a computation of type [[TestM]] that returns a value that can be implicitly turned
      * into a [[Prop]], run the computation.
      * @param testM
      *   The computation to run
      * @param initializer
      *   the computation that generates and sets up the [[TestR]] environment passed to [[testM]].
      *   Defaults to a (sensibly) randomly generated environment.
      * @param toProp
      *   The implicit function that transforms the result of the computation into a [[Prop]]
      * @param ioRuntime
      *   The implicit IO runtime in which [[IO]] effects can be executed
      * @tparam A
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

}
