package test

import cats.*
import cats.data.*
import cats.syntax.all.*
import scalus.cardano.ledger.rules.{CardanoMutator, STS}

import scala.annotation.tailrec

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

    /** If the monad underlying your Kendo can throw an error, you might want to observe the partial
      * results. This function is like [[kendoScan]], but will return an error alongside the partial
      * results if it is not successful.
      *
      * @param kendos
      *   A list of Kleislis parameterized over a monad F that can throw errors of type E and return
      *   values of type A, such as Either[E,A]
      * @param monadF
      * @param appEF
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
        appE: ApplicativeError[[X] =>> F[(E, Seq[A]), X], (E, Seq[A])]
    ): Kleisli[[X] =>> F[(E, Seq[A]), X], A, Seq[A]] = {
        // TODO: Figure out if this should be a list, vector, queue, or just a Seq

        @tailrec
        def loop(
            remainingKendos: Seq[Kendo[[X] =>> F[E, X], A]],
            partialResults: Seq[A]
        ): F[(E, Seq[A]), Seq[A]] =
            remainingKendos match {
                case Nil => appE.pure(partialResults)
                case nextKendo +: futureKendos => {
                    val lastResult = partialResults.last
                    nextKendo.run(lastResult) match {
                        case e: E => appE.raiseError((e, partialResults))
                        case a: A => loop(futureKendos, partialResults.appended(a))
                    }
                }
            }

        kendos match {
            case Nil => Kleisli(_ => appE.pure(Seq.empty[A]))
            case headKendo +: tailKendo =>
                Kleisli(a => {
                    headKendo.run(a) match {
                        case Left(headError: E) => appE.raiseError((headError, Seq.empty))
                        case Right(firstRes: A) => loop(tailKendo, partialResults = Seq(firstRes))
                    }
                })
        }
    }
}

private type EitherThatOr[Error] = [X] =>> Either[Error, X]

object TransactionChain {
    import Kendo.*
    import scalus.cardano.ledger.*
    import scalus.cardano.ledger.rules.{Context, State}

    /** Runs a list of transactions in sequence, returning the final state or the first error. */
    def foldTxChain(
        txs: List[Transaction]
    )(
        initialState: State,
        mutator: STS.Mutator = CardanoMutator,
        context: Context = Context()
    ): Either[TransactionException, State] = {
        def liftTx(tx: Transaction): Kendo[EitherThatOr[TransactionException], State] = {
            Kleisli(state => mutator.transit(context = context, state = state, event = tx))
        }
        kendoFold(txs.map(liftTx)).run(initialState)
    }

    /** Runs a list of transaction in sequence, returning all intermediate states and the
      * transaction that produced them, or the first error.
      */
    def scanTxChain(
        txs: List[Transaction]
    )(
        initialState: State,
        mutator: STS.Mutator = CardanoMutator,
        context: Context = Context()
    ): Either[TransactionException, Seq[(State, Transaction)]] = {
        def liftTx(
            tx: Transaction
        ): Kendo[EitherThatOr[TransactionException], (State, Transaction)] =
            Kleisli(lastStateAndTx =>
                mutator
                    .transit(context = context, state = lastStateAndTx._1, event = tx) match {
                    case Left(e)         => Left(e)
                    case Right(newState) => Right(newState, tx)
                }
            )

        txs match {
            case Nil => Right(Seq.empty)
            case headTx +: tailTxs =>
                mutator.transit(
                  context = context,
                  state = initialState,
                  event = headTx
                ) match {
                    case Left(e)            => Left(e)
                    case Right(secondState) => kendoScan(txs.map(liftTx)).run((secondState, headTx))
                }

        }

    }

    /** Observe a chain of transactions. If an error is encountered, it will return a transaction
      * Exception along with a sequence of intermediate states and the transactions that produced
      * them. If successful, all intermediate states and transactions will be returned.
      */
    def observeTxChain(txs: Seq[Transaction])(
        initialState: State,
        mutator: STS.Mutator = CardanoMutator,
        context: Context = Context()
    ): Either[(TransactionException, Seq[(State, Transaction)]), Seq[(State, Transaction)]] = {
        def liftTx(
            tx: Transaction
        ): Kendo[EitherThatOr[TransactionException], (State, Transaction)] =
            Kleisli(lastStateAndTx =>
                mutator
                    .transit(context = context, state = lastStateAndTx._1, event = tx) match {
                    case Left(e)         => Left(e)
                    case Right(newState) => Right(newState, tx)
                }
            )
        txs match {
            case Nil => Right(Seq.empty)
            case headTx +: tailTxs =>
                mutator.transit(
                  context = context,
                  state = initialState,
                  event = headTx
                ) match {
                    case Left(e) => Left((e, Seq.empty))
                    case Right(secondState) =>
                        kendoObserve(txs.map(liftTx)).run((secondState, headTx))
                }
        }
    }
}
