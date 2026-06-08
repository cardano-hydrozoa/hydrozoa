package test

import cats.*
import cats.data.*
import scalus.cardano.ledger.rules.CardanoMutator.{Context, Event, Result, State}
import scalus.cardano.ledger.rules.{CardanoMutator, DefaultMutators, DefaultValidators, OutsideValidityIntervalValidator, STS}

private type EitherThatOr[Error] = [X] =>> Either[Error, X]

object TransactionChain {
    import hydrozoa.lib.cats.data.Kendo.*
    import scalus.cardano.ledger.*
    import scalus.cardano.ledger.rules.{Context, State}

    /** Runs a list of transactions in sequence, returning the final state or the first error. */
    def foldTxChain(
        txs: Seq[Transaction]
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
        txs: Seq[Transaction]
    )(
        initialState: State,
        mutator: STS.Mutator = CardanoMutator,
        context: Context = Context()
    ): Either[TransactionException, Seq[(State, Transaction)]] = {
        def liftTx(
            tx: Transaction
        ): Kendo[EitherThatOr[TransactionException], (State, Transaction)] =
            Kleisli(previousStateAndTx =>
                mutator
                    .transit(context = context, state = previousStateAndTx._1, event = tx) match {
                    case Left(e)         => Left(e)
                    case Right(newState) => Right(newState, tx)
                }
            )

        txs match {
            case Nil               => Right(Seq.empty)
            case headTx +: tailTxs => kendoScan(tailTxs.map(liftTx)).run((initialState, headTx))
        }
    }

    /** Observe a chain of transactions. If an error is encountered, it will return a transaction
      * Exception along with a sequence of intermediate states and the transactions that produced
      * them. If successful, all intermediate states and transactions will be returned.
      */
    def observeTxChain(txs: Seq[Transaction])(
        initialState: State,
        mutator: STS.Mutator = CardanoMutator,
        context: Context = Context.testMainnet()
    ): Either[
      (TransactionException, Vector[(State, Transaction)]),
      Vector[(State, Transaction)]
    ] = {
        def liftTx(
            tx: Transaction
        ): Kendo[EitherThatOr[TransactionException], (State, Transaction)] =
            Kleisli(previousStateAndTx =>
                mutator
                    .transit(context = context, state = previousStateAndTx._1, event = tx) match {
                    case Left(e)         => Left(e)
                    case Right(newState) => Right(newState, tx)
                }
            )
        txs match {
            // No Txs ==> No results
            case Nil => Right(Vector.empty)
            // Head tx: apply it to the initial state
            case headTx +: tailTxs =>
                for {
                    state2 <- mutator
                        .transit(context, initialState, headTx)
                        .left
                        .map((_, Vector.empty))
                    res <- kendoObserve(tailTxs.map(liftTx)).run((state2, headTx))
                } yield res

        }
    }

    object ObserverMutator extends STS.Mutator {
        override final type Error = TransactionException

        override def transit(context: Context, state: State, event: Event): Result = {
            STS.Mutator.transit[Error](
              DefaultValidators.all
                  .filterNot(_.isInstanceOf[OutsideValidityIntervalValidator.type]),
              DefaultMutators.all,
              context,
              state,
              event
            )
        }

    }

}
