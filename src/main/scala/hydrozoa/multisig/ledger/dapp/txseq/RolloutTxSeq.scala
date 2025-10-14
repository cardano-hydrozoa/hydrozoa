package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.multisig.ledger.dapp.txseq.RolloutTxSeq.Builder.PartialResult.Many
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder.PartialResult as SinglePartialResult
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder as SingleBuilder
import hydrozoa.multisig.ledger.joint.utxo.Payout
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo

import scala.annotation.tailrec
import cats.syntax.all.*
import cats.data.{Kleisli, NonEmptyVector}

/**
 * A non-empty chain of rollout transactions in order of chaining.
 * The first Tx [[notLast]] vector consumes a rollout utxo produced by a settling or finalizing transaction;
 * the second consumes the rollout utxo produced by the first, and so on.
 * @param notLast
 * @param last
 */
final case class RolloutTxSeq(
    notLast: Vector[RolloutTx.NotLast],
    last: RolloutTx.Last
)

object RolloutTxSeq {
    object Builder {
        type Error = RolloutTx.Builder.Error
        type ErrorOr[A] = Either[Error, A]

        type Result = RolloutTxSeq

        sealed trait PartialResult

        object PartialResult {
            /**
             * A case class indicating when only single rollout transaction is required to 
             * fulfill all payout obligations. The transaction consumes the first rollout utxo
             * (produced by the settlement or finalization transaction) and directly pays out
             * all remaining payouts.
             * @param only
             */
            final case class Singleton(only: SinglePartialResult.Only) extends PartialResult {
                /** Finish the singleton rollout transaction by providing the first rollout utxo. */
                def finishPostProcess(
                    rolloutSpent: RolloutUtxo
                ): ErrorOr[RolloutTxSeq] =
                    for {
                        onlyFinished <- only.builder.Finish.finish(only.state, rolloutSpent.utxo)
                        onlyPostProcessed = only.builder.PostProcess.Last
                            .getRolloutTx(onlyFinished, only.builder)
                    } yield RolloutTxSeq(notLast = Vector.empty, last = onlyPostProcessed)
            }

            /** I.E.: RolloutUtxo => Either[Error, RolloutTx.NotLast] */
            type RolloutKleisli =
                Kleisli[ErrorOr, RolloutUtxo, RolloutTx.NotLast]

            /** A partial result for a rollout transaction _chain_, where the [[first]] transaction consumes a rollout
             * utxo from a settlement or finalization tx and produces the next rollout utxo, the [[intermediates]] 
             * consume the previous and produce another, and the [[last]] consumes a final rollout utxo and fulfills
             * all payout obligations (therefore not needing to produce another rollout utxo). */
            final case class Many(
                first: SinglePartialResult.First,
                intermediates: Vector[SinglePartialResult.Intermediate],
                last: SinglePartialResult.Last
            ) extends PartialResult {
                
                /** Finalize the partial result chain into a sequence of rollout transactions by providing
                 * the first rollout utxo to the first partial result, finishing it, and then threading the subsequent
                 * rollout utxos through the remainder of the sequence. 
                 * */
                def finishPostProcess(
                    rolloutSpent: RolloutUtxo
                ): ErrorOr[RolloutTxSeq] =
                    for {
                        firstFinished <- first.builder.Finish.finish(first.state, rolloutSpent.utxo)
                        firstPostProcessed = first.builder.PostProcess.NotLast
                            .getRolloutTx(firstFinished, first.builder)

                        vectorKleisli: Vector[RolloutKleisli] = intermediates
                            .map(finishPostProcessIntermediate)
                            .map(Kleisli(_))

                        intermediatesPostProcessed <- vectorKleisli
                            .mapAccumulate(Right(firstPostProcessed))(kleisliRunner)
                            ._2
                            .sequence

                        lastRolloutSpent = intermediatesPostProcessed.lastOption
                            .getOrElse(firstPostProcessed)
                            .rolloutProduced

                        lastFinished <- last.builder.Finish
                            .finish(last.state, lastRolloutSpent.utxo)
                        lastPostProcessed = last.builder.PostProcess.Last
                            .getRolloutTx(lastFinished, last.builder)

                    } yield RolloutTxSeq(
                      notLast = firstPostProcessed +: intermediatesPostProcessed,
                      last = lastPostProcessed
                    )

                private def finishPostProcessIntermediate(
                    current: SinglePartialResult.Intermediate
                )(
                    rolloutSpent: RolloutUtxo
                ): ErrorOr[RolloutTx.NotLast] =
                    for {
                        currentFinished <- current.builder.Finish
                            .finish(current.state, rolloutSpent.utxo)
                        currentPostProcessed = current.builder.PostProcess.NotLast
                            .getRolloutTx(currentFinished, current.builder)
                    } yield currentPostProcessed

                private def kleisliRunner(
                    eCurrent: ErrorOr[RolloutTx.NotLast],
                    k: RolloutKleisli
                ): (ErrorOr[RolloutTx.NotLast], ErrorOr[RolloutTx.NotLast]) = {
                    val res = for {
                        current <- eCurrent
                        next <- k.run(current.rolloutProduced)
                    } yield next
                    (res, res)
                }
            }
        }

        final case class State(
            notLast: Vector[SinglePartialResult.Intermediate],
            last: SinglePartialResult.Last
        )
    }

    final case class Builder(
        config: RolloutTx.Builder.Config,
        payouts: NonEmptyVector[Payout.Obligation.L1]
    ) {

        import Builder.*

        /**
         * Builds a "partial result chain" pertaining to rollout transactions. This can either be a [[Singleton]] chain
         * or a [[Many]] chain. In the case of the latter, it tries to pack as many payout obligations into 
         * each rollout transaction, proceeding from the last and working towards the first.
         * @return
         */
        def buildPartial(): ErrorOr[PartialResult] =
            for {
                lastRolloutTx <- SingleBuilder.Last(config, payouts.toVector).buildPartial()
                partialResult <- lastRolloutTx match {
                    case only: SinglePartialResult.Only =>
                        Right(PartialResult.Singleton(only))
                    case last: SinglePartialResult.Last =>
                        val initialState = State(last = last, notLast = Vector.empty)
                        for {
                            current <- SingleBuilder
                                .NotLast(
                                  config,
                                  lastRolloutTx.state.remainingPayoutObligations,
                                  lastRolloutTx.state.inputValueNeeded
                                )
                                .buildPartial()
                            loopResult <- loop(current, initialState)
                        } yield loopResult
                }
            } yield partialResult

        @tailrec
        def loop(current: SinglePartialResult.NotLast, acc: State): ErrorOr[PartialResult] =
            current match {
                case intermediate: SinglePartialResult.Intermediate =>
                    val newAcc = acc.copy(notLast = intermediate +: acc.notLast)
                    val eNewCurrent = SingleBuilder
                        .NotLast(
                          config,
                          intermediate.state.remainingPayoutObligations,
                          intermediate.state.inputValueNeeded
                        )
                        .buildPartial()
                    eNewCurrent match {
                        case Right(newCurrent) => loop(newCurrent, newAcc)
                        case Left(err)         => Left(err)
                    }
                case first: SinglePartialResult.First =>
                    Right(Many(first = first, intermediates = acc.notLast, last = acc.last))
            }
    }
}
