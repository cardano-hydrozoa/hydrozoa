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
            final case class Singleton(only: SinglePartialResult.Only) extends PartialResult {
                def finishPostProcess(
                    rolloutSpent: RolloutUtxo
                ): ErrorOr[RolloutTxSeq] =
                    for {
                        onlyFinished <- only.builder.Finish.finish(only.state, rolloutSpent.utxo)
                        onlyPostProcessed = only.builder.PostProcess.Last
                            .getRolloutTx(onlyFinished, only.builder)
                    } yield RolloutTxSeq(notLast = Vector.empty, last = onlyPostProcessed)
            }

            type RolloutKleisli =
                Kleisli[ErrorOr, RolloutUtxo, RolloutTx.NotLast]

            final case class Many(
                first: SinglePartialResult.First,
                intermediates: Vector[SinglePartialResult.Intermediate],
                last: SinglePartialResult.Last
            ) extends PartialResult {
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
