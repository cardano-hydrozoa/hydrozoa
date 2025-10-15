package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.{Kleisli, NonEmptyVector}
import cats.syntax.all.*
import hydrozoa.multisig.ledger.dapp.txseq.RolloutTxSeq.Builder.PartialResult.Many
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder as SingleBuilder
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder.PartialResult as SinglePartialResult
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder.PartialResult.FirstOrOnly
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout

import scala.annotation.tailrec

/** A non-empty chain of rollout transactions in order of chaining.
  *
  * @param notLast
  *   A vector of [[RolloutTx]] that are chained together. The first of these rollout txs consumes a
  *   [[RolloutUtxo]] produced by a settling or finalizing transaction, the second consumes the
  *   rollout utxo produced by the first, and so on.
  * @param last
  *   A [[RolloutTx]] that consumes the last [[RolloutUtxo]] produced by [[notLast]] and produces no
  *   rollout utxo of its own.
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

        sealed trait PartialResult:
            def firstOrOnly: RolloutTx.Builder.PartialResult.FirstOrOnly

            def skipFirst: Option[PartialResult.SkipFirst]

            def finishPostProcess(rolloutSpent: RolloutUtxo): ErrorOr[RolloutTxSeq]

        object PartialResult {

            /** A case class indicating when only single rollout transaction is required to fulfill
              * all payout obligations. The transaction consumes the first rollout utxo (produced by
              * the settlement or finalization transaction) and directly pays out all remaining
              * payouts.
              */
            final case class Singleton(only: SinglePartialResult.Only) extends PartialResult {

                val firstOrOnly: FirstOrOnly = only

                def skipFirst: Option[PartialResult.SkipFirst] = SkipFirst(this)

                /** Finish the singleton rollout transaction by providing the first rollout utxo. */
                override def finishPostProcess(rolloutSpent: RolloutUtxo): ErrorOr[Result] =
                    Finish.finishPostProcess(this, rolloutSpent)
            }

            /** A partial result for a rollout transaction _chain_, where the [[first]] transaction
              * consumes a rollout utxo from a settlement or finalization tx and produces the next
              * rollout utxo, the [[intermediates]] consume the previous and produce another, and
              * the [[last]] consumes a final rollout utxo and fulfills all payout obligations
              * (therefore not needing to produce another rollout utxo).
              */
            final case class Many(
                first: SinglePartialResult.First,
                intermediates: Vector[SinglePartialResult.Intermediate],
                last: SinglePartialResult.Last
            ) extends PartialResult {
                val firstOrOnly: FirstOrOnly = first

                def skipFirst: Option[PartialResult.SkipFirst] = SkipFirst(this)

                /** Finalize the partial result chain into a sequence of rollout transactions by
                  * providing the first rollout utxo to the first partial result, finishing it, and
                  * then threading the subsequent rollout utxos through the remainder of the
                  * sequence.
                  */
                override def finishPostProcess(rolloutSpent: RolloutUtxo): ErrorOr[Result] =
                    Finish.finishPostProcess(this, rolloutSpent)
            }

            /** A newtype wrapper around a [[PartialResult]] that has had its first
              * [[RolloutTx.Builder.PartialResult]] removed and the rest rotated accordingly.
              *
              * A [[SkipFirst]] cannot be constructed for a [[PartialResult.Singleton]] because no
              * other rollouts remain in the sequence after removing the first (and only) one.
              */
            type SkipFirst = SkipFirst.SkipFirst

            object SkipFirst {
                opaque type SkipFirst = PartialResult

                def apply(pr: PartialResult): Option[SkipFirst] = pr match {
                    case singleton: PartialResult.Singleton => None
                    case many: PartialResult.Many =>
                        import many.*
                        intermediates match {
                            case firstIntermediate +: rest =>
                                val newFirst = firstIntermediate.asFirst
                                Some(
                                  PartialResult.Many(
                                    first = newFirst,
                                    intermediates = rest,
                                    last = last
                                  )
                                )
                            case _Empty => Some(Singleton(only = last.asOnly))
                        }
                }

                private given Conversion[SkipFirst, PartialResult] = identity

                extension (self: SkipFirst) def partialResult: PartialResult = self.convert
            }
        }

        final case class State(
            notLast: Vector[SinglePartialResult.Intermediate],
            last: SinglePartialResult.Last
        )

        private object Finish {
            def finishPostProcess(
                partialResult: PartialResult,
                rolloutSpent: RolloutUtxo
            ): ErrorOr[RolloutTxSeq] = partialResult match {

                case singleton: PartialResult.Singleton =>
                    import singleton.*
                    for {
                        onlyFinished <- only.builder.finish(only.state, rolloutSpent.utxo)
                        onlyPostProcessed = only.builder.getRolloutTx(onlyFinished)
                    } yield RolloutTxSeq(notLast = Vector.empty, last = onlyPostProcessed)

                case many: PartialResult.Many =>
                    import many.*
                    for {
                        firstFinished <- first.builder.finish(first.state, rolloutSpent.utxo)
                        firstPostProcessed = first.builder.getRolloutTx(firstFinished)

                        vectorKleisli: Vector[IntermediateRolloutKleisli] = intermediates
                            .map(finishPostProcessIntermediate)
                            .map(Kleisli(_))

                        intermediatesPostProcessed <- vectorKleisli
                            .mapAccumulate(Right(firstPostProcessed))(kleisliRunner)
                            ._2
                            .sequence

                        lastRolloutSpent = intermediatesPostProcessed.lastOption
                            .getOrElse(firstPostProcessed)
                            .rolloutProduced

                        lastFinished <- last.builder.finish(last.state, lastRolloutSpent.utxo)
                        lastPostProcessed = last.builder.getRolloutTx(lastFinished)

                    } yield RolloutTxSeq(
                      notLast = firstPostProcessed +: intermediatesPostProcessed,
                      last = lastPostProcessed
                    )

            }

            /** I.E.: RolloutUtxo => Either[Error, RolloutTx.NotLast] */
            type IntermediateRolloutKleisli =
                Kleisli[ErrorOr, RolloutUtxo, RolloutTx.NotLast]

            private def finishPostProcessIntermediate(
                current: SinglePartialResult.Intermediate
            )(
                rolloutSpent: RolloutUtxo
            ): ErrorOr[RolloutTx.NotLast] =
                for {
                    currentFinished <- current.builder.finish(current.state, rolloutSpent.utxo)
                    currentPostProcessed = current.builder.getRolloutTx(currentFinished)
                } yield currentPostProcessed

            private def kleisliRunner(
                eCurrent: ErrorOr[RolloutTx.NotLast],
                k: IntermediateRolloutKleisli
            ): (ErrorOr[RolloutTx.NotLast], ErrorOr[RolloutTx.NotLast]) = {
                val res = for {
                    current <- eCurrent
                    next <- k.run(current.rolloutProduced)
                } yield next
                (res, res)
            }
        }
    }

    final case class Builder(
        config: RolloutTx.Builder.Config,
        payouts: NonEmptyVector[Payout.Obligation.L1]
    ) {

        import Builder.*

        /** Builds a "partial result chain" of rollout transactions. This can either be a
          * [[Singleton]] chain or a [[Many]] chain. In the case of the latter, it tries to pack as
          * many payout obligations into each rollout transaction, proceeding from the last and
          * working towards the first.
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
