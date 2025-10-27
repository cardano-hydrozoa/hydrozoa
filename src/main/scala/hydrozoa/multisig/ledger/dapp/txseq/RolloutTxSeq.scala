package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.{Kleisli, NonEmptyVector}
import cats.syntax.all.*
import hydrozoa.multisig.ledger.dapp.tx.RolloutTx.Builder as SingleBuilder
import hydrozoa.multisig.ledger.dapp.tx.RolloutTx.Builder.PartialResult as SinglePartialResult
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.BuildErrorOr
import hydrozoa.multisig.ledger.dapp.tx.{RolloutTx, Tx}
import hydrozoa.multisig.ledger.dapp.txseq.RolloutTxSeq.Builder.PartialResult.Many
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import scala.annotation.tailrec
import scalus.cardano.txbuilder.SomeBuildError

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
        sealed trait PartialResult:
            type FirstTxType <: RolloutTx
            def firstOrOnly: SinglePartialResult.First[FirstTxType]

            def skipFirst: Option[PartialResult.SkipFirst]

            def finishPostProcess(rolloutSpent: RolloutUtxo): BuildErrorOr[RolloutTxSeq]

        object PartialResult {

            /** A case class indicating when only single rollout transaction is required to fulfill
              * all payout obligations. The transaction consumes the first rollout utxo (produced by
              * the settlement or finalization transaction) and directly pays out all remaining
              * payouts.
              */
            final case class Singleton(only: SinglePartialResult.First[RolloutTx.Last])
                extends PartialResult {

                override type FirstTxType = RolloutTx.Last
                override def firstOrOnly: SinglePartialResult.First[FirstTxType] = only

                override def skipFirst: Option[PartialResult.SkipFirst] = SkipFirst(this)

                /** Finish the singleton rollout transaction by providing the first rollout utxo. */
                override def finishPostProcess(
                    rolloutSpent: RolloutUtxo
                ): BuildErrorOr[RolloutTxSeq] =
                    for {
                        onlyCompleted <- only.complete(rolloutSpent)
                    } yield RolloutTxSeq(notLast = Vector.empty, last = onlyCompleted)
            }

            /** A partial result for a rollout transaction _chain_, where the [[first]] transaction
              * consumes a rollout utxo from a settlement or finalization tx and produces the next
              * rollout utxo, the [[intermediates]] consume the previous and produce another, and
              * the [[last]] consumes a final rollout utxo and fulfills all payout obligations
              * (therefore not needing to produce another rollout utxo).
              */
            final case class Many(
                first: SinglePartialResult.First[RolloutTx.NotLast],
                intermediates: Vector[SinglePartialResult.NotFirst[RolloutTx.NotLast]],
                last: SinglePartialResult.NotFirst[RolloutTx.Last]
            ) extends PartialResult {
                override type FirstTxType = RolloutTx.NotLast
                override def firstOrOnly: SinglePartialResult.First[FirstTxType] = first

                override def skipFirst: Option[PartialResult.SkipFirst] = SkipFirst(this)

                /** Finalize the partial result chain into a sequence of rollout transactions by
                  * providing the first rollout utxo to the first partial result, finishing it, and
                  * then threading the subsequent rollout utxos through the remainder of the
                  * sequence.
                  */
                override def finishPostProcess(
                    rolloutSpent: RolloutUtxo
                ): BuildErrorOr[RolloutTxSeq] =
                    import Many.*
                    for {
                        firstPostProcessed <- first.complete(rolloutSpent)

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

                        lastPostProcessed <- last.complete(lastRolloutSpent)

                    } yield RolloutTxSeq(
                      notLast = firstPostProcessed +: intermediatesPostProcessed,
                      last = lastPostProcessed
                    )
            }

            object Many {

                /** I.E.: RolloutUtxo => Either[SomeBuildError, RolloutTx.NotLast] */
                type IntermediateRolloutKleisli =
                    Kleisli[BuildErrorOr, RolloutUtxo, RolloutTx.NotLast]

                def finishPostProcessIntermediate(
                    current: SinglePartialResult.NotFirst[RolloutTx.NotLast]
                )(
                    rolloutSpent: RolloutUtxo
                ): BuildErrorOr[RolloutTx.NotLast] =
                    for {
                        currentPostProcessed <- current.complete(rolloutSpent)
                    } yield currentPostProcessed

                def kleisliRunner(
                    eCurrent: BuildErrorOr[RolloutTx.NotLast],
                    k: IntermediateRolloutKleisli
                ): (BuildErrorOr[RolloutTx.NotLast], BuildErrorOr[RolloutTx.NotLast]) = {
                    val res = for {
                        current <- eCurrent
                        next <- k.run(current.rolloutProduced)
                    } yield next
                    (res, res)
                }
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
                            case _Empty => Some(Singleton(only = last.asFirst))
                        }
                }

                private given Conversion[SkipFirst, PartialResult] = identity

                extension (self: SkipFirst) def partialResult: PartialResult = self.convert
            }
        }

        final case class State(
            notLast: Vector[SinglePartialResult.NotFirst[RolloutTx.NotLast]],
            last: SinglePartialResult.NotFirst[RolloutTx.Last]
        )
    }

    final case class Builder(
        config: Tx.Builder.Config
    ) {

        import Builder.*

        lazy val singleBuilderLast = SingleBuilder.Last(config)
        lazy val singleBuilderNotLast = SingleBuilder.NotLast(config)

        /** Builds a "partial result chain" of rollout transactions. This can either be a
          * [[Singleton]] chain or a [[Many]] chain. In the case of the latter, it tries to pack as
          * many payout obligations into each rollout transaction, proceeding from the last and
          * working towards the first.
          * @return
          */
        def buildPartial(
            payouts: NonEmptyVector[Payout.Obligation.L1]
        ): BuildErrorOr[PartialResult] =
            for {
                lastRolloutTx <- singleBuilderLast.partialResult(SingleBuilder.Args.Last(payouts))
                partialResult <- lastRolloutTx match {
                    case only: SinglePartialResult.First[RolloutTx.Last] =>
                        Right(PartialResult.Singleton(only))
                    case last: SinglePartialResult.NotFirst[RolloutTx.Last] =>
                        val initialState = State(last = last, notLast = Vector.empty)
                        for {
                            current <- singleBuilderNotLast.partialResult(
                              SingleBuilder.Args.NotLast(
                                last.payoutObligationsRemaining,
                                last.inputValueNeeded
                              )
                            )
                            loopResult <- loop(current, initialState)
                        } yield loopResult
                }
            } yield partialResult

        @tailrec
        private def loop(
            current: SinglePartialResult[RolloutTx.NotLast],
            acc: State
        ): BuildErrorOr[PartialResult] =
            current match {
                case intermediate: SinglePartialResult.NotFirst[RolloutTx.NotLast] =>
                    val newAcc = acc.copy(notLast = intermediate +: acc.notLast)
                    val eNewCurrent = singleBuilderNotLast.partialResult(
                      SingleBuilder.Args.NotLast(
                        intermediate.payoutObligationsRemaining,
                        intermediate.inputValueNeeded
                      )
                    )
                    eNewCurrent match {
                        case Right(newCurrent) => loop(newCurrent, newAcc)
                        case Left(err)         => Left(err)
                    }
                case first: SinglePartialResult.First[RolloutTx.NotLast] =>
                    Right(Many(first = first, intermediates = acc.notLast, last = acc.last))
            }
    }
}
