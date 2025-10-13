package hydrozoa.multisig.ledger.dapp.txseq.txbak

import cats.*
import cats.data.*
import cats.implicits.toTraverseOps
import cats.syntax.all.*
import hydrozoa.lib.tx.ScriptSource.NativeScriptAttached
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder.State.Status.NeedsInput
import hydrozoa.multisig.ledger.dapp.txseq.txbak.RolloutTxSeq.Builder.IncoherentChainingError
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import hydrozoa.prebalancedDiffHandler
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.{Environment, TxBalancingError}

import scala.util.{Failure, Success, Try}

final case class RolloutTxSeq(
    rolloutSpent: RolloutUtxo,
    rolloutProduced: Option[RolloutUtxo],
    override val tx: Transaction
) extends Tx

object RolloutTxSeq {

    import RolloutTx.Builder.*
    import RolloutTx.Builder.State.*
    import hydrozoa.lib.tx.*
    import hydrozoa.lib.tx.BuildError.*
    import hydrozoa.lib.tx.TransactionBuilderStep.*

    final case class Builder(
        hnsReferenceOutput: TransactionUnspentOutput,
        headNativeScript: HeadMultisigScript,
        env: Environment,
        validators: Seq[Validator]
    ) {

        type RolloutErrorOr[A] =
            Either[RolloutTx.Builder.Error | IncoherentChainingError.type, A]

        // A NonEmptyList where the first element has a distinguished type
        type RolloutPartialResultChain =
            (PartialResult.NeedsInput.FirstOrOnly, List[PartialResult.NeedsInput.NotFirst])

        /** @return
          *   The settlement tx accompanied by a _chain_ of rollout transactions. TODO: make a
          *   newtype
          */
        def mkPartialChain(
            // TODO: make NonEmptyVector
            payouts: Vector[Payout.Obligation.L1]
        ): RolloutErrorOr[RolloutPartialResultChain] = {
            type RolloutMState = NonEmptyList[PartialResult.NeedsInput]
            type RolloutM[A] = StateT[RolloutErrorOr, RolloutMState, A]

            // This accumulates a chain of partial results (f1, f2, ..., fn) in the state.
            // We can subsequently use [[completeBuild]] with `f1` being fed the first rollout utxo
            // (from the settlement tx), and the chain goes on.
            // This is different from the order we build them -- we start with fn, then fn-1,
            // etc., prepending as we go.
            val mkPartialResults: RolloutM[Unit] = {
                // The state is a sequence that we _prepend_ partial res's to. The final sequence reflects the
                // correct ordering for the rollouts

                def pure0[A] =
                    StateT.pure[RolloutErrorOr, RolloutMState, A]

                def liftF0[A] =
                    StateT.liftF[RolloutErrorOr, RolloutMState, A]

                def modify0 = StateT.modify[RolloutErrorOr, RolloutMState]

                def get0 = StateT.get[RolloutErrorOr, RolloutMState]

                for {
                    state <- get0

                    // The partial result related to the NEXT rollout transaction
                    nextPartialRes = state._2.head

                    _ <- nextPartialRes match {
                        case x: PartialResult.NeedsInput.FirstOrOnly => pure0(())
                        case x: State.Intermediate[Status.NeedsInput] =>
                            for {
                                thisPartialRes <- liftF0(
                                  RolloutTx
                                      .Builder(
                                        payouts = payouts,
                                        headNativeScript = headNativeScript,
                                        headNativeScriptReferenceInput = hnsReferenceOutput,
                                        env = env,
                                        validators = validators,
                                        mbRolloutOutputValue = Some(x.inputValueNeeded)
                                      )
                                      .buildPartial()
                                )
                                _ <- modify0(_.prepend(thisPartialRes))

                            } yield (())

                        case x: State.Last[Status.NeedsInput] =>
                            for {
                                thisPartialRes <- liftF0(
                                  RolloutTx
                                      .Builder(
                                        payouts = payouts,
                                        headNativeScript = headNativeScript,
                                        headNativeScriptReferenceInput = hnsReferenceOutput,
                                        env = env,
                                        validators = validators,
                                        mbRolloutOutputValue = None
                                      )
                                      .buildPartial()
                                )
                                _ <- modify0(_.prepend(thisPartialRes))
                            } yield (())
                    }
                } yield (())
            }

            for {
                finalRollout <- RolloutTx
                    .Builder(
                      payouts = payouts,
                      headNativeScript = headNativeScript,
                      headNativeScriptReferenceInput = hnsReferenceOutput,
                      env = env,
                      validators = validators,
                      mbRolloutOutputValue = None
                    )
                    .buildPartial()

                partialResults: RolloutPartialResultChain <-
                    finalRollout match {
                        case value: State.Only[NeedsInput] => Right((value, List.empty))
                        case _ =>
                            for {
                                prs <- mkPartialResults.runS(NonEmptyList.one(finalRollout))
                                first0 = prs.head
                                first <- first0 match {
                                    case f: State.First[NeedsInput] => Right(f)
                                    case _                          => Left(IncoherentChainingError)
                                }
                                castedTail <- Try(
                                  prs.asInstanceOf[List[PartialResult.NeedsInput.NotFirst]]
                                ) match
                                    case Success(s) => Right(s)
                                    case Failure(_) => Left(IncoherentChainingError)

                            } yield (first, castedTail)
                    }
            } yield partialResults
        }

        /** This finishes the partial chain by calling [[completeBuild]] and threading through the
          * results.
          * @param partialResults
          * @param firstRolloutInput
          * @return
          */
        def finishPartialChain(
            partialResults: NonEmptyList[PartialResult.NeedsInput],
            firstRolloutInput: TransactionInput
        ): RolloutErrorOr[NonEmptyList[RolloutTx]] = {
            type RolloutKleisli =
                Kleisli[RolloutErrorOr, TransactionInput, RolloutTx]

            for {
                firstRolloutTx <- completeBuild(partialResults.head, firstRolloutInput)

                // A list of the functions that produce the remaining rollout transactions
                kleislis: List[RolloutKleisli] = partialResults.tail
                    .map(pr => (txIn: TransactionInput) => completeBuild(pr, txIn))
                    .map(Kleisli(_))

                // ===================================
                // The tricky part
                // ===================================
                // Now we need to feed the TransactionInputs corresponding to the rollout utxos through the
                // full transaction sequence.
                //
                // But each one of these could potentially fail during building, balancing, or verification if our
                // implementation is wrong, so we need to retain the ability to return a Left. We basically want a
                // monadic scan.
                //
                // We can't use a traverse, because traversing doesn't accumulate. Using a fold itself would be
                // cumbersome. What we want is mapAccumulate, which passes along an accumulation value.
                // In our case, the "accumulator" is just the _previous_ rollout transaction -- we only need to
                // keep around the previous RolloutTx to know the input for the next
                //
                // The type signature for map accumulate is
                // def mapAccumulate[S, B](init: S)(f: (S, A) => (S, B)): (S, F[B]) = (...)
                // where
                // S = Either[BuildError, RolloutTx]
                // A = RolloutKleisli
                // B = Either[BuildError, RolloutTx]
                // F = NonEmptyList
                //
                // So we get out the F[B] (a non-empty list of Eithers), call .sequence, and we're done.
                //
                // P.S.: I think this is a kleisli endomorphism scan where, basically:
                //   type Kendo[F[_], A] = Kleisli[[X] =>> F[X], A ,A]
                //   type RolloutKendo = Kendo[[X] =>> Either[BuildError, X], RolloutTx]
                // and the scan function is >=>. Writing it in terms of a kendo would require massaging the types
                // a bit more.
                rolloutTxs <- kleislis
                    .mapAccumulate(Right(firstRolloutTx))(
                      (
                          ePreviousRolloutTx: RolloutErrorOr[RolloutTx],
                          k: RolloutKleisli
                      ) => {
                          val res = for {
                              previousRolloutTx <- ePreviousRolloutTx
                              prevTxId <- previousRolloutTx.mbRolloutProduced match {
                                  // This shouldn't be possible. It means that the previous rollout transaction
                                  // _didn't_ produce a rollout utxo, but it should have -- we still have another
                                  // rollout Tx to produce.
                                  case None => Left(RolloutTx.Builder.IncoherentBalancingError)
                                  case Some(utxo) => Right(utxo.utxo._1)
                              }
                              rolloutTx <- k.run(prevTxId)
                          } yield rolloutTx
                          (res, res)
                      }
                    )
                    ._2
                    .sequence
            } yield NonEmptyList(firstRolloutTx, rolloutTxs)
        }

        // TODO add to RolloutTx
        /** Finalizes the build for a partial result by spending the real transaction input
          * corresponding to the previous rollout Utxo
          * @param partialRes
          * @param spentRolloutTxIn
          * @return
          */
        private def completeBuild(
            partialRes: PartialResult.NeedsInput,
            spentRolloutTxIn: TransactionInput
        ): Either[BuildError, RolloutTx] = {
            val spentRolloutUtxo = TransactionUnspentOutput(
              spentRolloutTxIn,
              Babbage(
                address = headNativeScript.mkAddress(env.network),
                value = partialRes.inputValueNeeded
              )
            )
            for {
                unbalanced <- TransactionBuilder
                    .modify(
                      partialRes.txBuilderContext,
                      List(
                        Spend(
                          spentRolloutUtxo,
                          NativeScriptWitness(
                            NativeScriptAttached,
                            headNativeScript.requiredSigners
                          )
                        )
                      )
                    )
                    .left
                    .map(StepError(_))

                finalized <- unbalanced
                    .finalizeContext(
                      protocolParams = env.protocolParams,
                      diffHandler = prebalancedDiffHandler,
                      evaluator = env.evaluator,
                      validators = validators
                    )
                    .left
                    .map({
                        case balanceError: TxBalancingError =>
                            BalancingError(balanceError)
                        case validationError: TransactionException =>
                            ValidationError(validationError)
                    })

                res = partialRes match {
                    case last: PartialResult.NeedsInput.LastOrOnly =>
                        RolloutTx.Last(RolloutUtxo(spentRolloutUtxo.toTuple), finalized.transaction)
                    case first: PartialResult.NeedsInput.NotLast =>
                        RolloutTx.Intermediate(
                          rolloutSpent = RolloutUtxo(spentRolloutUtxo.toTuple),
                          rolloutProduced = RolloutUtxo(
                            TransactionInput(transactionId = finalized.transaction.id, index = 0),
                            finalized.transaction.body.value.outputs.head.value
                          ),
                          tx = finalized.transaction
                        )
                }
            } yield res
        }
    }

    object Builder:

        type Error = BuildError | IncoherentBalancingError.type | IncoherentChainingError.type

        /** Returned when the first partial result in the chain isn't of type
          * State.First[NeedsInput], or when an element of the tail isn't of type
          * PartialResult.NeedsInputNotFirst
          */
        object IncoherentChainingError
}
