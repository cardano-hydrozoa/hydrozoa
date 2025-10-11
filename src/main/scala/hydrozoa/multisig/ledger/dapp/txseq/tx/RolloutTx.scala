package hydrozoa.multisig.ledger.dapp.txseq.tx

import cats.*
import cats.data.*
import cats.implicits.toTraverseOps
import cats.syntax.all.*
import hydrozoa.lib.tx.ScriptSource.NativeScriptAttached
import hydrozoa.lib.tx.TransactionBuilder.Context
import hydrozoa.lib.tx.TransactionUnspentOutput
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.{prebalancedDiffHandler, reportDiffHandler}
import scalus.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.TxBalancingError.CantBalance
import scalus.cardano.ledger.txbuilder.{BuilderContext, TxBalancingError}

final case class ROTx(
    rolloutSpent: RolloutUtxo,
    rolloutProduced: Option[RolloutUtxo],
    override val tx: Transaction
) extends Tx

object RolloutTx {
    final case class Builder(
        hnsReferenceOutput: TransactionUnspentOutput,
        headNativeScript: HeadMultisigScript,
        ctx: BuilderContext
    ) {

        import hydrozoa.lib.tx.*
        import hydrozoa.lib.tx.BuildError.*
        import hydrozoa.lib.tx.TransactionBuilderStep.*

        enum IncoherenceError:
            /** This is returned if finalizeWithRollout encounters a condition where it has been
              * asked to produce a chain of rollout transactions, r1 -> r2 -> ... -> r_n, but some
              * r_(i-1) did not produce a rollout utxo for r_i to consume. This shouldn't happen if
              * the algorithm has been implemented correctly.
              */
            case IncoherentPartialRollout

            /** This is returned if we get an unexpected error while trying to examine the diff of
              * the mock rollout tx.
              */
            case IncoherentBalancing

        /** @param toWithdrawReversed
          * @param mkSettlement
          * @return
          *   The settlement tx accompanied by a _chain_ of rollout transactions. TODO: make a
          *   newtype
          */
        def finalizeWithRollout(
            // Should probably newtype this so we don't forget to reverse or reverse twice
            toWithdrawReversed: NonEmptyList[Babbage],
            // A way to make a settlement transaction once it is given the value for the rollout utxo.
            // The Right return value contains the settlementTx and the transaction input of the first rollout utxo
            mkSettlement: Coin => Either[BuildError, (SettlementTx, TransactionInput)]
        ): Either[
          BuildError | IncoherenceError,
          (SettlementTx, NonEmptyList[ROTx])
        ] = {

            type RolloutM[A] =
                StateT[[X] =>> Either[BuildError | IncoherenceError, X], NonEmptyList[
                  PartialRolloutInfo
                ], A]

            // This creates a chain of functions (f1, f2, ..., fn), where each function should produce a rollout
            // transaction when given an input. `f1` is fed the first rollout utxo (from the settlement tx),
            // and the chain goes on. This is different than the order we build them -- we start with fn, then fn-1,
            // etc., prepending as we go.
            val mkDemockingFunctions: RolloutM[Unit] = {
                // The state is a sequence that we _prepend_ partial res's to. The final sequence reflects the
                // correct ordering for the rollouts

                def pure0[A] =
                    StateT.pure[[X] =>> Either[BuildError | IncoherenceError, X], NonEmptyList[
                      PartialRolloutInfo
                    ], A]

                def liftF0[A] =
                    StateT.liftF[[X] =>> Either[BuildError | IncoherenceError, X], NonEmptyList[
                      PartialRolloutInfo
                    ], A]

                def modify0 =
                    StateT.modify[[X] =>> Either[BuildError | IncoherenceError, X], NonEmptyList[
                      PartialRolloutInfo
                    ]]

                def get0 =
                    StateT.get[[X] =>> Either[BuildError | IncoherenceError, X], NonEmptyList[
                      PartialRolloutInfo
                    ]]

                for {
                    state <- get0

                    // The partial result related to the NEXT rollout transaction
                    nextPartialRes = state.head

                    // FIXME: Getting a weird type mismatch error here
//                    _ <-
//                        if nextPartialRes.remainingWithdrawalsReversed.isEmpty
//                        then pure0(())
//                        else
//                            for {
//                                thisPartialRes <- liftF0(
//                                  mockRollout(
//                                    toWithdrawReversed =
//                                        nextPartialRes.remainingWithdrawalsReversed,
//                                    nextRolloutCoin =
//                                        nextPartialRes.requiredCoinForPreviousRolloutUtxO
//                                  )
//                                )
//
//                                _ <- modify0(_.prepend(thisPartialRes))
//
//                            } yield (())
                } yield (())
            }

            type RolloutKleisli =
                Kleisli[[X] =>> Either[BuildError, X], TransactionInput, ROTx]

            for {
                finalRollout <- mockRollout(
                  toWithdrawReversed = toWithdrawReversed.toList,
                  nextRolloutCoin = Coin(0)
                )

                partialResults <- mkDemockingFunctions.runS(NonEmptyList.one(finalRollout))

                firstRolloutUtxoCoin = partialResults.head.requiredCoinForPreviousRolloutUtxO

                settlementTxAndRollout: (SettlementTx, TransactionInput) <- mkSettlement(
                  firstRolloutUtxoCoin
                )
                (settlementTx, firstRolloutTxId) = settlementTxAndRollout

                firstRolloutTx <- partialResults.head.completeBuild(firstRolloutTxId)

                // A list of the functions that produce the remaining rollout transactions
                kleislis: List[RolloutKleisli] = partialResults.tail
                    .map(_.completeBuild)
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
                // keep around the previous.
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
                          ePreviousRolloutTx: Either[
                            BuildError | IncoherenceError,
                            ROTx
                          ],
                          k: RolloutKleisli
                      ) => {
                          val res = for {
                              previousRolloutTx <- ePreviousRolloutTx
                              prevTxId <- previousRolloutTx.rolloutProduced match {
                                  // This shouldn't be possible. It means that the previous rollout transaction
                                  // _didn't_ produce a rollout utxo, but it should have -- we still have another
                                  // rollout Tx to produce.
                                  case None       => Left(IncoherenceError.IncoherentPartialRollout)
                                  case Some(utxo) => Right(utxo.utxo._1)
                              }
                              rolloutTx <- k.run(prevTxId)
                          } yield rolloutTx
                          (res, res)
                      }
                    )
                    ._2
                    .sequence
            } yield (settlementTx, NonEmptyList(firstRolloutTx, rolloutTxs))
        }

        def mockRollout(
            toWithdrawReversed: Seq[Babbage],
            // If 0, then this is the terminal rollout.
            nextRolloutCoin: Coin
        ): Either[BuildError | IncoherenceError, PartialRolloutInfo] = {

            val nativeScriptWitness =
                NativeScriptWitness(NativeScriptAttached, headNativeScript.requiredSigners)

            //////////////////////////////////////////////////
            // "Real" Steps
            val referenceHNS: ReferenceOutput = ReferenceOutput(hnsReferenceOutput)

            // We know this concretely because we calculated the fees previously
            val sendNextRollout: Option[Send] =
                if nextRolloutCoin.value == 0
                then None
                else
                    Some(
                      Send(
                        Babbage(
                          address = headNativeScript.mkAddress(ctx.network),
                          value = Value(nextRolloutCoin),
                          datumOption = None,
                          scriptRef = None
                        )
                      )
                    )

            val setAuxData: ModifyAuxiliaryData = ModifyAuxiliaryData(_ =>
                Some(MD(MD.L1TxTypes.Rollout, headNativeScript.mkAddress(ctx.network)))
            )

            case class Withdrawals(withdrawNow: Seq[Babbage], withdrawEarlier: Seq[Babbage])

            ///////////////////////////////
            // Now we mock the previous rollout
            val mockTxId = TransactionInput(
              transactionId =
                  TransactionHash.fromByteString(ByteString.fromArray(Array.fill(32)(0.toByte))),
              index = 0
            )

            // The previous rollout is constructed here with a mock transaction ID.
            // We set its Coin to zero because we want to see the deficit after balancing.
            val mockPreviousRollout0Ada: Spend =
                Spend(
                  TransactionUnspentOutput(
                    mockTxId,
                    Babbage(
                      address = headNativeScript.mkAddress(ctx.network),
                      value = Value(
                        coin = Coin.zero
                      )
                    )
                  ),
                  nativeScriptWitness
                )

            for {
                // These steps never change
                staticCtx <- TransactionBuilder
                    .build(ctx.network, List(referenceHNS, setAuxData))
                    .left
                    .map(StepError(_))

                // ===================================
                // Mock Rollout
                // ===================================

                // Pessimistic case: we _assume_ that we'll need another rollout included,
                // and try to fit all withdrawals with this in mind.
                withRollouts <- TransactionBuilder
                    .modify(staticCtx, sendNextRollout.toList.appended(mockPreviousRollout0Ada))
                    .left
                    .map(StepError(_))

                withdrawals: Withdrawals <- Right(???) // addWithdrawals

                withWithdrawals <- TransactionBuilder
                    .modify(withRollouts, withdrawals.withdrawNow.map(Send(_)))
                    .left
                    .map(StepError(_))

                requiredAdaForPreviousRollout <- withWithdrawals.finalizeContext(
                  ctx.protocolParams,
                  diffHandler = reportDiffHandler,
                  evaluator = ctx.evaluator,
                  // Validators are empty since we're only looking at balance.
                  validators = List.empty
                ) match
                    case Left(CantBalance(diff)) => Right(diff)
                    // Not possible -- our diff handler only returns CantBalance
                    // and we don't run any validators
                    case _ => Left(IncoherenceError.IncoherentBalancing)

                // ===================================
                // Real rollout
                // ===================================

                finalizeFunction = (realPayoutTI: TransactionInput) => {
                    val spendRealRolloutInput = Spend(
                      TransactionUnspentOutput(
                        realPayoutTI,
                        Babbage(
                          address = headNativeScript.mkAddress(ctx.network),
                          value = Value(Coin(requiredAdaForPreviousRollout))
                        )
                      )
                    )
                    for {
                        unbalanced <- TransactionBuilder
                            .modify(
                              staticCtx,
                              withdrawals.withdrawNow
                                  .map(Send(_))
                                  .appended(spendRealRolloutInput)
                                  ++ sendNextRollout.toList
                            )
                            .left
                            .map(StepError(_))
                        finalized <-
                            unbalanced
                                .finalizeContext(
                                  ctx.protocolParams,
                                  diffHandler = prebalancedDiffHandler, // Already balanced
                                  evaluator = ctx.evaluator,
                                  validators = ctx.validators
                                )
                                .left
                                .map({
                                    case balanceError: TxBalancingError =>
                                        BalancingError(balanceError)
                                    case validationError: TransactionException =>
                                        ValidationError(validationError)
                                })

                        rolloutProducedTI = TransactionInput(
                          finalized.transaction.id,
                          finalized.transaction.body.value.outputs.size - 1
                        )
                    } yield ROTx(
                      rolloutSpent = RolloutUtxo(spendRealRolloutInput.utxo.toTuple),
                      rolloutProduced =
                          sendNextRollout.map(send => RolloutUtxo(rolloutProducedTI, send.output)),
                      tx = finalized.transaction
                    )
                }

            } yield PartialRolloutInfo(
              completeBuild = finalizeFunction,
              requiredCoinForPreviousRolloutUtxO = Coin(requiredAdaForPreviousRollout),
              remainingWithdrawalsReversed = withdrawals.withdrawEarlier
            )
        }

        case class PartialRolloutInfo(
            // NOTE: The context MUST have the rollout utxo as the final output.
            completeBuild: TransactionInput => Either[BuildError, ROTx],
            requiredCoinForPreviousRolloutUtxO: Coin,
            remainingWithdrawalsReversed: Seq[Babbage]
        )

        // NOTE: This addPayouts + loop thing has a lot in common with adding direct payouts to
        // the settlement Tx. I'm copy-pasting for now because I don't know if it will be _exactly_ the same
        // but my gut says we can factor it out. Factoring out will be important for testing.
        //
        //        def tryAddPayout(
        //            ctx: Context,
        //            toWithdraw: Babbage
        //        ): Either[
        //          BuildError,
        //          // Not total sure what this hsould be
        //          Unit
        //        ] =
        //            for {
        //                // Trial this payout, see if it fits. We yield this if we are successful.
        //                builderContextToKeep <- TransactionBuilder
        //                    .modify(ctx.contextSoFar, Seq(Send(toWithdraw)))
        //                    .left
        //                    .map(StepError(_))
        //
        //                result = ctx.copy(contextSoFar = builderContextToKeep)
        //
        //                // Fake steps added for balancing
        //                builderContextTemp <- TransactionBuilder
        //                    .modify(builderContextToKeep, ctx.extraSteps)
        //                    .left
        //                    .map(StepError(_))
        //
        //                // Perform a trial balance, see what happens
        //                _ <- builderContextTemp
        //                    .finalizeContext(
        //                      context.protocolParams,
        //                      diffHandler = new ChangeOutputDiffHandler(
        //                        context.protocolParams,
        //                        0
        //                      ).changeOutputDiffHandler,
        //                      evaluator = context.evaluator,
        //                      validators = context.validators
        //                    )
        //                    .left
        //                    .map({
        //                        case balanceError: TxBalancingError =>
        //                            BalancingError(balanceError)
        //                        case validationError: TransactionException =>
        //                            ValidationError(validationError)
        //                    })
        //            } yield result
        //    }

    }
}
