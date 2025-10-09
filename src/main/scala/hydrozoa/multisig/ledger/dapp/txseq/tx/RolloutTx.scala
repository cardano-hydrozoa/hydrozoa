package hydrozoa.multisig.ledger.dapp.txseq.tx

import cats.data.{Kleisli, NonEmptyList, StateT}
import hydrozoa.lib.tx.ScriptSource.NativeScriptAttached
import hydrozoa.lib.tx.TransactionBuilder.Context
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.{RolloutTx, SettlementTx}
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.{BuilderContext, TxBalancingError}

import scala.collection.immutable.SortedMap

object RolloutTx {

    import hydrozoa.lib.tx.*
    import hydrozoa.lib.tx.BuildError.*
    import hydrozoa.lib.tx.TransactionBuilderStep.*

    type Cont1 = Coin => PartialResult2

    type PartialResult1 = (
        Cont1, // Coin => (Coin, Cont2)
        Seq[Babbage]
    )

    type Cont2 = RolloutUtxo => (Option[RolloutUtxo], RolloutTx)

    /** This Cont2 during `traverseInput` should:
      *   - update the "spend" step for rollout utxo
      *   - this won't change fees
      *   - rebuild the tx
      */
    type PartialResult2 = (
        Coin,
        Cont2,
    )

    /** @param toWithdrawReversed
      * @param mkSettlement
      * @param hnsReferenceOutput
      * @param headNativeScript
      * @param rolloutTokenName
      * @param ctx
      * @return
      *   The settlement tx accompanied by a _chain_ of rollout transactions. TODO: make a newtype
      */
    def finalizeWithRollout(
        // Should probably newtype this so we don't forget to reverse or reverse twice
        toWithdrawReversed: NonEmptyList[Babbage],
        // A way to make a settlement transaction once it is given the value for the rollout utxo.
        // The Right return value contains the settlementTx and the transaction input of the first rollout utxo
        mkSettlement: Value => Either[BuildError, (Context, TransactionInput)],
        // TODO Factor out context
        hnsReferenceOutput: TransactionUnspentOutput,
        headNativeScript: HeadMultisigScript,
        rolloutTokenName: AssetName,
        ctx: BuilderContext
    ): Either[BuildError, (SettlementTx, NonEmptyList[RolloutTx])] = {
        // The state is a sequence that we _prepend_ partial res's to. The final sequence reflects the
        // correct ordering for the rollouts
        type RolloutM[A] = StateT[[X] =>> Either[BuildError, X], NonEmptyList[PartialRes3], A]

        def pure0[A] = StateT.pure[[X] =>> Either[BuildError, X], NonEmptyList[PartialRes3], A]
        def liftF0[A] = StateT.liftF[[X] =>> Either[BuildError, X], NonEmptyList[PartialRes3], A]
        def modify0 = StateT.modify[[X] =>> Either[BuildError, X], NonEmptyList[PartialRes3]]
        def get0 = StateT.get[[X] =>> Either[BuildError, X], NonEmptyList[PartialRes3]]

        val mkDemockingFunctions: RolloutM[Unit] =
            for {
                state <- get0

                // The partial result related to the NEXT rollout transaction
                nextPartialRes = state.head

                _ <-
                    if nextPartialRes.remainingWithdrawalsReversed.isEmpty
                    then pure0(())
                    else
                        for {
                            thisPartialRes <- liftF0(
                              mockIntermediateRollout(
                                toWithdrawReversed = nextPartialRes.remainingWithdrawalsReversed,
                                hasPreviousRollout = ???,
                                nextRolloutValue =
                                    nextPartialRes.requiredValueForPreviousRolloutUtxO,
                                hnsReferenceOutput = hnsReferenceOutput,
                                headNativeScript = headNativeScript,
                                rolloutTokenName = rolloutTokenName,
                                ctx = ctx
                              )
                            )

                            _ <- modify0(_.prepend(thisPartialRes))

                        } yield (())
            } yield (())

        type RolloutKleisli = Kleisli[[X] =>> Either[BuildError, X], TransactionInput, RolloutTx]

        for {
            seed <- mockFinalPayout(
              toWithdrawReversed = toWithdrawReversed,
              hasPreviousRollout = ???,
              hnsReferenceOutput = hnsReferenceOutput,
              headNativeScript = headNativeScript,
              rolloutTokenName = rolloutTokenName,
              ctx = ctx
            )

            partialResults <- mkDemockingFunctions.runS(NonEmptyList.one(seed))

            listOfFuncs: NonEmptyList[TransactionInput => Either[BuildError, Context]] =
                partialResults.map(_.completeBuild)

            firstRolloutUtxoValue = partialResults.head.requiredValueForPreviousRolloutUtxO

            settlementTxAndRollout: (Context, TransactionInput) <- mkSettlement(
              firstRolloutUtxoValue
            )

            // And then feed the rollout UTxO through the Kleisli of the partial results
        } yield ???
    }

    def mockIntermediateRollout(
        // FIXME: Originally I was thinking of doing a "pessimisstic payout", but we don't need to.
        // We can pay out as many UTxOs as fit in this transaction, and then strip them off until
        // we can fit the previous rollout output as well.
        toWithdrawReversed: Seq[Babbage],
        // This is being passed as an argument for now only because its easier for me to
        // sketch
        hasPreviousRollout: Boolean,
        nextRolloutValue: Value,

        // TODO: Factor out
        hnsReferenceOutput: TransactionUnspentOutput,
        headNativeScript: HeadMultisigScript,
        rolloutTokenName: AssetName,
        ctx: BuilderContext
        // Right needs to return a potentially final result as well
    ): Either[BuildError, PartialRes3] = {

        val nativeScriptWitness =
            NativeScriptWitness(NativeScriptAttached, headNativeScript.requiredSigners)

        //////////////////////////////////////////////////
        // "Real" Steps
        val referenceHNS: ReferenceOutput = ReferenceOutput(hnsReferenceOutput)

        // We know this concretely because we calculated the fees previously
        val sendNextRollout: Send = Send(
          Babbage(
            address = headNativeScript.mkAddress(ctx.network),
            value = nextRolloutValue,
            datumOption = None,
            scriptRef = None
          )
        )

        val setAuxData: ModifyAuxiliaryData = ???

        // FIXME: This should be trialing deposits and adding until they don't fit, then
        // stripping off until we can fit the previous rollout UTxO
        val singletonWithdrawal: Send = Send(toWithdrawReversed.head)

        val steps =
            Seq(referenceHNS, singletonWithdrawal, sendNextRollout, setAuxData)

        ///////////////////////////////
        // Now we mock the previous roll out
        val mockTxId = TransactionInput(
          transactionId = TransactionHash.fromByteString(???),
          index = 0
        )

        // The previous rollout is constructed here with a mock transaction ID.
        // We set its Coin to zero and its multiasset to contain the rollout token; later, during balancing,
        // we modify its value to account for the correct fee.
        val mockPreviousRollout: Seq[Spend] =
            if hasPreviousRollout then
                Seq(
                  Spend(
                    TransactionUnspentOutput(
                      mockTxId,
                      Babbage(
                        address = headNativeScript.mkAddress(ctx.network),
                        value = Value(
                          coin = Coin.zero,
                          multiAsset = MultiAsset(
                            SortedMap((headNativeScript.policyId, SortedMap((rolloutTokenName, 1))))
                          )
                        ),
                        datumOption = None,
                        scriptRef = None
                      )
                    ),
                    nativeScriptWitness
                  )
                )
            else Seq.empty

        for {
            staticCtx <- TransactionBuilder.build(ctx.network, steps).left.map(StepError(_))

            mockedCtx <- TransactionBuilder
                .modify(staticCtx, mockPreviousRollout)
                .left
                .map(StepError(_))

            mockedFinalized <- mockedCtx
                .finalizeContext(
                  ctx.protocolParams,
                  // change INPUT diff handler: on a failing balance,
                  // add to the previous rollout utxo we are spending as in INPUT.
                  // The current Type of diffHandler doesn't give it access to the resolved UTxOs. We
                  // need to modify its type.
                  diffHandler = ???,
                  evaluator = ctx.evaluator,
                  validators = ctx.validators
                )
                .left
                .map({
                    case balanceError: TxBalancingError => BalancingError(balanceError)
                    case validationError: TransactionException =>
                        ValidationError(validationError)
                })

            // FIXME: partial
            requiredValue = mockedFinalized.resolvedUtxos.utxos.get(mockTxId).get.value

            demockingFunction = (realPayoutTI: TransactionInput) => {
                val realRollout = Spend(
                  TransactionUnspentOutput(
                    realPayoutTI,
                    Babbage(
                      address = headNativeScript.mkAddress(ctx.network),
                      value = requiredValue,
                      datumOption = None,
                      scriptRef = None
                    )
                  )
                )
                for {
                    unbalanced <- TransactionBuilder
                        .modify(staticCtx, Seq(realRollout))
                        .left
                        .map(StepError(_))
                    finalized <-
                        unbalanced
                            .finalizeContext(
                              ctx.protocolParams,
                              // Add this point, the tx MUST already be balanced, so this should just be a no-op (throwing
                              // an error if this isn't true
                              diffHandler = ???, // Already balanced
                              evaluator = ctx.evaluator,
                              validators = ctx.validators
                            )
                            .left
                            .map({
                                case balanceError: TxBalancingError => BalancingError(balanceError)
                                case validationError: TransactionException =>
                                    ValidationError(validationError)
                            })
                } yield finalized
            }

        } yield PartialRes3(
          completeBuild = ???,
          requiredValueForPreviousRolloutUtxO = ???,
          remainingWithdrawalsReversed = ???
        )
    }

    case class PartialRes3(
        // TransactionInput => Either[BuildError, RolloutTx]
        completeBuild: TransactionInput => Either[BuildError, Context],
        requiredValueForPreviousRolloutUtxO: Value,
        remainingWithdrawalsReversed: Seq[Babbage]
    )

    /** Is identical in size to the actual final payout, with a mocked Tx Id. The size of the
      * _previous_ rollout UTxO is set _during balancing_ such that it contains enough for fees +
      * withdrawal to cover this transaction.
      *
      * @param toWithdraw
      * @param previousRolloutAmount
      * @param hnsReferenceOutput
      * @param headNativeScript
      * @param rolloutTokenName
      * @param ctx
      * @return
      *   A tuple consisting of:
      *   - A function that will finalize the _actual_ final payout
      *   - The value that the previous rollout UTxO must contain
      */
    def mockFinalPayout(
        // FIXME: Originally I was thinking of doing a "pessimisstic payout", but we don't need to.
        // We can pay out as many UTxOs as fit in this transaction, and then strip them off until
        // we can fit the previous rollout output as well.
        toWithdrawReversed: NonEmptyList[Babbage],
        hasPreviousRollout: Boolean,

        // Context, factor out
        hnsReferenceOutput: TransactionUnspentOutput,
        headNativeScript: HeadMultisigScript,
        rolloutTokenName: AssetName,
        ctx: BuilderContext
    ): Either[BuildError, PartialRes3] = {

        val nativeScriptWitness =
            NativeScriptWitness(NativeScriptAttached, headNativeScript.requiredSigners)

        // Static steps
        val referenceHNS: ReferenceOutput = ReferenceOutput(hnsReferenceOutput)

        val mockTxId = TransactionInput(
          transactionId = TransactionHash.fromByteString(???),
          index = 0
        )

        // The previous rollout is constructed here with a mock transaction ID.
        // We set its Coin to zero and its multiasset to contain the rollout token; later, during balancing,
        // we modify its value to account for the correct fee.
        val mockRollout: Seq[Spend] =
            if hasPreviousRollout then
                Seq(
                  Spend(
                    TransactionUnspentOutput(
                      mockTxId,
                      Babbage(
                        address = headNativeScript.mkAddress(ctx.network),
                        value = Value(
                          coin = Coin.zero
                        ),
                        datumOption = None,
                        scriptRef = None
                      )
                    ),
                    nativeScriptWitness
                  )
                )
            else Seq.empty

        // We can add more withdrawals, this is just temporary
        val singletonWithdrawal: Send = Send(toWithdrawReversed.head)

        val burnRolloutToken: Mint = Mint(
          headNativeScript.policyId,
          assetName = rolloutTokenName,
          amount = -1L,
          witness = nativeScriptWitness
        )

        val setAuxData: ModifyAuxiliaryData = ???

        val staticSteps =
            Seq(referenceHNS, singletonWithdrawal, burnRolloutToken, setAuxData)

        for {
            // These are the baseline steps we KNOW we need to run
            staticCtx <- TransactionBuilder
                .build(
                  ctx.network,
                  staticSteps
                )
                .left
                .map(StepError(_))

            // In addition, we add the mocked spent rollout for fee calculation
            mockedCtx <- TransactionBuilder.modify(staticCtx, mockRollout).left.map(StepError(_))

            mockFinalized <- mockedCtx
                .finalizeContext(
                  ctx.protocolParams,
                  // change INPUT diff handler: on a failing balance,
                  // add to the previous rollout utxo we are spending as in INPUT.
                  // The current Type of diffHandler doesn't give it access to the resolved UTxOs. We
                  // need to modify its type.
                  diffHandler = ???,
                  evaluator = ctx.evaluator,
                  validators = ctx.validators
                )
                .left
                .map({
                    case balanceError: TxBalancingError => BalancingError(balanceError)
                    case validationError: TransactionException =>
                        ValidationError(validationError)
                })

            // FIXME: Partial
            requiredValue = mockFinalized.resolvedUtxos.utxos.get(mockTxId).get.value

            demockingFunction = (realPayoutTI: TransactionInput) => {
                val realRollout = Spend(
                  TransactionUnspentOutput(
                    realPayoutTI,
                    Babbage(
                      address = headNativeScript.mkAddress(ctx.network),
                      value = requiredValue,
                      datumOption = None,
                      scriptRef = None
                    )
                  )
                )

                for {
                    unbalanced <- TransactionBuilder
                        .modify(staticCtx, Seq(realRollout))
                        .left
                        .map(StepError(_))
                    finalized <-
                        unbalanced
                            .finalizeContext(
                              ctx.protocolParams,
                              // Add this point, the tx MUST already be balanced, so this should just be a no-op (throwing
                              // an error if this isn't true
                              diffHandler = ???, // Already balanced
                              evaluator = ctx.evaluator,
                              validators = ctx.validators
                            )
                            .left
                            .map({
                                case balanceError: TxBalancingError => BalancingError(balanceError)
                                case validationError: TransactionException =>
                                    ValidationError(validationError)
                            })
                } yield finalized
            }

        } yield PartialRes3(demockingFunction, requiredValue, toWithdrawReversed.tail)

    }

    /** Tries to add as many direct payouts as possible and returns the result and the rest of
      * withdrawals.
      *
      *   - Rollout output on every step should contain the exact amount of ada to pay out the rest
      *     of withdrawals.
      *   - Transactions should reserve some space (in form of a fake metadata) to account the
      *     increase of rollout output when running `traverseFee`. We can't use ada value for that
      *     since later we want to add fees, so we need to preserve the amount.
      *
      * Cont1 = Coin => PartialResult2 should:
      *   - add Coin to ada in the rollout output
      *   - change that "send" step
      *   - remove phony metadata step
      *   - rebalance to get the fees, return in the PartialResult2
      *
      * Outcomes:
      *   - list of withdrawals is depleted
      *   - some withdrawals don't fit
      *
      * @param remainingPayouts
      *   payouts that we still need to allocate into rollout txs
      * @return
      */
    def build(
        remainingPayouts: NonEmptyList[Babbage]
    ): Option[PartialResult1] = { ??? }

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
