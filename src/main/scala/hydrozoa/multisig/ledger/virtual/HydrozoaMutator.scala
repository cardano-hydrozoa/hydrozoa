package hydrozoa.multisig.ledger.virtual

import hydrozoa.multisig.ledger.VirtualLedger
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.rules.STS.Validator

/*
We define three mutators for the three L2 event types (Genesis, Transaction, Withdrawal).

Then, finally, we define a mutator that both validates and processes any L2Event
 */

object HydrozoaGenesisMutator {
    // Fold over utxos passed in the genesis event, adding them to the UtxoSet with the same txId and an incrementing
    // index
    def addGenesisUtxosToState(
        g: Seq[GenesisObligation],
        state: VirtualLedger.State
    ): VirtualLedger.State = {
        state.copy(activeUtxos = state.activeUtxos ++ g.map(go => go.toUtxo))
    }
}

// Change: 
//   - We remove all inputs as usual, but we only add outputs to the UTxO Set if they are L2-bound
//   - Update return type to return new (newState, listOfPayoutObligations)
object HydrozoaTransactionMutator extends STSL2.Mutator {
    override def transit(context: Context, state: State, l2Event: L2Event): Result = l2Event match {
        case L2EventTransaction(event) => {
            // A helper for mapping the error type and applying arguments
            def helper(v: Validator): Either[Error, Unit] =
                (v.validate(context, state, event))
            for
                _ <- L2ConformanceValidator.validate(context, state, l2Event)
                // Upstream validators (applied alphabetically for ease of comparison in a file browser
                // FIXME/Note (Peter, 2025-07-22): I don't know if all of these will apply or if this list is exhaustive,
                // but I've removed the rules that I'm certain won't apply
                _ <- helper(AllInputsMustBeInUtxoValidator)
                _ <- helper(EmptyInputsValidator)
                _ <- helper(InputsAndReferenceInputsDisjointValidator)
                _ <- helper(MissingKeyHashesValidator)
                _ <- helper(MissingOrExtraScriptHashesValidator)
                _ <- helper(NativeScriptsValidator)
                _ <- helper(OutputsHaveNotEnoughCoinsValidator)
                _ <- helper(OutputsHaveTooBigValueStorageSizeValidator)
                _ <- helper(OutsideValidityIntervalValidator)
                _ <- helper(TransactionSizeValidator) // Do we need this?
                _ <- helper(ValueNotConservedUTxOValidator)
                _ <- helper(VerifiedSignaturesInWitnessesValidator)
                /* Not yet implemented validators (2025-07-22)
                _ <- helper(ExactSetOfRedeemersValidator)
                _ <- helper(ScriptsWellFormedValidator)
                _ <- helper(ProtocolParamsViewHashesMatchValidator)
                _ <- helper(WrongNetworkValidator)
                _ <- helper(WrongNetworkInTxBodyValidator)
                 */
                // Upstream mutators
                state <- (
                  RemoveInputsFromUtxoMutator.transit(context, state, event)
                )
                state <- (AddOutputsToUtxoMutator.transit(context, state, event))
            yield state
        }
        case _ => Right(state)
    }
}

// TODO: Drop this completely
object HydrozoaWithdrawalMutator extends STSL2.Mutator {
    override def transit(context: Context, state: State, l2Event: L2Event): Result = l2Event match {
        case L2EventWithdrawal(event) => {
            // A helper for mapping the error type and applying arguments
            def helper(v: Validator): Either[Error, Unit] =
                (v.validate(context, state, event))

            for
                // L2 Native validators
                _ <-
                    if event.body.value.outputs.nonEmpty then
                        Left("Withdrawals cannot have outputs")
                    else Right(())
                _ <- L2ConformanceValidator.validate(context, state, l2Event)

                // Upstream validators (applied alphabetically for ease of comparison in a file browser
                // FIXME/Note (Peter, 2025-07-22): I don't know if all of these will apply or if this list is exhaustive,
                // but I've removed the rules that I'm certain won't apply

                // Differs from transactions in that the following rules are removed:
                // - OutputsHaveNotEnoughCoinsValidator
                // - OutputsHaveTooBigValuesStorageSizeValidator
                // - ValueNotConservedUtxOValidator
                _ <- helper(AllInputsMustBeInUtxoValidator)
                _ <- helper(EmptyInputsValidator)
                _ <- helper(InputsAndReferenceInputsDisjointValidator)
                _ <- helper(VerifiedSignaturesInWitnessesValidator)
                _ <- helper(MissingKeyHashesValidator)
                _ <- helper(MissingOrExtraScriptHashesValidator)
                _ <- helper(NativeScriptsValidator)
                _ <- helper(TransactionSizeValidator)
                _ <- helper(OutsideValidityIntervalValidator)
                _ <- helper(VerifiedSignaturesInWitnessesValidator)
                /* Not yet implemented validators (2025-07-22)
                _ <- helper(ExactSetOfRedeemersValidator)
                _ <- helper(ScriptsWellFormedValidator)
                _ <- helper(ProtocolParamsViewHashesMatchValidator)
                _ <- helper(WrongNetworkValidator)
                _ <- helper(WrongNetworkInTxBodyValidator)
                 */
                // Upstream mutators
                state <- (
                  RemoveInputsFromUtxoMutator.transit(context, state, event)
                )
            yield state
        }
        case _ => Right(state)
    }
}

//////////////////
// Helper

private def mapLeft[A, B, C](f: A => C)(e: Either[A, B]): (Either[C, B]) = e match {
    case Left(a)  => Left(f(a))
    case Right(b) => Right(b)
}
