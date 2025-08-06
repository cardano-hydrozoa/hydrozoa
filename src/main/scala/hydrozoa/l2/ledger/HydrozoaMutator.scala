package hydrozoa.l2.ledger

import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.rules.STS.Validator

/*
We define three mutators for the three L2 event types (Genesis, Transaction, Withdrawal).

Then, finally, we define a mutator that both validates and processes any L2Event
 */

private object HydrozoaGenesisMutator extends STSL2.Mutator {
    // Fold over utxos passed in the genesis event, adding them to the UtxoSet with the same txId and an incrementing
    // index
    private def addGenesisUtxosToState(g: L2EventGenesis, state: State): State = {
        state.copy(utxo = state.utxo ++ g.resolvedL2UTxOs)
    }

    override def transit(context: Context, state: State, event: Event): Result = event match {
        case g: L2EventGenesis => Right(addGenesisUtxosToState(g, state))
        case _                 => Right(state)
    }
}

private object HydrozoaTransactionMutator extends STSL2.Mutator {
    override def transit(context: Context, state: State, l2Event: L2Event): Result = l2Event match {
        case L2EventTransaction(event) => {
            // A helper for mapping the error type and applying arguments
            def helper(v: Validator): Either[Error, Unit] =
                (v.validate(context, state, event))
            for
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

private object HydrozoaWithdrawalMutator extends STSL2.Mutator {
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

// The primary entry point for the L2 ledger
object HydrozoaL2Mutator extends STSL2.Mutator {
    override def transit(context: Context, state: State, event: Event): Result = {
        for
            _ <- L2ConformanceValidator.validate(context, state, event)
            state <- HydrozoaGenesisMutator(context, state, event)
            state <- HydrozoaTransactionMutator(context, state, event)
            state <- HydrozoaWithdrawalMutator(context, state, event)
        yield state
    }
}

//////////////////
// Helper

private def mapLeft[A, B, C](f: A => C)(e: Either[A, B]): (Either[C, B]) = e match {
    case Left(a)  => Left(f(a))
    case Right(b) => Right(b)
}
