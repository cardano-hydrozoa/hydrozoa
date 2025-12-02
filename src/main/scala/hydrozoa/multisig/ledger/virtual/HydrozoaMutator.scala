package hydrozoa.multisig.ledger.virtual

import hydrozoa.multisig.ledger.VirtualLedger
import hydrozoa.multisig.ledger.VirtualLedger.{Config, State}
import scalus.cardano.ledger.TransactionException
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{Context as _, State as L1State, *}

/*
We define three mutators for the three L2 event types (Genesis, Transaction, Withdrawal).

Then, finally, we define a mutator that both validates and processes any L2Event
 */

object HydrozoaGenesisMutator {
    // Fold over utxos passed in the genesis event, adding them to the UtxoSet with the same txId and an incrementing
    // index
    def addGenesisUtxosToState(
        g: Seq[GenesisObligation],
        state: State
    ): State = {
        state.copy(state.activeUtxos ++ g.map(go => go.toUtxo))
    }
}

object HydrozoaTransactionMutator {
    def transit(
        context: Config,
        state: State,
        l2Event: L2Event
    ): Either[String | TransactionException, State] = l2Event match {
        case L2EventTransaction(event) =>
            // A helper for mapping the error type and applying arguments
            def helper(v: Validator): Either[String | TransactionException, Unit] =
                v.validate(context.toL1Context, L1State(utxos = state.activeUtxos), event)
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
                _ <- helper(TransactionSizeValidator)
                _ <- helper(ValueNotConservedUTxOValidator)
                _ <- helper(VerifiedSignaturesInWitnessesValidator)
                _ <- helper(ExactSetOfRedeemersValidator)
                _ <- helper(ScriptsWellFormedValidator)
                _ <- helper(ProtocolParamsViewHashesMatchValidator)
                _ <- helper(WrongNetworkValidator)
                _ <- helper(WrongNetworkInTxBodyValidator)
                // Upstream mutators
                state <-
                    RemoveInputsFromUtxoMutator.transit(
                      context.toL1Context,
                      state.toScalusState,
                      event
                    )
                state <- AddOutputsToUtxoMutator.transit(context.toL1Context, state, event)
            yield State.fromScalusState(state)
        case _ => Right(state)
    }
}

object HydrozoaWithdrawalMutator {
    def transit(
        context: Config,
        state: State,
        l2Event: L2Event
    ): Either[String | TransactionException, State] = l2Event match {
        case L2EventWithdrawal(event) =>
            // A helper for mapping the error type and applying arguments
            def helper(v: Validator): Either[String | TransactionException, Unit] =
                v.validate(context.toL1Context, state.toScalusState, event)

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
                _ <- helper(ExactSetOfRedeemersValidator)
                _ <- helper(ScriptsWellFormedValidator)
                _ <- helper(ProtocolParamsViewHashesMatchValidator)
                _ <- helper(WrongNetworkValidator)
                _ <- helper(WrongNetworkInTxBodyValidator)
                // Upstream mutators
                state <-
                    RemoveInputsFromUtxoMutator.transit(
                      context.toL1Context,
                      state.toScalusState,
                      event
                    )
            yield State.fromScalusState(state)
        case _ => Right(state)
    }
}

//////////////////
// Helper

private def mapLeft[A, B, C](f: A => C)(e: Either[A, B]): Either[C, B] = e match {
    case Left(a)  => Left(f(a))
    case Right(b) => Right(b)
}
