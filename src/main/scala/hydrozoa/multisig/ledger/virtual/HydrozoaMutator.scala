package hydrozoa.multisig.ledger.virtual

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.VirtualLedger
import hydrozoa.multisig.ledger.VirtualLedger.{Config, State}
import hydrozoa.multisig.ledger.dapp.token.CIP67
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.Metadatum.Int
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{Context as _, State as L1State, *}
import scalus.cardano.ledger.{KeepRaw, Metadatum, TransactionException, TransactionInput, TransactionOutput, Word64}

/*
We define three mutators for the two L2 event types (Genesis, Transaction).

Then, finally, we define a mutator that both validates and processes any L2Event
 */

object HydrozoaGenesisMutator {
    // Fold over utxos passed in the genesis event, adding them to the UtxoSet with the same txId and an incrementing
    // index
    def addGenesisUtxosToState(
        g: NonEmptyList[GenesisObligation],
        state: State
    ): State = {
        state.copy(state.activeUtxos ++ g.map(go => go.toUtxo).toList)
    }
}

// Change:
//   - We remove all inputs as usual, but we only add outputs to the UTxO Set if they are L2-bound
//   - Update return type to return new (newState, listOfPayoutObligations)
object HydrozoaTransactionMutator {
    def transit(
        context: Config,
        state: State,
        l2Event: L2EventTransaction
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
                // Native mutators
                state <- AddOutputsToUtxoL2Mutator.transit(context, state, l2Event)
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

/** Outputs to the transaction can be marked as "L2 bound" in the transaction metadata.
  */
object AddOutputsToUtxoL2Mutator:
    def transit(
        context: Config,
        state: State,
        event: L2EventTransaction
    ): Either[String | TransactionException, State] =
        for {
            metadataMap <- event.transaction.auxiliaryData match {
                case Some(keepRawM) =>
                    keepRawM.value match {
                        case Metadata(m) => Right(m)
                        case _           => Left("metadata not list")
                    }
                case _ => Left("Malformed metadata")
            }
            // Should we use a different tag here to indicate its L2?
            metaDatum <- metadataMap
                .get(Word64(CIP67.Tags.head))
                .toRight(
                  s"Head tag ${CIP67.Tags.head} not" +
                      "found in metadata map"
                )

            outputs = event.transaction.body.value.outputs

            // TODO: This is an idiot-proof way to do it. A better way might be a bitmask -- 0 for L1, 1 for L2
            indexList <- metaDatum match {
                case Metadatum.List(il: IndexedSeq[Metadatum])
                    if il.length == outputs.length
                        && il.forall(elem => elem == Int(1) || elem == Int(2)) =>
                    Right(il)
                case _ => Left("Malformed index list in L2 transaction")
            }

            // Format: ((output, l1OrL2), listIndex)
            zippedOutputs = outputs.zip(indexList).zipWithIndex

            l2UtxosToAdd: List[(TransactionInput, TransactionOutput)] = zippedOutputs.foldLeft(
              List.empty
            )((acc, elem) => {
                val l1Bound = elem._1._2 == Int(1)
                if l1Bound then acc
                else {
                    val input = TransactionInput(event.getEventId, elem._2)
                    acc.appended((input, elem._1._1.value))
                }
            })

        } yield state.copy(state.activeUtxos ++ l2UtxosToAdd)

//////////////////
// Helper

private def mapLeft[A, B, C](f: A => C)(e: Either[A, B]): Either[C, B] = e match {
    case Left(a)  => Left(f(a))
    case Right(b) => Right(b)
}
