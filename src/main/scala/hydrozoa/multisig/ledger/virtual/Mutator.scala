package hydrozoa.multisig.ledger.virtual

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.VirtualLedgerM
import hydrozoa.multisig.ledger.VirtualLedgerM.{Config, State}
import hydrozoa.multisig.ledger.virtual.tx.L2Tx
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{State as L1State, *}
import scalus.cardano.ledger.{CertState, Coin, TransactionException}

// FIXME: This is heavily inherited from the scalus STS. We don't really follow it too closely any more, so it
// could probably just be folded into VirtualLedger
object HydrozoaTransactionMutator {
    private[virtual] object CardanoLedgerContext {

        /** Turn into an L1 context with zero fee and an empty CertState
          */
        def fromCardanoNetwork(
            cardanoNetwork: CardanoNetwork.Section,
            time: QuantizedInstant
        ): Context = {
            import cardanoNetwork.*
            require(time.slotConfig == slotConfig)
            Context(
              fee = Coin(0),
              env = UtxoEnv(
                time.toSlot.slot,
                cardanoProtocolParams,
                CertState.empty,
                network
              ),
              slotConfig = slotConfig
            )
        }

    }

    def transit(
        config: Config,
        time: QuantizedInstant,
        state: State,
        l2Tx: L2Tx
    ): Either[String | TransactionException, State] = {

        // A helper for mapping the error type and applying arguments
        def helper(v: Validator): Either[String | TransactionException, Unit] =
            v.validate(
              CardanoLedgerContext.fromCardanoNetwork(config, time),
              L1State(utxos = state.activeUtxos),
              l2Tx.tx
            )
        for
            _ <- L2ConformanceValidator.validate(config, state, l2Tx)
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
            // Upstream mutators: removes inputs, adds all outputs (L1 and L2)
            scalusState <-
                PlutusScriptsTransactionMutator.transit(
                  CardanoLedgerContext.fromCardanoNetwork(config, time),
                  state.toScalusState,
                  l2Tx.tx
                )
            // Native mutators: removes the L1-marked outputs, leaving only L2 outputs
            state = EvacuatingMutator.transit(config, State.fromScalusState(scalusState), l2Tx)
        yield state
    }
}

/** TODO: Update
  *
  * Outputs to the transaction can be marked as "L2 bound" in the transaction metadata.
  */
object EvacuatingMutator:

    def transit(
        config: Config,
        state: State,
        l2Tx: L2Tx
    ): State =
        val l1UtxosToRemove = l2Tx.l1utxos.map(_._1).toSet
        // TODO: check all evacuatees exist?
        state.copy(state.activeUtxosKR -- l1UtxosToRemove)
