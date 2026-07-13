package hydrozoa.multisig.ledger.eutxol2

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger.Config
import hydrozoa.multisig.ledger.eutxol2.tx.L2Tx
import scala.annotation.unused
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{State as L1State, *}
import scalus.cardano.ledger.{CertState, Coin, TransactionException, Utxos}

object HydrozoaTransactionMutator {
    private[eutxol2] object CardanoLedgerContext {

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

    /** Validate and apply an L2 transaction over the two compartments.
      *
      * Validation decomposes into two runs whose conjunction also implies the transient-token
      * conservation equation (`overlay_in + mint = declared transients`; with zero fees and no
      * withdrawals the three balances are linearly dependent, so it needs no third run):
      *
      *   - the full transaction — scripts, signatures, redeemers, sizes, and value conservation
      *     with the mint field — validates against the **combined** view (main + transient), the
      *     only view the Cardano ledger rules ever see;
      *   - the main projection ([[L2Tx.projectMain]]) re-checks value conservation against the
      *     **main** compartment alone, which both keeps every reachable state L1-remittable and
      *     makes minting or burning main-compartment (L1-native) tokens fail by arithmetic.
      *
      * The other validators need no projection run: input resolution on main equals resolution on
      * combined (overlay keys are a subset of main keys), and min-ADA / value-size on the combined
      * view are at least as strict as on the projection (monotone in value content; coin is
      * untouched by the split).
      *
      * The mutation runs once, over the combined view; the result splits back into compartments by
      * subtracting the post-transaction overlay (spent entries removed, declared bundles added
      * under the new utxo ids).
      */
    def transit(
        config: Config,
        time: QuantizedInstant,
        state: Compartments,
        l2Tx: L2Tx
    ): Either[String | TransactionException, Compartments] = {

        val context = CardanoLedgerContext.fromCardanoNetwork(config, time)
        val combined = TransientTokens.mkCombinedUtxos(state.main, state.transientTokens)

        // A helper for mapping the error type and applying arguments
        def helper(v: Validator): Either[String | TransactionException, Unit] =
            v.validate(
              context,
              L1State(utxos = combined),
              l2Tx.tx
            )
        for
            _ <- L2ConformanceValidator.validate(config, state.main, l2Tx)
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
            // The projection to the main compartment must balance against it alone
            _ <- ValueNotConservedUTxOValidator
                .validate(context, L1State(utxos = state.main), l2Tx.projectMain)
                .left
                .map(error => s"main-projection conservation: $error")
            // Upstream mutators: removes inputs, adds all outputs (L1 and L2)
            scalusState <-
                PlutusScriptsTransactionMutator.transit(
                  context,
                  scalus.cardano.ledger.rules.State(combined),
                  l2Tx.tx
                )
            // Native mutators: removes the L1-marked outputs, leaving only L2 outputs
            combinedNext = EvacuatingMutator.transit(config, scalusState.utxos, l2Tx)
            spentInputs = l2Tx.tx.body.value.inputs.toSet
            transientTokensNext =
                state.transientTokens.removedAll(spentInputs) ++ l2Tx.mkTransientUtxos
            mainNext = TransientTokens.projectMainUtxos(combinedNext, transientTokensNext)
        yield Compartments(mainNext, transientTokensNext)
    }
}

/** TODO: Update
  *
  * Outputs to the transaction can be marked as "L2 bound" in the transaction metadata.
  */
object EvacuatingMutator:

    def transit(
        @unused config: Config,
        state: Utxos,
        l2Tx: L2Tx
    ): Utxos =
        val l1UtxosToRemove = l2Tx.l1utxos.map(_._1).toSet
        // TODO: check all evacuatees exist?
        state.removedAll(l1UtxosToRemove)
