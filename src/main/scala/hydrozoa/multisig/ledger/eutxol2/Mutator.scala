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

    def transit(
        config: Config,
        time: QuantizedInstant,
        state: Utxos,
        l2Tx: L2Tx
    ): Either[String | TransactionException, Utxos] = {

        // A helper for mapping the error type and applying arguments
        def helper(v: Validator): Either[String | TransactionException, Unit] =
            v.validate(
              CardanoLedgerContext.fromCardanoNetwork(config, time),
              L1State(utxos = state),
              l2Tx.tx
            )
        for
            _ <- L2ConformanceValidator.validate(config, state, l2Tx)
            // Cross-head-replay pin: the L2 tx must carry this head's headId (unless identity
            // isomorphism is on). Stateless — reclassified into screening in a later phase.
            _ <- HeadIdPinValidator.validate(config, l2Tx.tx)
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
                  scalus.cardano.ledger.rules.State(state),
                  l2Tx.tx
                )
            // Native mutators: removes the L1-marked outputs, leaving only L2 outputs
            state = EvacuatingMutator.transit(config, scalusState.utxos, l2Tx)
        yield state
    }

    /** Stateless auth pre-check for ledger screening: verify the L2 tx's vkey witnesses over its id,
      * reusing the exact validator [[transit]] runs at submission
      * ([[VerifiedSignaturesInWitnessesValidator]]). Signature verification reads neither the utxo
      * state nor the block time, so an empty state and a placeholder time are safe (and keep the
      * result byte-identical to the submission-time check).
      */
    def screenSignatures(
        config: Config,
        l2Tx: L2Tx
    ): Either[String | TransactionException, Unit] =
        VerifiedSignaturesInWitnessesValidator.validate(
          CardanoLedgerContext
              .fromCardanoNetwork(config, QuantizedInstant.fromSlot(config.slotConfig, 0L)),
          L1State(utxos = Map.empty),
          l2Tx.tx
        )
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
