package hydrozoa.multisig.ledger.eutxol2

import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger.Config
import hydrozoa.multisig.ledger.eutxol2.tx.L2Tx
import scala.annotation.unused
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{State as L1State, *}
import scalus.cardano.ledger.{CertState, Coin, TransactionException, Utxos}

object HydrozoaTransactionMutator {

    /** The upstream Scalus validators [[transit]] applies, in application order (alphabetical, for
      * ease of comparison in a file browser).
      *
      * One iterated list rather than a run of calls, because [[ruleNames]] derives the rule-list
      * element of `l2ParamsHash` from it (`docs/spec/head-params-hash.md`). Hashing the list this
      * runs is what keeps the digest and the ledger from drifting apart: dropping a validator here
      * moves the digest, so two peers on divergent builds cannot boot against the same head.
      *
      * FIXME/Note (Peter, 2025-07-22): I don't know if all of these will apply or if this list is
      * exhaustive, but I've removed the rules that I'm certain won't apply.
      */
    private[eutxol2] val upstreamValidators: Vector[Validator] = Vector(
      AllInputsMustBeInUtxoValidator,
      EmptyInputsValidator,
      InputsAndReferenceInputsDisjointValidator,
      MissingKeyHashesValidator,
      MissingOrExtraScriptHashesValidator,
      NativeScriptsValidator,
      OutputsHaveNotEnoughCoinsValidator,
      OutputsHaveTooBigValueStorageSizeValidator,
      OutsideValidityIntervalValidator,
      TransactionSizeValidator,
      ValueNotConservedUTxOValidator,
      VerifiedSignaturesInWitnessesValidator,
      ExactSetOfRedeemersValidator,
      ScriptsWellFormedValidator,
      ProtocolParamsViewHashesMatchValidator,
      WrongNetworkValidator,
      WrongNetworkInTxBodyValidator
    )

    /** Every rule [[transit]] applies, named, in application order — the rule-list element of
      * `l2ParamsHash`'s preimage (`docs/spec/head-params-hash.md`).
      *
      * The [[upstreamValidators]] names are read off that list, so they cannot disagree with what
      * runs. The rest are hydrozoa's own and are named by hand: they are not `Validator`s, so there
      * is nothing to read them from, and an edit here is only as reliable as the editor. That is
      * acceptable because their *semantics* are covered by the domain tag's version rather than by
      * their names — an upstream version pins upstream behaviour, and nothing but a deliberate bump
      * pins ours.
      */
    private[eutxol2] def ruleNames: Vector[String] =
        Vector("L2ConformanceValidator", "HeadIdPinValidator")
            ++ upstreamValidators.map(v => v.getClass.getSimpleName.stripSuffix("$"))
            ++ Vector(
              "ValueNotConservedUTxOValidator:main-projection",
              "PlutusScriptsTransactionMutator",
              "EvacuatingMutator"
            )

    private[eutxol2] object CardanoLedgerContext {

        /** Turn into an L1 context with zero fee and an empty CertState.
          *
          * The protocol parameters come from the head config's **L2** snapshot, not from the live
          * `CardanoNetwork` section the L1 transaction builders use. The two are the same value at
          * head initialization and diverge at the first hard fork: L1's must track the chain, and
          * this one must never move, because `l2ParamsHash` pins it for the head's life
          * (`docs/spec/head-params-hash.md`).
          */
        def fromConfig(
            config: Config,
            time: QuantizedInstant
        ): Context = {
            require(time.slotConfig == config.slotConfig)
            Context(
              fee = Coin(0),
              env = UtxoEnv(
                time.toSlot.slot,
                config.l2ProtocolParams,
                CertState.empty,
                config.network
              ),
              slotConfig = config.slotConfig
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

        val context = CardanoLedgerContext.fromConfig(config, time)
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
            // Cross-head-replay pin: the L2 tx must carry this head's headId (unless identity
            // isomorphism is on). Stateless — reclassified into screening in a later phase.
            _ <- HeadIdPinValidator.validate(config, l2Tx.headId)
            // The upstream validators, in the order [[upstreamValidators]] lists them. `flatMap`
            // short-circuits, so the first failure wins exactly as the call chain did.
            _ <- upstreamValidators.foldLeft[Either[String | TransactionException, Unit]](
              Right(())
            )((acc, validator) => acc.flatMap(_ => helper(validator)))
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

    /** Stateless auth pre-check for ledger screening: verify the L2 tx's vkey witnesses over its
      * id, reusing the exact validator [[transit]] runs at submission
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
              .fromConfig(config, QuantizedInstant.fromSlot(config.slotConfig, 0L)),
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
