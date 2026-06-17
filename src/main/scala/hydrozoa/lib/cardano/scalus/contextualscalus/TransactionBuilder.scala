package hydrozoa.lib.cardano.scalus.contextualscalus

import hydrozoa.config.head.network.CardanoNetwork
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{AddrKeyHash, EvaluatorMode, PlutusScriptEvaluator, TaggedSortedSet}
import scalus.cardano.txbuilder as scalusTx
import scalus.cardano.txbuilder.{DiffHandler, SomeBuildError}

/** Like [[scalus.cardano.txbuilder]], but specialized to hydrozoa
  */
object TransactionBuilder {
    def build(steps: Seq[scalusTx.TransactionBuilderStep])(using
        config: CardanoNetwork.Section
    ): Either[SomeBuildError, scalusTx.TransactionBuilder.Context] =
        scalusTx.TransactionBuilder.build(config.network, steps)

    extension (context: scalusTx.TransactionBuilder.Context) {
        def finalizeContext(diffHandler: DiffHandler, validators: Seq[Validator])(using
            config: CardanoNetwork.Section
        ): Either[SomeBuildError, scalusTx.TransactionBuilder.Context] =
            context.finalizeContext(
              protocolParams = config.cardanoProtocolParams,
              diffHandler = diffHandler,
              evaluator =
                  PlutusScriptEvaluator(config.cardanoInfo, EvaluatorMode.EvaluateAndComputeCost),
              validators = validators,
            )

        def empty(using config: CardanoNetwork.Section): scalusTx.TransactionBuilder.Context = {
            scalusTx.TransactionBuilder.Context.empty(config.network)
        }

        /** Register verification-key hashes the transaction is expected to be signed by, for
          * witness-set fee sizing only. Scalus sizes dummy signatures from `expectedSigners`, so
          * native-multisig spends — whose witnesses no longer carry signers — must declare their
          * signers here before [[finalizeContext]]. This does not add the keys to the transaction
          * body's `requiredSigners` (matching how scalus handled native-script
          * `additionalSigners`).
          */
        def addExpectedSigners(
            signers: Set[AddrKeyHash]
        ): scalusTx.TransactionBuilder.Context =
            context.copy(expectedSigners = context.expectedSigners ++ signers)

        /** Add verification-key hashes to BOTH the transaction body's `requiredSigners` (so they
          * appear in `txInfo.signatories` on-chain) and `expectedSigners` (witness-set fee sizing).
          * Plutus-script spends — ballot-box / treasury spends whose validators check
          * `tx.signatories` — must declare their signers here before [[finalizeContext]] (matching
          * how scalus handled Plutus-script `additionalSigners`). Equivalent to a
          * `RequireSignature` step applied post-build.
          */
        def addRequiredSigners(
            signers: Set[AddrKeyHash]
        ): scalusTx.TransactionBuilder.Context =
            context.copy(
              transaction = scalusTx.txRequiredSignersL.modify(existing =>
                  TaggedSortedSet(existing.toSet ++ signers)
              )(context.transaction),
              expectedSigners = context.expectedSigners ++ signers
            )
    }
}
