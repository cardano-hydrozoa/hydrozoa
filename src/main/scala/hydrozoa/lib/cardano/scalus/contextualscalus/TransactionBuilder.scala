package hydrozoa.lib.cardano.scalus.contextualscalus

import hydrozoa.config.head.network.CardanoNetwork
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{AddrKeyHash, EvaluatorMode, PlutusScriptEvaluator, TaggedSortedSet}
import scalus.cardano.txbuilder as scalusTx
import scalus.cardano.txbuilder.{DiffHandler, SomeBuildError}
import scalus.uplc.builtin.ByteString

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

        /** Reserve witness-set / fee space for `count` expected signatures whose individual key
          * hashes don't matter for sizing. Scalus sizes dummy signatures from
          * `expectedSigners.size`, so a native-multisig spend — whose witness carries no signers,
          * and whose real signer subset is chosen at aggregation — declares only its signer COUNT
          * here before [[finalizeContext]]. This inserts `count` distinct synthetic placeholder
          * hashes and does not add anything to the transaction body's `requiredSigners`.
          */
        def addExpectedSigners(count: Int): scalusTx.TransactionBuilder.Context = {
            val placeholders: Set[AddrKeyHash] = (0 until count).map { i =>
                val bytes = new Array[Byte](28)
                bytes(0) = (i >>> 24).toByte
                bytes(1) = (i >>> 16).toByte
                bytes(2) = (i >>> 8).toByte
                bytes(3) = i.toByte
                AddrKeyHash(ByteString.fromArray(bytes))
            }.toSet
            context.copy(expectedSigners = context.expectedSigners ++ placeholders)
        }

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
