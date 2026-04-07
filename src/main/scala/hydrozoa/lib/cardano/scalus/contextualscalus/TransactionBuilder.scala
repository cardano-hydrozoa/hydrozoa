package hydrozoa.lib.cardano.scalus.contextualscalus

import hydrozoa.config.head.network.CardanoNetwork
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{EvaluatorMode, PlutusScriptEvaluator}
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
    }
}
