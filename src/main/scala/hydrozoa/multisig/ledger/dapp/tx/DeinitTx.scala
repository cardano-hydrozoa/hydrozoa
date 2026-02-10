package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.config.EquityShares
import hydrozoa.config.EquityShares.MultisigRegimeDistribution
import hydrozoa.lib.cardano.value.coin.Coin
import hydrozoa.lib.cardano.value.coin.Coin.Unbounded
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuildErrorOr, explain}
import hydrozoa.multisig.ledger.dapp.utxo.ResidualTreasuryUtxo
import monocle.Focus.focus
import monocle.{Focus, Lens}
import scala.Function.const
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.{CardanoInfo, Coin as OldCoin, KeepRaw, PlutusScriptEvaluator, Sized, Transaction, TransactionOutput, Value}
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*

/** The Deinit tx in the multisig regime acts mostly the same way the eponymous tx in the rule-based
  * regime:
  *   - It spends the empty treasury utxo combined with the multisig witness utxo (it's done by the
  *     finalization tx) which contains peers' deposits and the remaining head equity.
  *   - It disburse to every peer one output (the deposits guarantee that every disbursement is at
  *     least >= 2 * minAda):
  *     - Their collateral deposit
  *     - Their voting deposit
  *     - Their share of fallback deposits (according to equity shares)
  *     - Their share of equity
  *   - All those disbursements will always fit the single tx. This holds, because the number of
  *     peers is limited by the fallback tx that should be able to _simultaneously_ distribute
  *     collaterals and (N+1) vote utxos in one go, the 2x+1 number of outputs.
  *   - The fees are paid from the treasury, reducing the equity. To calculate the fees the outputs
  *     amounts recalculated after the fees are calculated using [[RedistributeDiffHandler]].
  */
final case class DeinitTx(
    override val residualTreasurySpent: ResidualTreasuryUtxo,
    override val tx: Transaction,
    override val txLens: Lens[DeinitTx, Transaction] = Focus[DeinitTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos
) extends Tx[DeinitTx],
      ResidualTreasuryUtxo.Spent

object DeinitTx:
    final case class Config(
        cardanoInfo: CardanoInfo,
        headMultisigScript: HeadMultisigScript,
        equityShares: EquityShares
    ) {
        def evaluator: PlutusScriptEvaluator =
            PlutusScriptEvaluator(cardanoInfo, EvaluateAndComputeCost)
    }

    final case class Builder(
        residualTreasuryToSpend: ResidualTreasuryUtxo,
        config: Config,
    ) {
        def postProcess(ctx: TransactionBuilder.Context): DeinitTx =
            DeinitTx(
              residualTreasurySpent = residualTreasuryToSpend,
              tx = ctx.transaction,
              resolvedUtxos = ctx.resolvedUtxos
            )

        private val steps = Steps(residualTreasuryToSpend)

        def build: BuildErrorOr[DeinitTx] = for {
            disbursements <- steps.mkDisbursements.left
                .map(s =>
                    // TODO: Allow errors other than SomeBuildError to be raised
                    SomeBuildError.BalancingError.apply(
                      context = TransactionBuilder.Context.empty(config.cardanoInfo.network),
                      e = TxBalancingError.Failed(IllegalStateException(s))
                    )
                )
                .explain(const("Could not distribute equity"))

            ctx <- TransactionBuilder
                .build(
                  config.cardanoInfo.network,
                  steps.commonSteps ++ disbursements
                )
                .explain(const("Could not build deinit transaction"))

            res <- ctx
                .finalizeContext(
                  config.cardanoInfo.protocolParams,
                  RedistributeDiffHandler.handler(residualTreasuryToSpend),
                  config.evaluator,
                  Tx.Validators.nonSigningValidators
                )
                .explain(const("Could not balance deinit transaction"))
        } yield postProcess(res)

        private case class Steps(
            residualTreasuryToSpend: ResidualTreasuryUtxo,
        ) {

            def commonSteps: List[TransactionBuilderStep] =
                spendTreasury +: burnHeadTokens

            private def spendTreasury = {
                Spend(residualTreasuryToSpend.asUtxo, config.headMultisigScript.witnessValue)
            }

            private def burnHeadTokens = {
                List(
                  residualTreasuryToSpend.treasuryTokenName,
                  residualTreasuryToSpend.multisigRegimeTokenName
                )
                    .map(
                      Mint(
                        config.headMultisigScript.policyId,
                        _,
                        -1L,
                        // TODO: switch back to witnessAttached after resolving https://github.com/scalus3/scalus/issues/207
                        config.headMultisigScript.witnessValue
                      )
                    )
            }

            def mkDisbursements: Either[String, List[TransactionBuilderStep]] = {
                // FIXME: remove later
                val treasuryNewCoin = Coin.unsafeApply(residualTreasuryToSpend.value.coin.value)

                val distribute = MultisigRegimeDistribution.distribute(config.equityShares)

                for {
                    equity <-
                        (treasuryNewCoin -~ config.equityShares.totalFallbackDeposit).toCoin.left
                            .map(_ =>
                                "residual treasury can't be less then total fallback deposits"
                            )

                    distribution = distribute(equity)

                    outputs = distribution.disbursements
                        .map(p =>
                            Send(TransactionOutput.apply(p._1, Value.lovelace(p._2.underlying)))
                        )
                        .toList

                    fee = Fee(OldCoin.apply(distribution.dust.underlying))

                } yield outputs :+ fee
            }
        }

        /** A handler that re-distributes diff amount over all shares. Outputs (and optionally fee)
          * are replaced.
          *
          * It re-distributes shares based on the feeCoin value, using the dust as extra fees.
          *
          * NB: diff = inputs - (outputs + fee),
          */
        private object RedistributeDiffHandler:

            def handler(
                residualTreasuryToSpend: ResidualTreasuryUtxo,
            ): DiffHandler = (
                diff: Value,
                tx: Transaction
            ) => {

                if diff == Value.zero then Right(tx)
                else
                    val distribute = MultisigRegimeDistribution.distribute(config.equityShares)

                    // FIXME: remove later
                    val treasuryVCoin = Unbounded.apply(residualTreasuryToSpend.value.coin.value)

                    val feeCoin = Unbounded.apply(tx.body.value.fee.value)

                    for {
                        // equityShares.totalFallbackDeposit is added by the distributor
                        equity <-
                            (treasuryVCoin -~ config.equityShares.totalFallbackDeposit -~ feeCoin).toCoin.left
                                .map(_ =>
                                    TxBalancingError.Failed(
                                      IllegalStateException(
                                        "residual treasury can't be less then total fallback deposits and deinit tx fees"
                                      )
                                    )
                                )

                        distribution = distribute(equity)

                        outputs = distribution.disbursements
                            .map(p =>
                                Sized(
                                  TransactionOutput.apply(p._1, Value.lovelace(p._2.underlying))
                                )
                            )
                            .toList

                    } yield {
                        val tb = tx.body.value
                            .focus(_.outputs)
                            .replace(IndexedSeq.from(outputs))
                            .focus(_.fee)
                            .modify(fee => OldCoin.apply(fee.value + distribution.dust.underlying))
                        val t = tx.copy(body = KeepRaw(tb))
                        t
                    }

            }
    }

end DeinitTx
