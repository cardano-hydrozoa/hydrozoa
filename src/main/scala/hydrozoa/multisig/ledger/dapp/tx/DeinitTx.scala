package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.config.EquityShares
import hydrozoa.config.EquityShares.MultisigRegimeDistribution
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.value.coin.Coin
import hydrozoa.lib.cardano.value.coin.Coin.Unbounded
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuildErrorOr, explain}
import hydrozoa.multisig.ledger.dapp.utxo.ResidualTreasuryUtxo
import monocle.Focus.focus
import monocle.{Focus, Lens}
import scala.Function.const
import scalus.cardano.ledger.{Coin as OldCoin, KeepRaw, Sized, Transaction, TransactionOutput, Value}
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*

/** The Deinit tx in the multisig regime acts mostly the same way the eponymous tx in the rule-based
  * regime:
  *   - It spends the empty treasury utxo (combined with the multisig utxo by the finalization tx)
  *     that contains peers' deposits, and the remaining head equity.
  *   - It pays out to every peer as one output (the deposits guarantee that every payout is at
  *     least >= 2 * minAda):
  *     - Their collateral deposit
  *     - Their voting deposit
  *     - Their share of fallback deposits (according to equity shares)
  *     - Their share of equity
  *   - All those payouts will always fit the single tx. This holds, because the number of peers is
  *     limited by the fallback tx that should be able _simultaneously_ distribute collaterals and
  *     (N+1) vote utxos in one go, the 2x+1 number of outputs.
  *   - The fees are paid from the treasury, reducing the equity. To calculate the fees the outputs
  *     amounts recalculated after the fees are calculated.
  */
final case class DeinitTx(
    override val residualTreasurySpent: ResidualTreasuryUtxo,
    override val tx: Transaction,
    override val txLens: Lens[DeinitTx, Transaction] = Focus[DeinitTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos
) extends Tx[DeinitTx],
      ResidualTreasuryUtxo.Spent

object DeinitTx:
    type Config = CardanoNetwork.Section & HeadPeers.Section

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
            payouts <- steps.mkPayouts.left
                .map(s =>
                    // TODO: Allow errors other than SomeBuildError to be raised
                    SomeBuildError.BalancingError.apply(
                      context = TransactionBuilder.Context.empty(config.network),
                      e = TxBalancingError.Failed(IllegalStateException(s))
                    )
                )
                .explain(const("Could not distribute equity"))

            ctx <- TransactionBuilder
                .build(
                  config.network,
                  steps.commonSteps ++ payouts
                )
                .explain(const("Could not build deinit transaction"))

            res <- ctx
                .finalizeContext(
                  config.cardanoProtocolParams,
                  SharePayoutsDiffHandler.handler(residualTreasuryToSpend),
                  config.plutusScriptEvaluatorForTxBuild,
                  Tx.Validators.nonSigningValidators
                )
                .explain(const("Could not balance deinit transaction"))
        } yield postProcess(res)

        private case class Steps(
            residualTreasuryToSpend: ResidualTreasuryUtxo,
        ) {

            def commonSteps: List[TransactionBuilderStep] =
                spendTreasury +: burnHeadTokens

            private def spendTreasury =
                Spend(residualTreasuryToSpend.asUtxo, config.headMultisigScript.witnessValue)

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
                        config.headMultisigScript.witnessAttached
                      )
                    )
            }

            def mkPayouts: Either[String, List[TransactionBuilderStep]] = {
                // FIXME: remove later
                val treasuryNewCoin = Coin.unsafeApply(residualTreasuryToSpend.value.coin.value)

                val distribute =
                    MultisigRegimeDistribution.distribute(???) // FIXME config.equityShares

                for {
                    equity <- Right(???) // FIXME
//                        (treasuryNewCoin -~ config.equityShares.totalFallbackDeposit).toCoin.left
//                            .map(_ => "residual treasury can't be less then total deposits")

                    distribution = distribute(equity)

                    outputs = distribution.payouts
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
          * NB: diff is inputs - (outputs + fee)
          */
        private object SharePayoutsDiffHandler:

            def handler(
                residualTreasuryToSpend: ResidualTreasuryUtxo,
            ): DiffHandler = (
                diff: Value,
                tx: Transaction
            ) => {

                if diff == Value.zero then Right(tx)
                else
                    val distribute = MultisigRegimeDistribution.distribute(???) // FIXME

                    // FIXME: remove later
                    val treasuryVCoin = Unbounded.apply(residualTreasuryToSpend.value.coin.value)

                    val feeCoin = Unbounded.apply(tx.body.value.fee.value)

                    for {
                        // equityShares.totalFallbackDeposit is added by the distributor
                        equity <- Right(???) // FIXME
//                            (treasuryVCoin -~ config.equityShares.totalFallbackDeposit -~ feeCoin).toCoin.left
//                                .map(_ =>
//                                    TxBalancingError.Failed(
//                                      IllegalStateException(
//                                        "residual treasury can't be less then total deposits"
//                                      )
//                                    )
//                                )

                        distribution = distribute(equity)

                        outputs = distribution.payouts
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
