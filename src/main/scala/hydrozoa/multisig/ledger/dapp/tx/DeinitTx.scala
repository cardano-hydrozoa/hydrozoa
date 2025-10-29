package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.config.EquityShares
import hydrozoa.config.EquityShares.MultisigRegimeDistribution
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuildErrorOr, explain}
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import monocle.Focus.focus
import scala.Function.const
import scalus.cardano.ledger.value.Coin
import scalus.cardano.ledger.value.Coin.{ArithmeticError, Unbounded}
import scalus.cardano.ledger.{Coin as OldCoin, KeepRaw, Sized, Transaction, TransactionOutput, Value}
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilderStep.*
import spire.implicits.additiveGroupOps

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
case class DeinitTx(
    override val treasurySpent: TreasuryUtxo,
    override val tx: Transaction
) extends Tx,
      TreasuryUtxo.Spent

object DeinitTx:

    final case class Builder(
        treasuryToSpend: TreasuryUtxo,
        equityShares: EquityShares,
        config: Tx.Builder.Config
    ) {
        def postProcess(ctx: TransactionBuilder.Context): DeinitTx =
            DeinitTx(treasuryToSpend, ctx.transaction)

        private val steps = Steps(treasuryToSpend, equityShares, config)

        def build: BuildErrorOr[DeinitTx] = for {
            payouts <- steps.mkPayouts.left
                .map(s =>
                    // Use tx-specific errors
                    SomeBuildError.BalancingError.apply(
                      TxBalancingError.Failed(IllegalStateException(s))
                    )
                )
                .explain(const("Could not distribute equity"))

            ctx <- TransactionBuilder
                .build(
                  config.env.network,
                  steps.commonSteps ++ payouts
                )
                .explain(const("Could not build deinit transaction"))

            res <- ctx
                .finalizeContext(
                  config.env.protocolParams,
                  SharePayoutsDiffHandler.handler(treasuryToSpend, equityShares),
                  config.env.evaluator,
                  config.validators
                )
                .explain(const("Could not balance deinit transaction"))
        } yield postProcess(res)
    }

    private case class Steps(
        treasuryToSpend: TreasuryUtxo,
        equityShares: EquityShares,
        config: Tx.Builder.Config
    ) {

        def commonSteps: List[TransactionBuilderStep] =
            List(
              stepReferenceHNS,
              spendTreasury
            ) ++ burnHeadTokens

        private def stepReferenceHNS =
            ReferenceOutput(config.headNativeScriptReferenceInput)

        private def spendTreasury =
            Spend(treasuryToSpend.asUtxo, config.headNativeScript.witness)

        private def burnHeadTokens = {
            List(
              Mint(
                config.headNativeScript.policyId,
                treasuryToSpend.headTokenName,
                -1L,
                config.headNativeScript.witness
              )
              // TODO: burn witness token
            )
        }

        def mkPayouts: Either[String, List[TransactionBuilderStep]] = {
            // FIXME: remove later
            val treasuryVCoin = Coin.unsafeApply(treasuryToSpend.value.coin.value)

            val distribute = MultisigRegimeDistribution.distribute(equityShares)

            for {
                equity <- (treasuryVCoin - equityShares.totalFallbackDeposit).toCoin match
                    case Left(ArithmeticError.Underflow) =>
                        Left("residual treasury can't be less then total deposits")
                    case Right(equity) => Right(equity)

                distribution = distribute(equity)

                outputs = distribution.payouts
                    .map(p => Send(TransactionOutput.apply(p._1, Value.lovelace(p._2.underlying))))
                    .toList

                fee = Fee(OldCoin.apply(distribution.dust.underlying))

            } yield outputs :+ fee
        }
    }

    /** A handler that re-distributes diff amount over all shares. Outputs (and optionally fee) are
      * replaced.
      */
    object SharePayoutsDiffHandler:
        def handler(treasuryToSpend: TreasuryUtxo, equityShares: EquityShares): DiffHandler = (
            diff: Long,
            tx: Transaction
        ) => {

            // FIXME: remove later
            val treasuryVCoin = Unbounded.apply(treasuryToSpend.value.coin.value)

            val distribute = MultisigRegimeDistribution.distribute(equityShares)
            val diffCoin = Coin.unsafeApply(diff)

            for {
                equity <-
                    (treasuryVCoin - (equityShares.totalFallbackDeposit + diffCoin)).toCoin match
                        case Left(ArithmeticError.Underflow) =>
                            Left(
                              TxBalancingError.Failed(
                                IllegalStateException(
                                  "residual treasury can't be less then total deposits"
                                )
                              )
                            )
                        case Right(equity) => Right(equity)

                distribution = distribute(equity)

                outputs = distribution.payouts
                    .map(p => Sized(TransactionOutput.apply(p._1, Value.lovelace(p._2.underlying))))
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

end DeinitTx
