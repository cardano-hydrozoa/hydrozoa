package hydrozoa.config

import hydrozoa.AddressL1
import java.math.RoundingMode as JRoundingMode
import scala.math.BigDecimal.RoundingMode
import scalus.cardano.ledger.value.Coin
import spire.compat.integral
import spire.math.Number.apply
import spire.math.{Rational, UByte}
import spire.syntax.literals.r

case class PeerShare(
    payoutAddress: AddressL1,
    equityShare: Rational,
    fallbackDeposit: Coin
)

/** TODO: revise
  *
  * This represents the head's distributable liabilities due to shareholders (peers):
  *   - collateral deposits (used to provide a collateral utxos for peers upon switching to the
  *     rule-based regime)
  *   - vote deposits (used to fund vote utxos)
  *   - equity shares
  */
case class EquityShares private (
    private val peerShares: Map[UByte, PeerShare]
)

object EquityShares:

    def apply(
        shares: Map[UByte, (AddressL1, Rational)],
        collectiveContingency: CollectiveContingency,
        individualContingency: IndividualContingency
    ): Either[HeadConfigError, EquityShares] = {
        import collectiveContingency.*
    import individualContingency.*
        val sharesSum = shares.values.map(_._2).sum
        for {
            // Check quity shares
            _ <- Either.cond(
              sharesSum === r"1",
              (),
              HeadConfigError.SharesMustSumToOne(sharesSum)
            )

            collateralUtxo = collateralDeposit + voteTxFee
            voteUtxo = voteDeposit + tallyTxFee
            peerShares = shares.view
                .mapValues((address, share) => {
                    val collectiveContingencyShare = (fallbackTxFee + defaultVoteDeposit)
                        .scale(share.toBigDecimal(32, JRoundingMode.HALF_EVEN))
                    // TODO: magic unwrapping, unsafeToCoin
                    val fallbackDeposit = Coin
                        .Unbounded(
                          collectiveContingencyShare
                              .round(RoundingMode.UP) + collateralUtxo + voteUtxo
                        )
                        .unsafeToCoin
                    PeerShare(address, share, fallbackDeposit)
                })
                .toMap

        } yield new EquityShares(peerShares)
    }

    // ===================================
    // Distribution
    // ===================================

    case class Distribution(
        payouts: Map[AddressL1, Coin],
        dust: Dust // Round leftover after the distribution
    )

    type Dust = Coin

    object MultisigRegime:
        extension (self: EquityShares)
            def distribute(equity: Coin): Distribution = {
                val parts = self.peerShares.values.map(v =>
                    v.payoutAddress -> (v.fallbackDeposit, equity
                        .scale(v.equityShare.toBigDecimal(32, JRoundingMode.UP))
                        .round(RoundingMode.DOWN)
                        .unsafeToCoin)
                )
                val equityPayoutsTotal = parts.foldLeft(Coin.Unbounded.zero)((acc, part) =>
                    Coin.Unbounded(acc + part._2._2.toCoinUnbounded)
                )
                val payouts = parts
                    .map((a, parts) => a -> Coin.Unbounded(parts._1 + parts._2).unsafeToCoin)
                    .toMap
                val dust = Coin.Unbounded(equity.toCoinUnbounded - equityPayoutsTotal).unsafeToCoin
                Distribution(payouts, dust)
            }

    object RuleBasedRegime:
        extension (self: EquityShares)
            def distribute(defaultVoteDeposit: Coin, voteDeposit: Coin)(
                equity: Coin
            ): Distribution = {
                val payouts = self.peerShares.values.map(v =>
                    v.payoutAddress -> Coin
                        .Fractional(
                          voteDeposit.toCoinFractional +
                              (equity + defaultVoteDeposit).scale(
                                v.equityShare.toBigDecimal(32, JRoundingMode.UP)
                              )
                        )
                        .round(RoundingMode.DOWN)
                        .unsafeToCoin
                )
                val equityPayoutsTotal = payouts.foldLeft(Coin.Unbounded.zero)((acc, part) =>
                    Coin.Unbounded(acc + part._2.toCoinUnbounded)
                )
                val dust = Coin.Unbounded(equity.toCoinUnbounded - equityPayoutsTotal).unsafeToCoin
                Distribution(payouts.toMap, dust)
            }
