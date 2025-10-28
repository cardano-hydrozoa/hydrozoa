package hydrozoa.config

import hydrozoa.AddressL1
import scalus.cardano.ledger.value.Coin
import spire.compat.integral
import spire.implicits.additiveSemigroupOps
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
            // Check sum(quity shares) = 1
            _ <- Either.cond(sharesSum === r"1", (), HeadConfigError.SharesMustSumToOne(sharesSum))

            collateralUtxo = collateralDeposit + voteTxFee
            voteUtxo = voteDeposit + tallyTxFee
            peerShares = shares.view
                .mapValues((address, share) => {
                    val collectiveContingencyShare =
                        (fallbackTxFee + defaultVoteDeposit).scaleFractional(share)
                    val fallbackDeposit =
                        (collectiveContingencyShare + collateralUtxo + voteUtxo).unsafeToCoin()
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

    /** Distribution of equity and deposits for multisig regime
      */
    object MultisigRegimeDistribution:
        extension (self: EquityShares)
            def distribute(equity: Coin): Distribution = {

                // (AddressL1, (fallbackDeposit, equity payouts))
                val payoutsParts: Iterable[(AddressL1, (Coin, Coin))] =
                    self.peerShares.values.map(v =>
                        v.payoutAddress -> (
                          v.fallbackDeposit,
                          equity
                              .scaleFractional(v.equityShare)
                              // TODO: RoundingMode.DOWN
                              .unsafeToCoin()
                        )
                    )

                val equityPayoutsTotal =
                    payoutsParts
                        .foldLeft(Coin.Unbounded.zero)((acc, p) => acc + p._2._2)
                        .unsafeToCoin

                val dust = (equity - equityPayoutsTotal).unsafeToCoin

                val payouts = payoutsParts
                    .map((a, p) => a -> (p._1 + p._2).unsafeToCoin)
                    .toMap

                Distribution(payouts, dust)
            }

    object RuleBasedRegimeDistribution:
        extension (self: EquityShares)
            def distribute(defaultVoteDeposit: Coin, voteDeposit: Coin)(
                equity: Coin
            ): Distribution = {
                val payouts = self.peerShares.values.map(v =>
                    v.payoutAddress ->
                        ((equity + defaultVoteDeposit).scaleFractional(v.equityShare) + voteDeposit)
                            // TODO: RoundingMode.DOWN
                            .unsafeToCoin()
                )
                val equityPayoutsTotal = payouts.foldLeft(Coin.Unbounded.zero)((acc, p) => acc + p._2).unsafeToCoin
                val dust = (equity - equityPayoutsTotal).unsafeToCoin
                Distribution(payouts.toMap, dust)
            }
