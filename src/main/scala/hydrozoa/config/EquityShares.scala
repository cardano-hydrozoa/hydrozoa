package hydrozoa.config

import hydrozoa.AddressL1
import scalus.cardano.ledger.value.Coin
import scalus.cardano.ledger.value.Coin.Unbounded
import spire.compat.numeric
import spire.implicits.additiveSemigroupOps
import spire.math.Number.apply
import spire.math.{Rational, UByte}
import spire.syntax.literals.r

/** This represents the head's distributable liabilities due to shareholders (peers), see
  * [[PeerEquityShare]].
  */
case class EquityShares private (
    private val peerShares: Map[UByte, PeerEquityShare]
) {
    def totalFallbackDeposit: Coin =
        peerShares.values.map(_.fallbackDeposit).sumCoins.unsafeToCoin
}

/** Peers's equity share and deposit. */
case class PeerEquityShare(
    // Address to pay out liabilities
    payoutAddress: AddressL1,
    // Peer's share of equity
    equityShare: Rational,
    // Peer's total deposit, stored in multisig utxo, the collective part depends on the equityShare
    fallbackDeposit: Coin
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
                    PeerEquityShare(address, share, fallbackDeposit)
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

        /** @param equity
          *   total equity (not to be confused with residual treasury that contains the deposits
          *   after the finalization tx)
          * @return
          *   distribution of funds
          */
        def distribute(equityShares: EquityShares)(equity: Coin): Distribution = {

            // (AddressL1, (fallback deposit, equity share))
            val payoutsParts: Iterable[(AddressL1, (Coin, Coin))] =
                equityShares.peerShares.values.map(v =>
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
                    .foldLeft(Unbounded.zero)((acc, p) => acc + p._2._2)
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
                val equityPayoutsTotal =
                    payouts.foldLeft(Coin.Unbounded.zero)((acc, p) => acc + p._2).unsafeToCoin
                val dust = (equity - equityPayoutsTotal).unsafeToCoin
                Distribution(payouts.toMap, dust)
            }
