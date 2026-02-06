package hydrozoa.config
import hydrozoa.AddressL1
import hydrozoa.lib.cardano.value.coin.Coin
import spire.compat.numeric
import spire.math.Number.apply
import spire.math.{Rational, SafeLong, UByte}
import spire.syntax.literals.r

/** This represents the head's distributable liabilities due to shareholders (peers), see
  * [[PeerEquityShare]].
  */
case class EquityShares private (
    peerShares: Map[UByte, PeerEquityShare]
) {
    def totalFallbackDeposit: Coin =
        peerShares.values.map(_.fallbackDeposit).coinSum.unsafeToCoin

//    // TODO: remove unsafe Option.get
//    def shareWeights: Distribution.NormalizedWeights = Distribution.parseNormalizedWeights(
//      NonEmptyList.fromList(peerShares.iterator.toList).get,
//      _._2.equityShare
//    ).get
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
    ): Either[HeadConfig.Error, EquityShares] = {
        import collectiveContingency.*
        import individualContingency.*

        val sharesSum = shares.values.map(_._2).sum
        for {
            // Check sum(quity shares) = 1
            _ <- Either.cond(sharesSum === r"1", (), HeadConfig.Error.SharesMustSumToOne(sharesSum))

            collateralUtxo = collateralDeposit +~ voteTxFee
            voteUtxo = voteDeposit +~ tallyTxFee
            peerShares = shares.view
                .mapValues((address, share) => {
                    val collectiveContingencyShare =
                        (fallbackTxFee +~ defaultVoteDeposit) *~ share
                    val fallbackDeposit =
                        Coin.unsafeApply(
                          (collectiveContingencyShare +~ collateralUtxo +~ voteUtxo).underlying.ceil.toLong
                        )
                    PeerEquityShare(address, share, fallbackDeposit)
                })
                .toMap

        } yield new EquityShares(peerShares)
    }

    // ===================================
    // Distribution
    // ===================================

    case class Distribution(
        disbursements: Map[AddressL1, Coin],
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
                      Coin.unsafeApply((equity *~ v.equityShare).underlying.floor.toLong)
                    )
                )

            val equityPayoutsTotal =
                payoutsParts.iterator.map(_._2._2).toList.coinSum.unsafeToCoin

            val dust = (equity -~ equityPayoutsTotal).unsafeToCoin

            val payouts = payoutsParts
                .map((a, p) => a -> (p._1 +~ p._2).unsafeToCoin)
                .toMap

            Distribution(payouts, dust)
        }

    object RuleBasedRegimeDistribution:
        extension (self: EquityShares)
            def distribute(defaultVoteDeposit: Coin, voteDeposit: Coin)(
                treasuryCoin: Coin
            ): Distribution = {
                // TODO: check this calculation
                val equity = (treasuryCoin -~ defaultVoteDeposit -~ (voteDeposit *~ SafeLong(
                  self.peerShares.size
                ))).unsafeToCoin
                val payouts = self.peerShares.values.map(v =>
                    v.payoutAddress ->
                        Coin.unsafeApply(
                          ((equity +~ defaultVoteDeposit) *~ v.equityShare +~ voteDeposit).underlying.floor.toLong
                        )
                )
                val equityPayoutsTotal =
                    payouts.foldLeft(Coin.Unbounded.zero)((acc, p) => acc +~ p._2).unsafeToCoin
                val dust = (treasuryCoin -~ equityPayoutsTotal).unsafeToCoin
                Distribution(payouts.toMap, dust)
            }
