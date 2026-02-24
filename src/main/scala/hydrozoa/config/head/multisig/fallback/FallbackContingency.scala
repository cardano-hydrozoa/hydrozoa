package hydrozoa.config.head.multisig.fallback

import cats.data.NonEmptyMap
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import scalus.cardano.ledger.Coin
import spire.math.Rational

export FallbackContingency.totalFallbackContingency
export FallbackContingency.{mkFallbackContingencyWithDefaults, mkCollectiveContingencyWithDefaults, mkIndividualContingencyWithDefaults}

final case class FallbackContingency(
    override val collectiveContingency: FallbackContingency.Collective,
    override val individualContingency: FallbackContingency.Individual,
) extends FallbackContingency.Section {
    override transparent inline def fallbackContingency: FallbackContingency = this
}

object FallbackContingency {

    /** This amount is collected from the first peer, in addition to the first peer's
      * [[FallbackContingency.Individual]]. Technically, this is on behalf of the whole group, but
      * it's easier to just get it from the first peer and return back to that peer if/when
      * necessary.
      */
    final case class Collective(
        defaultVoteDeposit: Coin,
        fallbackTxFee: Coin,
    ) {
        lazy val total: Coin = defaultVoteDeposit + fallbackTxFee
    }

    /** This amount is collected from each peer in the initialization tx.
      * @param collateralDeposit
      *   the collateral for scripts that we put into the collateral utxo
      * @param tallyTxFee
      *   the allocation for the tally transaction fee
      * @param voteDeposit
      *   the min-ADA that we put into the vote utxo
      * @param voteTxFee
      *   the allocation for the vote transaction fee
      */
    final case class Individual(
        collateralDeposit: Coin,
        tallyTxFee: Coin,
        voteDeposit: Coin,
        voteTxFee: Coin,
    ) {

        /** The ADA that should be put into the collateral utxo, which includes both the collateral
          * for scripts and the tally tx fee.
          */
        lazy val forCollateralUtxo: Coin = collateralDeposit + tallyTxFee

        /** The ADA that should be put into the vote utxo, which includes both the min-ADA and the
          * vote tx fee.
          */
        lazy val forVoteUtxo: Coin = voteDeposit + voteTxFee

        lazy val total: Coin = forCollateralUtxo + forVoteUtxo
    }

    trait Section {
        def fallbackContingency: FallbackContingency

        def collectiveContingency: FallbackContingency.Collective
        def individualContingency: FallbackContingency.Individual

        final def totalContingencyFor(headPeerNumber: HeadPeerNumber): Coin =
            if headPeerNumber == HeadPeerNumber.zero
            then individualContingency.total + collectiveContingency.total
            else individualContingency.total

    }

    extension (config: FallbackContingency.Section & HeadPeers.Section)
        def totalFallbackContingency: Coin = Coin(
          config.collectiveContingency.total.value +
              config.nHeadPeers.convert * config.individualContingency.total.value
        )

    extension (config: CardanoNetwork.Section & FallbackContingency.Section & HeadPeers.Section)
        /** In the finalization tx, distribute the fallback contingency as follows:
          *   - Each peer gets the total individual contingency collected from that peer.
          *   - The first peer also gets the entire collective contingency share, minus the actual
          *     finalization fee.
          */
        def distributeFallbackContingencyInFinalization: NonEmptyMap[HeadPeerNumber, Coin] =
            config.headPeerNums
                .map((_, config.individualContingency.total))
                .toNem
                .updateWith(HeadPeerNumber.zero)(_ + config.collectiveContingency.total)

    extension (config: CardanoNetwork.Section) {
        def mkFallbackContingencyWithDefaults(
            tallyTxFee: Coin,
            voteTxFee: Coin
        ): FallbackContingency = FallbackContingency(
          mkCollectiveContingencyWithDefaults,
          mkIndividualContingencyWithDefaults(tallyTxFee = tallyTxFee, voteTxFee = voteTxFee)
        )

        def mkCollectiveContingencyWithDefaults: Collective = Collective(
          defaultVoteDeposit = voteUtxoMinLovelace,
          fallbackTxFee = fallbackTxFee
        )

        def mkIndividualContingencyWithDefaults(
            tallyTxFee: Coin,
            voteTxFee: Coin
        ): Individual = Individual(
          collateralDeposit = collateralDeposit(tallyTxFee = tallyTxFee, voteTxFee = voteTxFee),
          tallyTxFee = tallyTxFee,
          voteDeposit = voteUtxoMinLovelace,
          voteTxFee = voteTxFee
        )

        private def fallbackTxFee: Coin = config.maxNonPlutusTxFee

        private def collateralDeposit(tallyTxFee: Coin, voteTxFee: Coin): Coin = {
            import hydrozoa.lib.cardano.value.coin.Coin as HCoin
            import hydrozoa.lib.cardano.value.coin.Coin.coinMax

            val c1 = HCoin.unsafeApply(collateralUtxoMinLovelace.value)
            val c2 = HCoin.unsafeApply(tallyTxFee.value)
            val c3 = HCoin.unsafeApply(voteTxFee.value)

            val max = List(c1, c2, c3).coinMax
            val ret = max *~ Rational(config.cardanoProtocolParams.collateralPercentage, 100)

            Coin(ret.underlying.ceil.toLong)
        }

        private def collateralUtxoMinLovelace: Coin =
            config.babbageUtxoMinLovelace(Assumptions.adaOnlyBaseAddressUtxoBytes)

        private def voteUtxoMinLovelace: Coin =
            config.babbageUtxoMinLovelace(Assumptions.maxVoteUtxoBytes)
    }

    object Assumptions {
        // Serialized size of ADA-only utxo at the base address (with staking and payment credentials)
        val adaOnlyBaseAddressUtxoBytes: PositiveInt = PositiveInt.unsafeApply(67)

        // Max serialized size of a vote utxo (with/without vote)
        val maxVoteUtxoBytes: PositiveInt = PositiveInt.unsafeApply(150)
    }
}
