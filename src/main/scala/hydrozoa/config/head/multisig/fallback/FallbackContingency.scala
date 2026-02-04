package hydrozoa.config.head.multisig.fallback

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.number.{Distribution, PositiveInt}
import scalus.cardano.ledger.Coin

export FallbackContingency.totalFallbackContingency
export FallbackContingency.{distributeFallbackContingencyInMultisigRegime, distributeFallbackContingencyInRuleBasedRegime}
export FallbackContingency.{mkFallbackContingencyWithDefaults, mkCollectiveContingencyWithDefaults, mkIndividualContingencyWithDefaults}

final case class FallbackContingency(
    override val collectiveContingency: FallbackContingency.Collective,
    override val individualContingency: FallbackContingency.Individual,
) extends FallbackContingency.Section {
    override transparent inline def fallbackContingency: FallbackContingency = this
}

object FallbackContingency {
    final case class Collective(
        defaultVoteDeposit: Coin,
        fallbackTxFee: Coin,
    ) {
        lazy val total: Coin = defaultVoteDeposit + fallbackTxFee
    }

    final case class Individual(
        collateralDeposit: Coin,
        tallyTxFee: Coin,
        voteDeposit: Coin,
        voteTxFee: Coin,
    ) {
        lazy val collateralUtxo: Coin = collateralDeposit + tallyTxFee

        lazy val voteUtxo: Coin = voteDeposit + voteTxFee

        lazy val total: Coin = collateralUtxo + voteUtxo
    }

    trait Section {
        def fallbackContingency: FallbackContingency

        def collectiveContingency: FallbackContingency.Collective
        def individualContingency: FallbackContingency.Individual

    }

    extension (config: FallbackContingency.Section & HeadPeers.Section)
        def totalFallbackContingency: Coin = Coin(
          config.collectiveContingency.total.value +
              config.nHeadPeers.convert * config.individualContingency.total.value
        )

        def distributeFallbackContingencyInMultisigRegime: List[Coin] =
            distributeEvenlyToPeers(config.totalFallbackContingency)

        def distributeFallbackContingencyInRuleBasedRegime: List[Coin] =
            distributeEvenlyToPeers(
              config.collectiveContingency.defaultVoteDeposit +
                  config.individualContingency.voteDeposit
            )

        private def distributeEvenlyToPeers(amount: Coin): List[Coin] = {
            val evenWeights = Distribution.evenWeights(config.nHeadPeers).get
            val distSafeLong = evenWeights.distribute(amount.value)
            val distCoin = distSafeLong.iterator.map(_.toLong).map(Coin.apply).toList
            distCoin
        }

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
          collateralDeposit = collateralDeposit(tallyTxFee = tallyTxFee),
          tallyTxFee = tallyTxFee,
          voteDeposit = voteDeposit(voteTxFee = voteTxFee),
          voteTxFee = voteTxFee
        )

        private def fallbackTxFee: Coin = config.maxNonPlutusTxFee

        private def collateralDeposit(tallyTxFee: Coin): Coin = Coin(
          collateralUtxoMinLovelace.value.max(
            tallyTxFee.value * config.cardanoProtocolParams.collateralPercentage
          )
        )

        private def voteDeposit(voteTxFee: Coin): Coin =
            voteUtxoMinLovelace + Coin(
              voteTxFee.value * config.cardanoProtocolParams.collateralPercentage
            )

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
