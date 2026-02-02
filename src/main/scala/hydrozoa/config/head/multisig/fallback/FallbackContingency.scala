package hydrozoa.config.head.multisig.fallback

import hydrozoa.config.head.network.CardanoNetworkConfig
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.number.PositiveInt
import scalus.cardano.ledger.{Coin, Value}

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
        lazy val totalCollectiveContingency: Coin = defaultVoteDeposit + fallbackTxFee
    }

    final case class Individual(
        collateralDeposit: Coin,
        tallyTxFee: Coin,
        voteDeposit: Coin,
        voteTxFee: Coin,
    ) {
        lazy val collateralUtxo: Coin = collateralDeposit + tallyTxFee

        lazy val voteUtxo: Coin = voteDeposit + voteTxFee

        lazy val totalIndividualContingency: Coin = collateralUtxo + voteUtxo
    }

    trait Section {
        def fallbackContingency: FallbackContingency

        def collectiveContingency: FallbackContingency.Collective
        def individualContingency: FallbackContingency.Individual
    }

    extension (config: FallbackContingency.Section & HeadPeers.Section)
        def totalFallbackContingency: Coin = Coin(
          config.collectiveContingency.totalCollectiveContingency.value +
              config.nPeers.convert * config.individualContingency.totalIndividualContingency.value
        )

        def multisigRegimeUtxoValue: Value = Value(config.totalFallbackContingency)

    extension (config: CardanoNetworkConfig.Section) {
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
          collateralUtxoMinLovelace.value.max(tallyTxFee.value * config.params.collateralPercentage)
        )

        private def voteDeposit(voteTxFee: Coin): Coin =
            voteUtxoMinLovelace + Coin(voteTxFee.value * config.params.collateralPercentage)

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
