package hydrozoa.config.head.multisig.fallback

import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.number.PositiveInt
import scalus.cardano.ledger.{Coin, ProtocolParams, Value}

final case class FallbackContingency(
    override val collectiveContingency: FallbackContingency.Collective,
    override val individualContingency: FallbackContingency.Individual,
) extends FallbackContingency.Section {
    override transparent inline def fallbackContingency: FallbackContingency = this
}

object FallbackContingency {
    def withDefaults(params: ProtocolParams)(
        tallyTxFee: Coin,
        voteTxFee: Coin
    ): FallbackContingency =
        FallbackContingency(
          Collective.withDefaults(params),
          Individual.withDefaults(params)(tallyTxFee = tallyTxFee, voteTxFee = voteTxFee)
        )

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

    final case class Collective(
        defaultVoteDeposit: Coin,
        fallbackTxFee: Coin,
    ) {
        lazy val totalCollectiveContingency: Coin = defaultVoteDeposit + fallbackTxFee
    }

    object Collective {
        def withDefaults(params: ProtocolParams): Collective =
            val default = Default(params)
            Collective(
              defaultVoteDeposit = default.voteUtxoMinLovelace,
              fallbackTxFee = default.fallbackTxFee
            )
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

    object Individual {
        def withDefaults(params: ProtocolParams)(tallyTxFee: Coin, voteTxFee: Coin): Individual =
            val default = Default(params)

            Individual(
              collateralDeposit = default.collateralDeposit(tallyTxFee = tallyTxFee),
              tallyTxFee = tallyTxFee,
              voteDeposit = default.voteDeposit(voteTxFee = voteTxFee),
              voteTxFee = voteTxFee
            )
    }

    final case class Default(params: ProtocolParams) {
        import Default.*
        import Assumptions.*

        def fallbackTxFee: Coin = maxNonPlutusTxFee(params)

        def collateralDeposit(tallyTxFee: Coin): Coin = Coin(
          collateralUtxoMinLovelace.value.max(tallyTxFee.value * params.collateralPercentage)
        )

        def voteDeposit(voteTxFee: Coin): Coin =
            voteUtxoMinLovelace + Coin(voteTxFee.value * params.collateralPercentage)

        def collateralUtxoMinLovelace: Coin =
            babbageUtxoMinLovelace(params, adaOnlyBaseAddressUtxoBytes)

        def voteUtxoMinLovelace: Coin =
            babbageUtxoMinLovelace(params, maxVoteUtxoBytes)
    }

    object Default {
        object Assumptions {
            // Serialized size of ADA-only utxo at the base address (with staking and payment credentials)
            val adaOnlyBaseAddressUtxoBytes: PositiveInt = PositiveInt.unsafeApply(67)

            // Max serialized size of a vote utxo (with/without vote)
            val maxVoteUtxoBytes: PositiveInt = PositiveInt.unsafeApply(150)
        }

        def babbageUtxoMinLovelace(
            params: ProtocolParams,
            serializedSize: PositiveInt
        ): Coin = Coin(
          (160 + serializedSize.convert) * params.utxoCostPerByte
        )

        def maxNonPlutusTxFee(params: ProtocolParams): Coin = Coin(
          params.txFeeFixed + params.maxTxSize * params.txFeePerByte
        )
    }
}
