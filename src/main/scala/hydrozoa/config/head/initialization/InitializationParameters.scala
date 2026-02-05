package hydrozoa.config.head.initialization

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.dapp.token.CIP67.HeadTokenNames
import scalus.builtin.{ByteString, platform}
import scalus.cardano.ledger.{Blake2b_256, Coin, Hash, Hash32, TransactionOutput, Utxo, Utxos, Value}

export InitializationParameters.isBalancedInitializationFunding

/** Configuration settings for the head's initialization.
  *
  * @param initialL2Utxos
  *   the utxos with which the head's L2 ledger should be populated upon initialization.
  * @param initialEquityContributions
  *   the ADA amounts (if any) that each peer contributed to the head's equity. The total ADA
  *   contributed must be sufficient for the initialization tx fee, and will also be used for all
  *   subsequent settlement, rollout, and finalization tx fees.
  * @param initialSeedUtxo
  *   among the utxos funding the head's initialization, this utxo's ID determines the head's token
  *   names.
  * @param initialAdditionalFundingUtxos
  *   the other funding utxos for initialization, additional to the seed utxo.
  * @param initialChangeOutputs
  *   change outputs that must contain all ADA and non-ADA assets from the funding utxos that are in
  *   excess of the unbalanced treasury value ([[initialEquityContributed]] + [[initialL2Value]]).
  */
final case class InitializationParameters(
    override val headStartTime: QuantizedInstant,
    override val initialL2Utxos: Utxos,
    override val initialEquityContributions: Map[HeadPeerNumber, Coin],
    override val initialSeedUtxo: Utxo,
    override val initialAdditionalFundingUtxos: Utxos,
    override val initialChangeOutputs: List[TransactionOutput],
) extends InitializationParameters.Section {
    override transparent inline def initializationParams: InitializationParameters = this

    override lazy val initialL2Value: Value =
        initialL2Utxos.values.map(_.value).fold(Value.zero)(_ + _)

    override lazy val initialEquityContributed: Coin =
        initialEquityContributions.values.fold(Coin.zero)(_ + _)

    override lazy val initialFundingValue: Value =
        initialFundingUtxos.values.map(_.value).fold(Value.zero)(_ + _) -
            initialChangeOutputs.map(_.value).fold(Value.zero)(_ + _)

    override lazy val headTokenNames: HeadTokenNames = HeadTokenNames(initialSeedUtxo.input)

    // TODO: We need this hash to put into the initialization tx's metadata,
    //  so that the equity contributions are pinned by something signed by all peers.
    override lazy val initialEquityContributionsHash: Hash32 = Hash[Blake2b_256, Any](
      platform.blake2b_256(ByteString.unsafeFromArray(???))
    )
}

object InitializationParameters {
    trait Section {
        def initializationParams: InitializationParameters

        def headStartTime: QuantizedInstant

        def initialL2Utxos: Utxos
        def initialEquityContributions: Map[HeadPeerNumber, Coin]
        def initialSeedUtxo: Utxo
        def initialAdditionalFundingUtxos: Utxos
        def initialChangeOutputs: List[TransactionOutput]

        def initialEquityContributed: Coin
        def initialFundingValue: Value
        def initialL2Value: Value
        def headTokenNames: HeadTokenNames

        def initialEquityContributionsHash: Hash32

        final def initialFundingUtxos: Utxos =
            initialAdditionalFundingUtxos + initialSeedUtxo.toTuple
    }

    extension (
        config: InitializationParameters.Section & FallbackContingency.Section & HeadPeers.Section
    )
        def isBalancedInitializationFunding: Boolean = {
            config.initialFundingValue ==
                config.initialL2Value +
                Value(config.initialEquityContributed + config.totalFallbackContingency)
        }

}
