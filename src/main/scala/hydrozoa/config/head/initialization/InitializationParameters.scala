package hydrozoa.config.head.initialization

import cats.data.NonEmptyMap
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.number.Distribution
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.dapp.token.CIP67.{HasTokenNames, HeadTokenNames}
import scala.collection.immutable.TreeMap
import scalus.builtin.{ByteString, platform}
import scalus.cardano.ledger.{Blake2b_256, Coin, Hash, Hash32, TransactionOutput, Utxo, Utxos, Value}
import spire.math.Rational

export InitializationParameters.isBalancedInitializationFunding

/** Configuration settings for the head's initialization.
  *
  * @param headStartTime
  *   TODO:
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
    override val initialEquityContributions: NonEmptyMap[HeadPeerNumber, Coin],
    override val initialSeedUtxo: Utxo,
    override val initialAdditionalFundingUtxos: Utxos,
    override val initialChangeOutputs: List[TransactionOutput],
) extends InitializationParameters.Section {
    override transparent inline def initializationParams: InitializationParameters = this

    override lazy val initialL2Value: Value =
        initialL2Utxos.values.map(_.value).fold(Value.zero)(_ + _)

    override lazy val initialEquityContributed: Coin =
        initialEquityContributions.toSortedMap.values.fold(Coin.zero)(_ + _)

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
    trait Section extends HasTokenNames {
        def initializationParams: InitializationParameters

        def headStartTime: QuantizedInstant

        def initialL2Utxos: Utxos
        def initialEquityContributions: NonEmptyMap[HeadPeerNumber, Coin]
        def initialSeedUtxo: Utxo
        def initialAdditionalFundingUtxos: Utxos
        def initialChangeOutputs: List[TransactionOutput]

        def initialEquityContributed: Coin
        def initialFundingValue: Value
        def initialL2Value: Value

        def initialEquityContributionsHash: Hash32

        final def initialFundingUtxos: Utxos =
            initialAdditionalFundingUtxos + initialSeedUtxo.toTuple
    }

    extension (config: InitializationParameters.Section & HeadPeers.Section)
        def distributeEquity(equityLovelace: Coin): NonEmptyMap[HeadPeerNumber, Coin] =
            // TODO: Ensure that initial equity contributions are non-zero, so that we can get rid of Option.
            //  This should already hold in practice because the initialization tx fee cannot be paid
            //  if no one contributed any equity. We just need to convince the type system.
            val weights: Distribution.NormalizedWeights = Distribution.unsafeNormalizeWeights(
              config.initialEquityContributions.toNel.map(_._2.value),
              Rational.apply
            )

            val shares: Iterator[Coin] = weights
                .distribute(equityLovelace.value)
                .iterator
                .map(_.toLong)
                .map(Coin.apply)

            NonEmptyMap.fromMapUnsafe(
              TreeMap.from(config.initialEquityContributions.toSortedMap.keys.zip(shares))
            )

    extension (
        config: InitializationParameters.Section & FallbackContingency.Section & HeadPeers.Section
    )
        def isBalancedInitializationFunding: Boolean = {
            config.initialFundingValue ==
                config.initialL2Value +
                Value(config.initialEquityContributed + config.totalFallbackContingency)
        }

}
