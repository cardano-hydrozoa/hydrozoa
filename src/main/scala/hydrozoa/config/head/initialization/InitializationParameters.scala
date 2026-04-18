package hydrozoa.config.head.initialization

import cats.data.NonEmptyMap
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.given
import hydrozoa.lib.number.Distribution
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.peer.HeadPeerNumber.given
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder, *}
import scala.collection.immutable.TreeMap
import scalus.cardano.ledger.{AssetName, Blake2b_256, Coin, Hash, Hash32, TransactionOutput, Utxo, Utxos, Value}
import scalus.uplc.builtin.{ByteString, platform}
import spire.math.Rational

export hydrozoa.config.head.initialization.InitializationParameters.isBalancedInitializationFunding

/** Configuration settings for the head's initialization.
  *
  * @param headStartTime
  *   TODO:
  * @param initialEvacuationMap
  *   the utxos with which the head's L2 ledger should be populated upon initialization.
  * @param initialEquityContributions
  *   the ADA amounts (if any) that each peer contributed to the head's equity. The total ADA
  *   contributed must be sufficient for the initialization tx fee, and will also be used for all
  *   subsequent settlement, rollout, and finalization tx fees.
  * @param seedUtxo
  *   among the utxos funding the head's initialization, this utxo's ID determines the head's token
  *   names.
  * @param additionalFundingUtxos
  *   the other funding utxos for initialization, additional to the seed utxo.
  * @param initialChangeOutputs
  *   change outputs that must contain all ADA and non-ADA assets from the funding utxos that are in
  *   excess of the unbalanced treasury value ([[initialEquityContributed]] + [[initialL2Value]]).
  */
final case class InitializationParameters(
    override val initialEvacuationMap: EvacuationMap,
    override val initialEquityContributions: NonEmptyMap[HeadPeerNumber, Coin],
    override val seedUtxo: Utxo,
    override val additionalFundingUtxos: Utxos,
    // TODO: just changeOutputs?
    override val initialChangeOutputs: List[TransactionOutput],
) extends InitializationParameters.Section {
    override transparent inline def initializationParameters: InitializationParameters = this
}

object InitializationParameters {
    given initializationParametersEncoder(using
        config: CardanoNetwork.Section
    ): Encoder[InitializationParameters] = Encoder.derived[InitializationParameters]

    given initializationParametersDecoder(using
        network: CardanoNetwork.Section
    ): Decoder[InitializationParameters] = deriveDecoder[InitializationParameters]

    trait Section extends HasTokenNames {
        def initializationParameters: InitializationParameters

        def initialEvacuationMap: EvacuationMap = initializationParameters.initialEvacuationMap
        def initialEquityContributions: NonEmptyMap[HeadPeerNumber, Coin] =
            initializationParameters.initialEquityContributions
        def additionalFundingUtxos: Utxos =
            initializationParameters.additionalFundingUtxos
        def initialChangeOutputs: List[TransactionOutput] =
            initializationParameters.initialChangeOutputs
        def seedUtxo: Utxo = initializationParameters.seedUtxo

        final def initialEquityContributed: Coin =
            initialEquityContributions.toSortedMap.values.fold(Coin.zero)(_ + _)
        final def headTokenNames = CIP67.HeadTokenNames(seedUtxo.input)

        final def headId: HeadId = HeadId(headTokenNames.treasuryTokenName)
        final def initialFundingValue: Value =
            initialFundingUtxos.values.map(_.value).fold(Value.zero)(_ + _) -
                initialChangeOutputs.map(_.value).fold(Value.zero)(_ + _)

        def initialL2Value: Value =
            Value.combine(initialEvacuationMap.outputs.map(_.utxo.value.value))

        final def initialEquityContributionsHash: Hash32 = Hash[Blake2b_256, Any](
          platform.blake2b_256(ByteString.unsafeFromArray(???))
        )

        final def initialFundingUtxos: Utxos =
            additionalFundingUtxos + seedUtxo.toTuple

        // FIXME: Must be positive
        final def initialSeedIx: Int =
            initialFundingUtxos.keys.toList.sorted.indexOf(seedUtxo.input)
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
            val ret = config.initialFundingValue ==
                config.initialL2Value +
                Value(config.initialEquityContributed + config.totalFallbackContingency)
            ret
        }

    opaque type HeadId = AssetName

    // TODO: Must do validation on CIP67 HYDR string for both apply and Decoder
    object HeadId:
        def apply(treasuryBeaconTokenName: AssetName): HeadId = treasuryBeaconTokenName

        // Circe codecs using hex encoding for underlying AssetName
        // TODO: shall we use CIP-0116 codec for AssetName?
        given Encoder[HeadId] =
            Encoder.encodeString.contramap((headId: HeadId) => headId.bytes.toHex)

        given Decoder[HeadId] =
            Decoder.decodeString.map(s => AssetName.fromHex(s))

        extension (self: HeadId) def toHex: String = self.bytes.toHex
}
