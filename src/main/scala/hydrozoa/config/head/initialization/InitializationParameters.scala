package hydrozoa.config.head.initialization

import cats.data.NonEmptyMap
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.number.Distribution
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.peer.HeadPeerNumber.given
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.token.CIP67.{HasTokenNames, HeadTokenNames}
import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder}
import scala.annotation.unused
import scala.collection.immutable.TreeMap
import scalus.cardano.ledger.{AssetName, Blake2b_256, Coin, Hash, Hash32, Value}
import scalus.uplc.builtin.{ByteString, platform}
import spire.math.Rational

/** Configuration settings for the head's initialization.
  *
  * @param initialEvacuationMap
  *   the head's opening L2 state as an evacuation map — the projection of the L2 ledger's state
  *   into its L1-compatible Cardano-utxo representation. General to any backend (for the EUTXO
  *   ledger it is the initial utxo set). It is an explicit, agreed field of the bootstrap config:
  *   the init tx commits to it on-chain (treasury value + datum KZG) and every node verifies that,
  *   so it cannot be derived-and-forgotten.
  * @param initialEquityContributions
  *   the ADA amounts (if any) that each peer contributed to the head's equity. The total ADA
  *   contributed must be sufficient for the initialization tx fee, and will also be used for all
  *   subsequent settlement, rollout, and finalization tx fees.
  * @param headId
  *   the head's on-chain identity (the treasury CIP-67 token name), presented explicitly in the
  *   config. Computed by the bootstrap author from the seed utxo; the running head reads it here
  *   and reconstructs the full [[HeadTokenNames]] from it, rather than re-deriving it from a seed.
  */
final case class InitializationParameters(
    override val initialEvacuationMap: EvacuationMap,
    override val initialEquityContributions: NonEmptyMap[HeadPeerNumber, Coin],
    override val headId: InitializationParameters.HeadId,
) extends InitializationParameters.Section {
    override transparent inline def initializationParameters: InitializationParameters = this
}

object InitializationParameters {
    given initializationParametersEncoder(using
        @unused config: CardanoNetwork.Section
    ): Encoder[InitializationParameters] = Encoder.derived[InitializationParameters]

    given initializationParametersDecoder(using
        network: CardanoNetwork.Section
    ): Decoder[InitializationParameters] = deriveDecoder[InitializationParameters]

    trait Section extends HasTokenNames {
        def initializationParameters: InitializationParameters

        def initialEvacuationMap: EvacuationMap = initializationParameters.initialEvacuationMap
        def initialEquityContributions: NonEmptyMap[HeadPeerNumber, Coin] =
            initializationParameters.initialEquityContributions
        def headId: HeadId = initializationParameters.headId

        /** Reconstruct the head's CIP-67 token names from its explicitly-stored head id. */
        final def headTokenNames: HeadTokenNames = HeadTokenNames.fromHeadId(headId)

        final def initialEquityContributed: Coin =
            initialEquityContributions.toSortedMap.values.fold(Coin.zero)(_ + _)

        def initialL2Value: Value =
            Value.combine(initialEvacuationMap.outputs.map(_.utxo.value.value))

        final def initialEquityContributionsHash: Hash32 = Hash[Blake2b_256, Any](
          platform.blake2b_256(ByteString.unsafeFromArray(???))
        )
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
