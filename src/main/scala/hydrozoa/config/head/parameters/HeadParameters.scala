package hydrozoa.config.head.parameters

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import scalus.cardano.ledger.Hash32

/** The parameters that peers agree upon to run the protocol. These parameters get hashed into the
  * treasury datum.
  */
final case class HeadParameters(
    override val txTiming: TxTiming,
    override val fallbackContingency: FallbackContingency,
    override val disputeResolutionConfig: DisputeResolutionConfig,
    override val settlementConfig: SettlementConfig,
    override val l2ParamsHash: Hash32
) extends HeadParameters.Section {
    override transparent inline def headParameters: HeadParameters = this
}

object HeadParameters {

    given headParametersEncoder: Encoder[HeadParameters] = deriveEncoder[HeadParameters]

    given headParametersDecoder(using CardanoNetwork.Section): Decoder[HeadParameters] =
        deriveDecoder[HeadParameters]

    trait Section
        extends TxTiming.Section,
          FallbackContingency.Section,
          DisputeResolutionConfig.Section,
          SettlementConfig.Section {
        def headParameters: HeadParameters

        /** A black-box, L2-specific blake2b-256 hash of the L2 parameters that the peers agree upon
          * during the negotiation phase.
          */
        def l2ParamsHash: Hash32 = headParameters.l2ParamsHash

        final def headParamsHash: Hash32 = ???

        def txTiming: TxTiming = headParameters.txTiming

        def fallbackContingency: FallbackContingency =
            headParameters.fallbackContingency

        def disputeResolutionConfig: DisputeResolutionConfig =
            headParameters.disputeResolutionConfig

        def settlementConfig: SettlementConfig =
            headParameters.settlementConfig
    }
}
