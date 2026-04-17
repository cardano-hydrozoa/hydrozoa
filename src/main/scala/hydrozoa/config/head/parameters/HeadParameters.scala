package hydrozoa.config.head.parameters

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import scalus.cardano.ledger.Hash32

final case class HeadParameters(
    override val txTiming: TxTiming,
    override val fallbackContingency: FallbackContingency,
    override val disputeResolutionConfig: DisputeResolutionConfig,
    override val settlementConfig: SettlementConfig
) extends HeadParameters.Section {
    override transparent inline def headParameters: HeadParameters = this
}

object HeadParameters {
    trait Section
        extends TxTiming.Section,
          FallbackContingency.Section,
          DisputeResolutionConfig.Section,
          SettlementConfig.Section {
        def headParameters: HeadParameters

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
