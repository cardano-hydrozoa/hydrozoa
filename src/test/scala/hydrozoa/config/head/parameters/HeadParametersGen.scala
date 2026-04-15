package hydrozoa.config.head.parameters

import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.settlement.{SettlementConfig, generateSettlementConfig}
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import org.scalacheck.Gen

type GenHeadParams = Gen[CardanoNetwork.Section ?=> HeadParameters]

def generateHeadParameters(
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateSettlementConfig: Gen[SettlementConfig] = generateSettlementConfig
): GenHeadParams = {
    for {
        txTiming <- generateTxTiming
        fallbackContingency <- generateFallbackContingency
        disputeResolutionConfig <- generateDisputeResolutionConfig
        settlementConfig <- generateSettlementConfig
    } yield (_ : CardanoNetwork.Section) ?=> HeadParameters(
        txTiming = txTiming,
        fallbackContingency = fallbackContingency,
        disputeResolutionConfig = disputeResolutionConfig,
        settlementConfig = settlementConfig
    )
}
