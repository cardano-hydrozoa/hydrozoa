package hydrozoa.config.head.parameters

import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import hydrozoa.lib.number.PositiveInt
import org.scalacheck.Gen

type GenHeadParams = CardanoNetwork => (
    TxTimingGen,
    FallbackContingencyGen,
    DisputeResolutionConfigGen,
    Gen[SettlementConfig]
) => Gen[HeadParameters]

def generateHeadParameters(cardanoNetwork: CardanoNetwork)(
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateSettlementConfig: Gen[SettlementConfig] =
        Gen.choose(1, 200).map(i => SettlementConfig(PositiveInt(i).get))
): Gen[HeadParameters] = for {
    txTiming <- generateTxTiming(cardanoNetwork.slotConfig)
    fallbackContingency <- generateFallbackContingency(cardanoNetwork)
    disputeResolutionConfig <- generateDisputeResolutionConfig(cardanoNetwork.slotConfig)
    settlementConfig <- generateSettlementConfig
} yield HeadParameters(
  txTiming = txTiming,
  fallbackContingency = fallbackContingency,
  disputeResolutionConfig = disputeResolutionConfig,
  settlementConfig = settlementConfig
)
