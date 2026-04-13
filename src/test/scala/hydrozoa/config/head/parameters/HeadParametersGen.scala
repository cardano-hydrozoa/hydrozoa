package hydrozoa.config.head.parameters

import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.settlement.{SettlementConfig, generateSettlementConfig}
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.ledger.Hash32

type GenHeadParams = CardanoNetwork => (
    TxTimingGen,
    FallbackContingencyGen,
    DisputeResolutionConfigGen,
    Gen[SettlementConfig],
    Gen[Hash32]
) => Gen[HeadParameters]

def generateHeadParameters(cardanoNetwork: CardanoNetwork)(
    generateTxTiming: TxTimingGen = generateDefaultTxTiming,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateDisputeResolutionConfig: DisputeResolutionConfigGen = generateDisputeResolutionConfig,
    generateSettlementConfig: Gen[SettlementConfig] = generateSettlementConfig,
    generateL2ParamsHash: Gen[Hash32] = Arbitrary.arbitrary[Hash32]
): Gen[HeadParameters] = for {
    txTiming <- generateTxTiming(cardanoNetwork.slotConfig)
    fallbackContingency <- generateFallbackContingency(cardanoNetwork)
    disputeResolutionConfig <- generateDisputeResolutionConfig(cardanoNetwork.slotConfig)
    settlementConfig <- generateSettlementConfig
    l2ParamsHash <- generateL2ParamsHash
} yield HeadParameters(
  txTiming = txTiming,
  fallbackContingency = fallbackContingency,
  disputeResolutionConfig = disputeResolutionConfig,
  settlementConfig = settlementConfig,
  l2ParamsHash = l2ParamsHash
)
