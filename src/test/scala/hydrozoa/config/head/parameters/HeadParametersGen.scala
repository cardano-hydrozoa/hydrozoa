package hydrozoa.config.head.parameters

import cats.data.*
import hydrozoa.config.head.multisig.fallback.{FallbackContingency, generateFallbackContingency}
import hydrozoa.config.head.multisig.settlement.{SettlementConfig, generateSettlementConfig}
import hydrozoa.config.head.multisig.timing.{TxTiming, generateDefaultTxTiming}
import hydrozoa.config.head.rulebased.dispute.{DisputeResolutionConfig, generateDisputeResolutionConfig}
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.ledger.Hash32
import test.{GenWithTestPeers, given}

def generateHeadParameters(
    generateTxTiming: GenWithTestPeers[TxTiming] = generateDefaultTxTiming,
    generateFallbackContingency: GenWithTestPeers[FallbackContingency] =
        generateFallbackContingency,
    generateDisputeResolutionConfig: GenWithTestPeers[DisputeResolutionConfig] =
        generateDisputeResolutionConfig,
    generateSettlementConfig: Gen[SettlementConfig] = generateSettlementConfig,
    generateL2ParamsHash: Gen[Hash32] = Arbitrary.arbitrary[Hash32]
): GenWithTestPeers[HeadParameters] = {
    for {
        txTiming <- generateTxTiming
        fallbackContingency <- generateFallbackContingency
        disputeResolutionConfig <- generateDisputeResolutionConfig
        settlementConfig <- ReaderT.liftF(generateSettlementConfig)
        l2ParamsHash <- ReaderT.liftF(generateL2ParamsHash)
    } yield HeadParameters(
      txTiming = txTiming,
      fallbackContingency = fallbackContingency.fallbackContingency,
      disputeResolutionConfig = disputeResolutionConfig,
      settlementConfig = settlementConfig,
      // TODO: Generate
      coilQuorum = 0,
      l2ParamsHash = l2ParamsHash
    )
}
