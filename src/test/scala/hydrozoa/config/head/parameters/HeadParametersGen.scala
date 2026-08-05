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
    generateL2ParamsHash: Gen[Hash32] = Arbitrary.arbitrary[Hash32],
    generateL2Ledger: Gen[L2LedgerKind] = Gen.const(L2LedgerKind.CardanoEutxo),
    // Default identity-isomorphism ON (headId pin NOT enforced) so generated L2 txs, which carry no
    // headId metadatum, are accepted. Pin-enforcing suites override this to `false`.
    generateIdentityIsomorphism: Gen[Boolean] = Gen.const(true)
): GenWithTestPeers[HeadParameters] = {
    for {
        txTiming <- generateTxTiming
        fallbackContingency <- generateFallbackContingency
        disputeResolutionConfig <- generateDisputeResolutionConfig
        settlementConfig <- ReaderT.liftF(generateSettlementConfig)
        l2ParamsHash <- ReaderT.liftF(generateL2ParamsHash)
        l2Ledger <- ReaderT.liftF(generateL2Ledger)
        identityIsomorphism <- ReaderT.liftF(generateIdentityIsomorphism)
    } yield HeadParameters(
      txTiming = txTiming,
      fallbackContingency = fallbackContingency.fallbackContingency,
      disputeResolutionConfig = disputeResolutionConfig,
      settlementConfig = settlementConfig,
      // TODO: Generate
      coilQuorum = 0,
      l2ParamsHash = l2ParamsHash,
      l2Ledger = l2Ledger,
      identityIsomorphism = identityIsomorphism
    )
}
