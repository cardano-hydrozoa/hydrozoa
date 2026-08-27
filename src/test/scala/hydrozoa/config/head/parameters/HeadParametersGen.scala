package hydrozoa.config.head.parameters

import cats.data.*
import hydrozoa.config.head.multisig.block.{BlockConfig, generateBlockConfig}
import hydrozoa.config.head.multisig.fallback.{FallbackContingency, generateFallbackContingency}
import hydrozoa.config.head.multisig.settlement.{SettlementConfig, generateSettlementConfig}
import hydrozoa.config.head.multisig.timing.{TxTiming, generateDefaultTxTiming}
import hydrozoa.config.head.rulebased.dispute.{DisputeResolutionConfig, generateDisputeResolutionConfig}
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import org.scalacheck.Gen
import scalus.cardano.ledger.Hash32
import test.{GenWithTestPeers, given}

def generateHeadParameters(
    generateTxTiming: GenWithTestPeers[TxTiming] = generateDefaultTxTiming,
    generateFallbackContingency: GenWithTestPeers[FallbackContingency] =
        generateFallbackContingency,
    generateDisputeResolutionConfig: GenWithTestPeers[DisputeResolutionConfig] =
        generateDisputeResolutionConfig,
    generateSettlementConfig: Gen[SettlementConfig] = generateSettlementConfig,
    generateBlockConfig: Gen[BlockConfig] = generateBlockConfig,
    // A plain value, deliberately not a Gen: an extra draw here shifts every seeded fixture
    // downstream and breaks unrelated suites.
    rateLimits: RateLimits = RateLimits.default,
    // The built-in ledger reports its own digest at every `restoreTo` anchor and JointLedger
    // checks the config against it, so a random value here would fail every eutxo boot.
    generateL2ParamsHash: Gen[Hash32] = Gen.const(EutxoL2Ledger.l2ParamsHash),
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
        blockConfig <- ReaderT.liftF(generateBlockConfig)
        l2ParamsHash <- ReaderT.liftF(generateL2ParamsHash)
        l2Ledger <- ReaderT.liftF(generateL2Ledger)
        identityIsomorphism <- ReaderT.liftF(generateIdentityIsomorphism)
    } yield HeadParameters(
      txTiming = txTiming,
      fallbackContingency = fallbackContingency.fallbackContingency,
      disputeResolutionConfig = disputeResolutionConfig,
      settlementConfig = settlementConfig,
      blockConfig = blockConfig,
      rateLimits = rateLimits,
      // TODO: Generate
      coilQuorum = 0,
      l2ParamsHash = l2ParamsHash,
      l2Ledger = l2Ledger,
      identityIsomorphism = identityIsomorphism
    )
}
