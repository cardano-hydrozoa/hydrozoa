package hydrozoa.config.head.parameters

import cats.data.*
import hydrozoa.config.head.multisig.block.{BlockConfig, generateBlockConfig}
import hydrozoa.config.head.multisig.fallback.{FallbackContingency, generateFallbackContingency}
import hydrozoa.config.head.multisig.settlement.{SettlementConfig, generateSettlementConfig}
import hydrozoa.config.head.multisig.timing.{TxTiming, generateDefaultTxTiming}
import hydrozoa.config.head.rulebased.dispute.{DisputeResolutionConfig, generateDisputeResolutionConfig}
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import org.scalacheck.Gen
import test.{GenWithTestPeers, TestPeers, given}

def generateHeadParameters(
    generateTxTiming: GenWithTestPeers[TxTiming] = generateDefaultTxTiming,
    generateFallbackContingency: GenWithTestPeers[FallbackContingency] =
        generateFallbackContingency,
    generateDisputeResolutionConfig: GenWithTestPeers[DisputeResolutionConfig] =
        generateDisputeResolutionConfig,
    generateSettlementConfig: Gen[SettlementConfig] = generateSettlementConfig,
    generateBlockConfig: Gen[BlockConfig] = generateBlockConfig,
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
        // The L2 parameters are the test network's, as bootstrap snapshots them. Both fields come
        // from the one value: the ledger reports a digest over what it validates against at every
        // `restoreTo` anchor and JointLedger checks the config against it, so a mismatched pair
        // here would fail every eutxo boot. Read from the environment rather than drawn — an extra
        // Gen draw shifts every seeded fixture in unrelated suites.
        testPeers <- ReaderT.ask[Gen, TestPeers]
        l2ProtocolParams = testPeers.cardanoNetwork.cardanoProtocolParams
        l2Ledger <- ReaderT.liftF(generateL2Ledger)
        identityIsomorphism <- ReaderT.liftF(generateIdentityIsomorphism)
    } yield HeadParameters(
      txTiming = txTiming,
      fallbackContingency = fallbackContingency.fallbackContingency,
      disputeResolutionConfig = disputeResolutionConfig,
      settlementConfig = settlementConfig,
      blockConfig = blockConfig,
      // TODO: Generate
      coilQuorum = 0,
      l2ProtocolParams = l2ProtocolParams,
      l2ParamsHash = EutxoL2Ledger.l2ParamsHash(l2ProtocolParams),
      l2Ledger = l2Ledger,
      identityIsomorphism = identityIsomorphism
    )
}
