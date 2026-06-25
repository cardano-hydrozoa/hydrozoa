package hydrozoa.rulebased.ledger.l1

import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.HeadConfig
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.v3.PosixTime
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.bls12_381.G2Element

/** Shared pure fixtures for the rule-based dispute-resolution tests.
  *
  * Both the example tests (e.g. `DisputeResolutionScenarioTest`) and the ScalaCheck `Commands`
  * explorer (`DisputeResolutionCommandsTest`) need to build the same starting on-chain state — a
  * ballot box and an Unresolved rule-based treasury — deterministically, without any TestM/effect
  * context. These constructors live here so a single definition is shared rather than duplicated
  * per suite; the `MultiNodeConfigTestM` variants in `DisputeActorTestHelpers` delegate to them.
  */
object DisputeTestFixtures {

    /** A ballot-box utxo at the dispute-resolution address: `nVoteTokens` vote tokens under the
      * head multisig policy plus a fixed 5-ADA min-value, carrying a [[VoteDatum]] with the given
      * key/link/voteStatus.
      */
    def mkBallotBoxUtxoPure(
        headConfig: HeadConfig,
        key: BigInt,
        link: BigInt,
        voteStatus: VoteStatus,
        // Careful, these can't conflict!
        txIn: TransactionInput,
        nVoteTokens: BigInt = 1
    ): Utxo = {
        val disputeResAddress = HydrozoaBlueprint.mkDisputeAddress(headConfig.network)
        val ownVoteUtxoOutput = Babbage(
          address = disputeResAddress,
          value = Value.assets(
            lovelace = Coin.ada(5),
            assets = Map(
              (
                headConfig.headMultisigScript.policyId,
                Map((headConfig.headTokenNames.voteTokenName, nVoteTokens.toLong))
              )
            )
          ),
          datumOption = Some(
            Inline(toData(VoteDatum(key = key, link = link, voteStatus = voteStatus)))
          ),
          scriptRef = None
        )
        Utxo((txIn, ownVoteUtxoOutput))
    }

    /** An Unresolved rule-based treasury utxo holding `value`, with the given major version and
      * voting deadline, and the full trusted-setup G2 points needed for evacuation.
      */
    def mkRuleBasedTreasuryPure(
        versionMajor: BigInt,
        value: Value,
        txIn: TransactionInput,
        votingDeadline: PosixTime
    ): RuleBasedTreasuryUtxo = {
        val datum = Unresolved(
          deadlineVoting = votingDeadline,
          versionMajor = versionMajor,
          setupG2 = TrustedSetup
              .takeSrsG2(EvacuationTx.Assumptions.maxEvacuationsPerTx + 1)
              .map(p2 => G2Element(p2).toCompressedByteString)
        )
        RuleBasedTreasuryUtxo(
          utxoId = txIn,
          treasuryOutput = RuleBasedTreasuryOutput(datum, value)
        )
    }
}
