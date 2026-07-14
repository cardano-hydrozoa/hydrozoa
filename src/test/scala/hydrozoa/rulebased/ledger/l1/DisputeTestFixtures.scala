package hydrozoa.rulebased.ledger.l1

import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.rulebased.ledger.l1.script.plutus.{DeploymentTx, SetupLadder}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedRegimeUtxo, RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.v3.PosixTime
import scalus.uplc.builtin.Data.toData

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
      * voting deadline.
      */
    def mkRuleBasedTreasuryPure(
        headConfig: HeadConfig,
        versionMajor: BigInt,
        value: Value,
        txIn: TransactionInput,
        votingDeadline: PosixTime
    ): RuleBasedTreasuryUtxo = {
        val datum = Unresolved(
          headMp = headConfig.headMultisigScript.policyId,
          deadlineVoting = votingDeadline,
          versionMajor = versionMajor
        )
        RuleBasedTreasuryUtxo(
          utxoId = txIn,
          treasuryOutput = RuleBasedTreasuryOutput(datum, value)
        )
    }

    /** The rule-based regime utxo (HRWT beacon + head-identity datum) as the FallbackTx would
      * produce it, for use as a reference input in dispute/treasury tests. Resolve its output via
      * `toUtxo` with the head config in scope.
      */
    def mkRegimeUtxoPure(txIn: TransactionInput): RuleBasedRegimeUtxo =
        RuleBasedRegimeUtxo(txIn)

    /** The setup-ladder rung utxo covering `k` evacuations: an inline `List[ByteString]` datum at
      * the burn address, as the ladder deployment would produce it.
      */
    def mkSetupRungUtxoPure(
        headConfig: HeadConfig,
        k: Int,
        txIn: TransactionInput
    ): Utxo = {
        val rung = SetupLadder.rungForEvacuations(k).toOption.get
        Utxo(
          txIn,
          Babbage(
            address = DeploymentTx.mkBurnAddress(headConfig.network),
            value = Value.zero,
            datumOption = Some(Inline(SetupLadder.rungDatum(rung))),
            scriptRef = None
          ).ensureMinAda(headConfig)
        )
    }
}
