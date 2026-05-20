package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.Voted
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.{gens as _, *}
import hydrozoa.rulebased.ledger.l1.tx.CommonGeneratorsTypes.{KzgCommitment, VersionMinor}
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryUtxo, VoteOutput, VoteUtxo}
import org.scalacheck.{Arbitrary, Gen, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.uplc.builtin.Builtins.blake2b_224
import registry.scalacheck.*

private lazy val resolutionGens =
    // entry point: forAll[ResolutionTx.Build] looks up Gen[ResolutionTx.Build] in the registry
    gen[ResolutionTx.Build] +:
        // override the structural Gen[RuleBasedTreasuryUtxo] with the bounded version that
        // attaches the head beacon token (without it the Plutus validator rejects the tx)
        gen(genRuleBasedTreasuryUtxo) +:
        // wire VoteOutput[Voted] + TransactionInput into VoteUtxo[Voted]; needed because the
        // phantom type tag isn't picked up by the registry's default structural derivation
        gen[VoteUtxo[Voted]] +:
        // build VoteOutput[Voted] from the VoteDatum so output.key/link match datum.key/link
        // (the validator rejects mismatched values)
        gen(voteOutput) +:
        // produce VoteDatum with the Voted status — the registry can't pick a VoteStatus variant
        // structurally, and the resolution tx needs a tallied (Voted) vote
        gen(genTalliedVoteDatum) +:
        gen[VoteStatus.Voted] +:
        CommonGenerators.gens

def genTalliedVoteDatum(voteStatus: VoteStatus.Voted): VoteDatum =
    VoteDatum(
      key = 1, // First peer voted
      link = 2, // Links to next peer
      voteStatus
    )

def voteOutput(
    config: HeadPeers.Section & HasTokenNames & CardanoNetwork.Section,
    voteDatum: VoteDatum,
    voteStatus: VoteStatus.Voted,
): VoteOutput[Voted] =
    VoteOutput(
      key = voteDatum.key,
      link = voteDatum.link,
      coin = Coin(10_000_000),
      voteTokens = PositiveInt.unsafeApply(config.nHeadPeers + 1),
      status = voteStatus
    )

object ResolutionTxTest extends Properties("Resolution Tx Test") {
    import MultiNodeConfig.*

    val _ = property("Resolution Builder generator works") = runDefault(
      for {
          builder <- forAll[ResolutionTx.Build](resolutionGens)
          tx <- failLeft(builder.result)
          // Basic smoke test assertions
          _ <- assertWith(tx.talliedVoteUtxo != null, "Tallied vote UTXO should not be null")
          _ <- assertWith(
            tx.treasuryUnresolvedUtxoSpent != null,
            "Treasury unresolved UTXO spent should not be null"
          )
          _ <- assertWith(
            tx.treasuryResolvedUtxoProduced != null,
            "Treasury resolved UTXO produced should not be null"
          )
          _ <- assertWith(tx.tx != null, "Transaction should not be null")

          // Verify the spent treasury UTXO matches the recipe input
          _ <- assertWith(
            tx.treasuryUnresolvedUtxoSpent == builder.treasuryUtxo,
            "Spent treasury UTXO should match recipe input"
          )

          // Verify treasury state transition from Unresolved to Resolved
          _ <- assertWith(
            tx.treasuryUnresolvedUtxoSpent.treasuryOutput.datum.isInstanceOf[Unresolved],
            "Input treasury should be Unresolved"
          )
      } yield true
    )
}
