package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.Voted
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.{gens as _, *}
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryUtxo, VoteOutput, VoteUtxo}
import org.scalacheck.{Arbitrary, Gen, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.uplc.builtin.Builtins.blake2b_224
import registry.scalacheck.*

private lazy val resolutionGens =
    gen[ResolutionTx.Build] +:
        // override the structural Gen[RuleBasedTreasuryUtxo] with the bounded version that
        // attaches the head beacon token (without it the Plutus validator rejects the tx)
        gen(genRuleBasedTreasuryUtxo) +:
        gen(resolutionTallyVoteUtxo) +:
        gen(genTalliedVoteDatum) +:
        gen(addrKeyHash) +:
        const[TransactionHash] +:
        CommonGenerators.gens

/** Generate a tallied vote datum with Vote status for resolution testing
  */
def genTalliedVoteDatum: Gen[VoteDatum] =
    // First peer voted
    val key = 1
    // Links to next peer
    val link = 2
    for {
        versionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
        commitment <- genByteStringOfN(48) // KZG commitment
    } yield VoteDatum(
      key = key,
      link = link,
      voteStatus = VoteStatus.Voted(commitment, versionMinor)
    )

def resolutionTallyVoteUtxo(
    config: HeadPeers.Section & HasTokenNames & CardanoNetwork.Section,
    fallbackTxId: TransactionHash,
    voteDatum: VoteDatum,
): VoteUtxo[Voted] = {
    // Index 1 — index 0 is the treasury (see genRuleBasedTreasuryUtxo). Sharing the same
    // TransactionHash via const[TransactionHash] requires the indices to differ.
    val txId = TransactionInput(fallbackTxId, 1)
    val scriptAddr = HydrozoaBlueprint.mkDisputeAddress(config.network)

    val voteTokenAssetName = config.headTokenNames.voteTokenName
    val voteToken = Value.asset(
      policyId = config.headMultisigScript.policyId,
      assetName = voteTokenAssetName,
      amount = config.nHeadPeers.convert + 1
    )

    val voteOutput: VoteOutput[Voted] = VoteOutput(
      key = voteDatum.key,
      link = voteDatum.link,
      coin = Coin(10_000_000),
      voteTokens = PositiveInt.unsafeApply(config.nHeadPeers + 1),
      status = voteDatum.voteStatus.asInstanceOf[Voted]
    )

    VoteUtxo(input = txId, voteOutput)
}

def addrKeyHash(multiNodeConfig: MultiNodeConfig): AddrKeyHash =
    multiNodeConfig.addrKeyHashOf(HeadPeerNumber.zero)

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
