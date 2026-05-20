package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.node
import hydrozoa.config.node.NodeConfig
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.block.BlockHeader.Minor.{HeaderSignature, Onchain}
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.AwaitingVote
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.{gens as _, *}
import hydrozoa.rulebased.ledger.l1.tx.CommonGeneratorsTypes.*
import hydrozoa.rulebased.ledger.l1.utxo.{VoteOutput, VoteUtxo}
import org.scalacheck.{Gen, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString
import registry.scalacheck.*

private def voteGens =
    gen[VoteTx.Build] +:
        // share one Onchain block header within each generated VoteTx.Build so the same instance
        // is used by both `headersList` and the builder's own Onchain field (multisignature check
        // requires consistency); using `share` rather than `const` re-samples per iteration so it
        // doesn't conflict with the share-pinned VersionMajor below
        share[Onchain] +:
        // share one TransactionHash so the treasury's input and the vote utxo's input carry the
        // same fallback tx hash; the vote validator enforces this linkage
        share[TransactionHash] +:
        // share one VersionMajor so the treasury datum (Unresolved.versionMajor) and the on-chain
        // block header (BlockHeader.Onchain.versionMajor) carry the same value; the
        // dispute-resolution validator rejects the tx when they differ
        share[VersionMajor] +:
        // build the multisigned header list from a single Onchain header
        gen(headersList) +:
        // build VoteUtxo[AwaitingVote] from a coordinated TransactionInput + VoteDatum, with
        // matching key/link in the produced VoteOutput
        gen(voteUtxo) +:
        // constrain the vote utxo's output index to >= 1 (the treasury sits at index 0)
        gen(genTransactionInput) +:
        // set the head beacon token; the validator rejects the tx without it
        gen(genRuleBasedTreasuryUtxo) +:
        // set voting deadline slot to >= 0;
        gen(genTreasuryUnresolvedDatum) +:
        // produce a VoteDatum with `AwaitingVote` status and a key matching one of the
        // configured peers
        gen(genPeerVoteDatumAwaitingVote) +:
        CommonGenerators.gens

def headersList(header: Onchain, config: node.MultiNodeConfig) =
    config.multisignHeader(header).toList

def genPeerVoteDatumAwaitingVote(config: HeadPeers.Section): Gen[VoteDatum] = {
    val peersVKs = config.headPeerVKeys
    for {
        // key == 0 is the default `NoVote`, here we need a datum for OwnVoteUtxo
        key <- Gen.choose(1, peersVKs.length)
        link = (key + 1) % (peersVKs.length + 1)
        peer = PubKeyHash(blake2b_224(peersVKs.toList(key - 1)))
    } yield VoteDatum(
      key = key,
      link = link,
      voteStatus = VoteStatus.AwaitingVote(peer)
    )
}

def genTransactionInput(
    fallbackTxId: TransactionHash,
    config: VoteTx.Config
): Gen[TransactionInput] =
    Gen.choose(1, config.nHeadPeers.toInt).map(outputIx => TransactionInput(fallbackTxId, outputIx))

// TODO: Determine what *Config.Section this should take
def voteUtxo(
    transactionInput: TransactionInput,
    voteDatum: VoteDatum,
    config: VoteTx.Config
): VoteUtxo[VoteStatus.AwaitingVote] =
    VoteUtxo(
      input = transactionInput,
      voteOutput = VoteOutput(
        key = voteDatum.key,
        link = voteDatum.link,
        coin = Coin.ada(10),
        voteTokens = PositiveInt.unsafeApply(1),
        status = voteDatum.voteStatus.asInstanceOf[VoteStatus.AwaitingVote]
      )
    )

object VoteTxTest extends Properties("Vote Tx Test") {
    import MultiNodeConfig.*
    val _ = property("Vote Tx") = runDefault {
        // generate one multi-node config for each forAll
        val gens = voteGens.const[MultiNodeConfig]
        for {
            nc <- forAll[NodeConfig](gens)
            builder <- forAll[VoteTx.Build](gens)
            tx <- failLeft(builder.result(using nc))
            _ <- assertWith(
              tx.voteUtxoSpent == builder.uncastVoteUtxo,
              "Spent vote UTXO should match recipe input"
            )
            _ <- assertWith(
              tx.voteUtxoProduced != null,
              "Vote UTXO produced should not be null"
            )
            _ <- assertWith(tx.tx != null, "Transaction should not be null")
        } yield true
    }

}
