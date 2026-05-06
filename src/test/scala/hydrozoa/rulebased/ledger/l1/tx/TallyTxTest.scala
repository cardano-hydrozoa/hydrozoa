package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.{gens as _, *}
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryUtxo, VoteOutput, VoteUtxo}
import org.scalacheck.{Gen, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import registry.scalacheck.*

private lazy val tallyGens =
    gen(genTallyTxBuilder) +:
        // override CommonGenerators' structural Gen[RuleBasedTreasuryUtxo] with the bounded
        // version that attaches the head beacon token (the Plutus validator rejects the tx without it)
        gen(genRuleBasedTreasuryUtxo) +:
        gen(tallyAddrKeyHash) +:
        // pin one TransactionHash so the treasury (index 0) and both vote utxos (indices 1, 2)
        // share the same fallback tx id
        const[TransactionHash] +:
        CommonGenerators.gens

def tallyAddrKeyHash(multiNodeConfig: MultiNodeConfig): AddrKeyHash =
    multiNodeConfig.addrKeyHashOf(HeadPeerNumber.zero)

/** Generate a pair of compatible vote datums for tallying — the continuing vote's `link` matches
  * the removed vote's `key`.
  */
def genCompatibleVoteDatums(peersN: Int): Gen[(VoteDatum, VoteDatum)] =
    for {
        continuingKey <- Gen.choose(0, peersN - 1)
        removedKey = continuingKey + 1
        nextLink = (removedKey + 1) % (peersN + 1)
        continuingVersionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
        continuingCommitment <- genByteStringOfN(48)
        removedVersionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
        removedCommitment <- genByteStringOfN(48)
    } yield (
      VoteDatum(
        key = continuingKey,
        link = removedKey,
        voteStatus = VoteStatus.Voted(continuingCommitment, continuingVersionMinor),
      ),
      VoteDatum(
        key = removedKey,
        link = nextLink,
        voteStatus = VoteStatus.Voted(removedCommitment, removedVersionMinor),
      ),
    )

/** Build a TallyTx.Build recipe. The two vote utxos must be coordinated (compatible datums,
  * shared fallback tx id, distinct output indices), so structural derivation via `gen[TallyTx.Build]`
  * doesn't fit here — register this builder directly instead.
  */
def genTallyTxBuilder(
    multiNodeConfig: MultiNodeConfig,
    treasuryUtxo: RuleBasedTreasuryUtxo,
    collateralUtxo: CollateralUtxo,
    fallbackTxId: TransactionHash,
): Gen[TallyTx.Build] = {
    val nodeConfig = multiNodeConfig.nodeConfigs.head._2
    given NodeConfig = nodeConfig
    for {
        (continuingDatum, removedDatum) <- genCompatibleVoteDatums(nodeConfig.nHeadPeers.toInt)
    } yield TallyTx.Build(
      continuingVoteUtxo = mkTallyVoteUtxo(fallbackTxId, 1, continuingDatum),
      removedVoteUtxo = mkTallyVoteUtxo(fallbackTxId, 2, removedDatum),
      treasuryUtxo = treasuryUtxo,
      collateralUtxo = collateralUtxo,
    )
}

private def mkTallyVoteUtxo(
    fallbackTxId: TransactionHash,
    outputIndex: Int,
    voteDatum: VoteDatum,
): VoteUtxo[VoteStatus] =
    VoteUtxo(
      input = TransactionInput(fallbackTxId, outputIndex),
      voteOutput = VoteOutput(
        key = voteDatum.key,
        link = voteDatum.link,
        coin = Coin.ada(5),
        voteTokens = PositiveInt.unsafeApply(1),
        status = voteDatum.voteStatus,
      ),
    )

object TallyTxTest extends Properties("Tally Tx Test") {
    import MultiNodeConfig.*

    val _ = property("Tally Tx happy path") = runDefault(
      for {
          builder <- forAll[TallyTx.Build](tallyGens)
          _ <- failLeft(builder.result)
      } yield true
    )
}
