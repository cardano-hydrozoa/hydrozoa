package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.lib.number.PositiveInt
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.{gens as _, *}
import hydrozoa.rulebased.ledger.l1.tx.CommonGeneratorsTypes.genTreasuryUnresolvedDatum
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryUtxo, VoteOutput, VoteUtxo}
import org.scalacheck.{Gen, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import registry.scalacheck.*

private lazy val tallyGens =
    // The two vote utxos used by the builder must share the fallback tx hash, use distinct output indices,
    // and have compatible key/link values
    gen(genTallyTxBuilder) +:
        // Set the voting-deadline slot >= 0.
        // without this the treasury-datum parse step fails with "Slot number must be non-negative"
        gen(genTreasuryUnresolvedDatum) +:
        // The vote datums must be compatible for tallying 
        gen(genCompatibleVoteDatums) +:
        CommonGenerators.gens

/** Generate a pair of compatible vote datums for tallying — the continuing vote's `link` matches
  * the removed vote's `key`.
  */
def genCompatibleVoteDatums(headPeers: HeadPeers): Gen[(VoteDatum, VoteDatum)] =
    val peersN = headPeers.nHeadPeers.toInt
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

/** The two vote utxos must be coordinated:
 *   - compatible datums,
  *  - shared fallback tx id
 *   - distinct output indices
  */
def genTallyTxBuilder(
    config: NodeConfig,
    treasuryUtxo: RuleBasedTreasuryUtxo,
    collateralUtxo: CollateralUtxo,
    fallbackTxId: TransactionHash,
    voteDatums: (VoteDatum, VoteDatum),
): TallyTx.Build = {
    TallyTx.Build(
      continuingVoteUtxo = mkTallyVoteUtxo(fallbackTxId, 1, voteDatums._1),
      removedVoteUtxo = mkTallyVoteUtxo(fallbackTxId, 2, voteDatums._2),
      treasuryUtxo,
      collateralUtxo,
    )(using config)
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
