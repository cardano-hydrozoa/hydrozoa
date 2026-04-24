package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.{VoteOutput, VoteUtxo}
import org.scalacheck.{Gen, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString

/** Generate a vote datum with a cast vote for tally testing
  */
def genCastVoteDatum(
    key: Int,
    link: Int,
    versionMajor: BigInt
): Gen[VoteDatum] =
    for {
        versionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
        commitment <- genByteStringOfN(48) // KZG commitment
    } yield VoteDatum(
      key = key,
      link = link,
      voteStatus = VoteStatus.Voted(commitment, versionMinor)
    )

/** Generate a pair of compatible vote datums for tallying
  */
def genCompatibleVoteDatums(peersN: Int): Gen[(VoteDatum, VoteDatum)] =
    for {
        continuingKey <- Gen.choose(0, peersN - 1)
        removedKey = continuingKey + 1
        nextLink = (removedKey + 1) % (peersN + 1)

        // Generate independent commitments and versions for each vote
        continuingVersionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
        continuingCommitment <- genByteStringOfN(48)

        removedVersionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
        removedCommitment <- genByteStringOfN(48)

        continuingDatum = VoteDatum(
          key = continuingKey,
          link = removedKey, // Key constraint: continuing vote links to removed vote
          voteStatus = VoteStatus.Voted(continuingCommitment, continuingVersionMinor)
        )

        removedDatum = VoteDatum(
          key = removedKey,
          link = nextLink,
          voteStatus = VoteStatus.Voted(removedCommitment, removedVersionMinor)
        )
    } yield (continuingDatum, removedDatum)

def genTallyVoteUtxo(
    fallbackTxId: TransactionHash,
    outputIndex: Int,
    voteDatum: VoteDatum,
    voter: AddrKeyHash,
)(using
    config: CardanoNetwork.Section & HasTokenNames & HeadPeers.Section
): Gen[VoteUtxo[VoteStatus]] = {
    val txId = TransactionInput(fallbackTxId, outputIndex)
    val scriptAddr = HydrozoaBlueprint.mkDisputeAddress(config.network)

    val voteOutput = VoteOutput(
      key = voteDatum.key,
      link = voteDatum.link,
      coin = Coin.ada(5),
      voteTokens = PositiveInt.unsafeApply(1),
      status = voteDatum.voteStatus
    )

    Gen.const(
      VoteUtxo(txId, voteOutput)
    )
}

def genTallyTxBuilder(using multiNodeConfig: MultiNodeConfig): Gen[TallyTx.Build] =
    given config: NodeConfig = multiNodeConfig.nodeConfigs.head._2
    for {

        versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
        treasuryDatum <- gens.make[Gen[Unresolved]]

        fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          section = config,
          fallbackTxId = fallbackTxId,
          unresolvedDatum = treasuryDatum
        )

        // Generate compatible vote datums for tallying
        (continuingVoteDatum, removedVoteDatum) <- genCompatibleVoteDatums(config.nHeadPeers.toInt)

        // Generate a vote utxo with cast votes
        continuingVoteUtxo <- genTallyVoteUtxo(
          fallbackTxId,
          1, // Output index 1
          continuingVoteDatum,
          AddrKeyHash(blake2b_224(config.headPeers.headPeerVKeys.head)),
        )

        removedVoteUtxo <- genTallyVoteUtxo(
          fallbackTxId,
          2, // Output index 2
          removedVoteDatum,
          AddrKeyHash(blake2b_224(config.headPeers.headPeerVKeys.toList(1))),
        )

        collateralUtxo <- genCollateralUtxo(
          multiNodeConfig.addrKeyHashOf(HeadPeerNumber.zero)
        )

    } yield TallyTx.Build(
      continuingVoteUtxo = continuingVoteUtxo,
      removedVoteUtxo = removedVoteUtxo,
      treasuryUtxo = treasuryUtxo,
      collateralUtxo = collateralUtxo
    )

object TallyTxTest extends Properties("Tally Tx Test") {

    import MultiNodeConfig.*

    val _ = property("Tally Tx happy path") = runDefault(
      for {
          env <- ask
          _ <- {
              given MultiNodeConfig = env
              for {
                  builder <- pick(genTallyTxBuilder)
                  tx <- failLeft(builder.result)
              } yield ()
          }
      } yield true
    )
}
