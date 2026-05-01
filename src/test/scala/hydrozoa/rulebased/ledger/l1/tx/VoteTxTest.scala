package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.node
import hydrozoa.config.node.NodeConfig
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
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

lazy val gens =
    // same OnChain needed in headersList and OnChain block header
    const[Onchain] +:
    // same tx hash in genRuleBasedTreasuryUtxo and genTransactionInput (used by voteUtxo)
    const[TransactionHash] +:
    const[VersionMajor] +:
    // use the same multi-node config for the whole test
    const[MultiNodeConfig] +:
    gen[VoteTx.Build] +:
    gen(headersList) +:
    gen(voteUtxo) +:
    gen(genTransactionInput) +:
    gen(genRuleBasedTreasuryUtxo) +:
    gen(genCollateralUtxo) +:
    gen(addressKeyHash) +:
    gen(genPeerVoteDatumAwaitingVote) +:
    gen((_: NodeConfig).headConfig.headPeers) +:
    gen((_: MultiNodeConfig).nodeConfigs.head._2) +:
    gen(MultiNodeConfig.generateDefault) +:
    CommonGenerators.gens

def addressKeyHash(voteDatum: VoteDatum, voteTxConfig: VoteTx.Config) =
    val peerAddresses =
        voteTxConfig.headPeerVKeys.map(_.shelleyAddress()(using voteTxConfig))
    peerAddresses
        .toList(voteDatum.key.intValue - 1)
        .keyHashOption
        .get
        .asInstanceOf[AddrKeyHash]

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

def genTransactionInput(fallbackTxId: TransactionHash, config: VoteTx.Config):Gen[TransactionInput] =
    Gen.choose(1, config.nHeadPeers.toInt).map(outputIx => TransactionInput(fallbackTxId, outputIx))

// TODO: Determine what *Config.Section this should take
def voteUtxo(
    transactionInput: TransactionInput,
    voteDatum: VoteDatum,
    config: VoteTx.Config): VoteUtxo[VoteStatus.AwaitingVote] =
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

    val _ = property("Vote Tx") = runDefault(
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
    )

}
