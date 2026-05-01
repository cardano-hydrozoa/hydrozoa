package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.node.NodeConfig
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
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
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.{gens => _, *}
import hydrozoa.rulebased.ledger.l1.tx.CommonGeneratorsTypes.*
import hydrozoa.rulebased.ledger.l1.utxo.{VoteOutput, VoteUtxo}
import org.scalacheck.{Gen, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString
import registry.scalacheck.*

def gens(using config: MultiNodeConfig) =
    gen[VoteTx.Build] +:
    listOf[HeaderSignature] +:
    gen[HeaderSignature] +:
    gen[Onchain] +:
    gen((v: VoteUtxo[VoteStatus]) => v. asInstanceOf[VoteUtxo[VoteStatus.AwaitingVote]]) +:
    gen[VoteUtxo[VoteStatus]] +:
    gen[VoteOutput[VoteStatus]] +:
    gen[VoteStatus.AwaitingVote] +:
    gen(genPeerVoteDatumAwaitingVote) +:
    gen((c: HeadConfig) => c.headPeers) +:
    gen((c: NodeConfig) => c.headConfig) +:
    gen((c: MultiNodeConfig) => c.nodeConfigs.head._2) +:
    const[Version] +:
    gen(config) +:
    CommonGenerators.gens

/** key != 0
  *
  * @param peersVKs
  * @return
  */
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

// TODO: Determine what *Config.Section this should take
def genVoteUtxo(
    fallbackTxId: TransactionHash,
    voteDatum: VoteDatum,
)(using
    config: HeadPeers.Section & HasTokenNames & CardanoNetwork.Section
): Gen[VoteUtxo[VoteStatus]] =
    for {
        outputIx <- Gen.choose(1, config.nHeadPeers.toInt)
        txId = TransactionInput(fallbackTxId, outputIx)
        scriptAddr = HydrozoaBlueprint.mkDisputeAddress(config.network)

        voteTokenAssetName = config.headTokenNames.voteTokenName

        voteOutput = VoteOutput(
          key = voteDatum.key,
          link = voteDatum.link,
          coin = Coin.ada(10),
          voteTokens = PositiveInt.unsafeApply(1),
          status = voteDatum.voteStatus
        )

    } yield VoteUtxo(
      input = txId,
      voteOutput = voteOutput
    )

def genVoteTxBuilder(using multiNodeConfig: MultiNodeConfig): Gen[VoteTx.Build] = {
    given config: VoteTx.Config = multiNodeConfig.nodeConfigs.head._2

    for {
        // Generate a treasury UTXO to use a reference input
        versionMajor <- gens.make[Gen[VersionMajor]]
        treasuryDatum <- gens.make[Gen[Unresolved]]
        fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)

        // This is 4 bytes shorter to accommodate CIP-67 prefixes
        // NB: we use the same token name _suffix_ for all head tokens so far, which is not the case in reality
        headTokenSuffix <- genByteStringOfN(28)

        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          section = config,
          fallbackTxId = fallbackTxId,
          unresolvedDatum = treasuryDatum
        )

        // Generate a vote UTXO with NoVote status (input)
        voteDatum <- gens.make[Gen[VoteDatum]]
        voteUtxo <- genVoteUtxo(
          fallbackTxId = fallbackTxId,
          voteDatum = voteDatum
        ).map(_.asInstanceOf[VoteUtxo[AwaitingVote]])

        // Generate an onchain block header and sign using peers' wallets
        blockHeader <- genOnchainBlockHeader(versionMajor)

        signatures = multiNodeConfig.multisignHeader(blockHeader)

        // Make vote details
        // TODO: simplify getting peers addresses
        peerAddresses = config.headPeerVKeys.map(_.shelleyAddress()(using config))
        collateralUtxo <- genCollateralUtxo(
          // FIXME Being lazy here, do this better
          peerAddresses
              .toList(voteDatum.key.intValue - 1)
              .keyHashOption
              .get
              .asInstanceOf[AddrKeyHash]
        )

        // Create builder context (not needed for Recipe anymore)
        allUtxos = Map(
          voteUtxo.toUtxo.input -> voteUtxo.toUtxo.output,
          treasuryUtxo.toUtxo.toTuple._1 -> treasuryUtxo.toUtxo.toTuple._2,
          collateralUtxo._1 -> collateralUtxo._2
        )

    } yield VoteTx.Build(
      voteUtxo,
      treasuryUtxo,
      collateralUtxo,
      blockHeader,
      signatures.toList,
    )
}

object VoteTxTest extends Properties("Vote Tx Test") {
    import MultiNodeConfig.*

    val _ = property("Vote Tx") = runDefault(
      for {
          mnc <- ask
          _ <- {
              given MultiNodeConfig = mnc
              given VoteTx.Config = mnc.nodeConfigs.head._2
              for {
                  builder <- forAll[VoteTx.Build](gens)
                  tx <- failLeft(builder.result)
                  // Verify VoteTx structure
                  _ <- assertWith(
                    tx.voteUtxoSpent == builder.uncastVoteUtxo,
                    "Spent vote UTXO should match recipe input"
                  )
                  _ <- assertWith(
                    tx.voteUtxoProduced != null,
                    "Vote UTXO produced should not be null"
                  )
                  _ <- assertWith(tx.tx != null, "Transaction should not be null")
              } yield ()
          }
      } yield true
    )

}
