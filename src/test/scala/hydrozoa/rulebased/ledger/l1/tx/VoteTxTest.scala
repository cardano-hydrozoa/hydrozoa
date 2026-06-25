package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.AwaitingVote
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.{BallotBox, BallotBoxOutput}
import org.scalacheck.{Gen, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString

/** key != 0
  *
  * @param peersVKs
  * @return
  */
def genPeerVoteDatumAwaitingVote(using config: HeadPeers.Section): Gen[VoteDatum] = {
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
def genBallotBox(
    fallbackTxId: TransactionHash,
    voteDatum: VoteDatum,
)(using
    config: HeadPeers.Section & HasTokenNames & CardanoNetwork.Section
): Gen[BallotBox[VoteStatus]] =
    for {
        outputIx <- Gen.choose(1, config.nHeadPeers.toInt)
        txId = TransactionInput(fallbackTxId, outputIx)
        _ = HydrozoaBlueprint.mkDisputeAddress(config.network)

        _ = config.headTokenNames.voteTokenName

        ballotBoxOutput = BallotBoxOutput(
          key = voteDatum.key,
          link = voteDatum.link,
          coin = Coin.ada(10),
          voteTokens = PositiveInt.unsafeApply(1),
          status = voteDatum.voteStatus
        )

    } yield BallotBox(
      input = txId,
      ballotBoxOutput = ballotBoxOutput
    )

def genVoteTxBuilder(using multiNodeConfig: MultiNodeConfig): Gen[VoteTx.Build] = {
    given config: VoteTx.Config = multiNodeConfig.nodeConfigs.head._2

    for {
        versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
        // Generate a treasury UTXO to use a reference input
        treasuryDatum <- genTreasuryUnresolvedDatum(versionMajor)(using multiNodeConfig)
        fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)

        // This is 4 bytes shorter to accommodate CIP-67 prefixes
        // NB: we use the same token name _suffix_ for all head tokens so far, which is not the case in reality
        _ <- genByteStringOfN(28)

        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          fallbackTxId = fallbackTxId,
          treasuryDatum,
          ArbitraryInstances.given_Arbitrary_Value.arbitrary
        )

        // Generate a ballot box with AwaitingVote status (input)
        voteDatum <- genPeerVoteDatumAwaitingVote
        voteUtxo <- genBallotBox(
          fallbackTxId = fallbackTxId,
          voteDatum = voteDatum
        ).map(_.asInstanceOf[BallotBox[AwaitingVote]])

        // Generate an onchain block header and sign using peers' wallets
        blockHeader <- genOnchainBlockHeader(versionMajor)

        signatures = multiNodeConfig.multisignHeader(blockHeader)
        coilSignatures = multiNodeConfig.multisignHeaderCoil(blockHeader)

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
        _ = Map(
          voteUtxo.toUtxo.input -> voteUtxo.toUtxo.output,
          treasuryUtxo.toUtxo.toTuple._1 -> treasuryUtxo.toUtxo.toTuple._2,
          collateralUtxo._1 -> collateralUtxo._2
        )

    } yield VoteTx.Build(
      uncastBallotBox = voteUtxo,
      treasuryUtxo = treasuryUtxo,
      collateralUtxo = collateralUtxo,
      sec = blockHeader,
      signatures = signatures.toList,
      coilSignatures = coilSignatures,
    )
}

object VoteTxTest extends Properties("Vote Tx Test") {
    import MultiNodeConfig.*

    val _ = property("Vote Tx") = runWithCoil()(
      for {
          mnc <- ask
          _ <- {
              given MultiNodeConfig = mnc
              given VoteTx.Config = mnc.nodeConfigs.head._2
              for {
                  builder <- pick(genVoteTxBuilder)
                  tx <- failLeft(builder.result)
                  // Verify VoteTx structure
                  _ <- assertWith(
                    tx.ballotBoxSpent == builder.uncastBallotBox,
                    "Spent ballot box should match recipe input"
                  )
                  _ <- assertWith(
                    tx.ballotBoxProduced != null,
                    "Ballot box produced should not be null"
                  )
                  _ <- assertWith(tx.tx != null, "Transaction should not be null")
              } yield ()
          }
      } yield true
    )

}
