package hydrozoa.rulebased.ledger.l1.tx

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.ShelleyAddressExtra
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.AwaitingVote
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.OwnVoteUtxo
import org.scalacheck.{Gen, Properties}
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData

/** key != 0
  *
  * @param peersVKs
  * @return
  */
def genPeerVoteDatum(peersVKs: NonEmptyList[VerificationKey]): Gen[VoteDatum] =
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

// TODO: Determine what *Config.Section this should take
def genVoteUtxo(
    config: HeadPeers.Section & HasTokenNames & CardanoNetwork.Section,
    fallbackTxId: TransactionHash,
    voteDatum: VoteDatum,
): Gen[OwnVoteUtxo] =
    for {
        outputIx <- Gen.choose(1, config.nHeadPeers.toInt)
        txId = TransactionInput(fallbackTxId, outputIx)
        spp = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash)
        scriptAddr = ShelleyAddress(config.network, spp, ShelleyDelegationPart.Null)

        voteTokenAssetName = config.headTokenNames.voteTokenName
        voteToken = Value.assets(
          Map(config.headMultisigScript.policyId -> Map(voteTokenAssetName -> 1))
        )

        voteOutput = Babbage(
          address = scriptAddr,
          // Sufficient ADA for minAda + vote/tallying fees
          value = Value(Coin(10_000_000L)) + voteToken,
          datumOption = Some(Inline(voteDatum.toData(using VoteState.given_ToData_VoteDatum))),
          scriptRef = None
        )
    } yield OwnVoteUtxo(
      AddrKeyHash(voteDatum.voteStatus.asInstanceOf[AwaitingVote].peer.hash),
      Utxo(txId, voteOutput)
    )

def genVoteTxBuilder(multiNodeConfig: MultiNodeConfig): Gen[VoteTx.Build] = {
    val config = multiNodeConfig.headConfig

    for {
        versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
        // Generate a treasury UTXO to use a reference input
        treasuryDatum <- genTreasuryUnresolvedDatum(
          config,
          versionMajor
        )
        fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)

        // This is 4 bytes shorter to accommodate CIP-67 prefixes
        // NB: we use the same token name _suffix_ for all head tokens so far, which is not the case in reality
        headTokenSuffix <- genByteStringOfN(28)

        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          config = config,
          fallbackTxId = fallbackTxId,
          treasuryDatum
        )

        // Generate a vote UTXO with NoVote status (input)
        voteDatum <- genPeerVoteDatum(config.headPeerVKeys)
        voteUtxo <- genVoteUtxo(
          config,
          fallbackTxId = fallbackTxId,
          voteDatum = voteDatum
        )

        // Generate an onchain block header and sign using peers' wallets
        blockHeader <- genOnchainBlockHeader(versionMajor)

        signatures = multiNodeConfig.multisignHeader(blockHeader)

        // Make vote details
        // TODO: simplify getting peers addresses
        peerAddresses = config.headPeerVKeys.map(vkey =>
            ShelleyAddressExtra.mkShelleyAddress(vkey, config.network)
        )
        collateralUtxo <- genCollateralUtxo(
          config,
          // FIXME Being lazy here, do this better
          peerAddresses
              .toList(voteDatum.key.intValue - 1)
              .keyHashOption
              .get
              .asInstanceOf[AddrKeyHash]
        )

        // Create builder context (not needed for Recipe anymore)
        allUtxos = Map(
          voteUtxo.utxo.input -> voteUtxo.utxo.output,
          treasuryUtxo.asTuple._1 -> treasuryUtxo.asTuple._2,
          collateralUtxo._1 -> collateralUtxo._2
        )

    } yield VoteTx.Build(multiNodeConfig.nodeConfigs.head._2)(
      _voteUtxo = voteUtxo,
      _treasuryUtxo = treasuryUtxo,
      _collateralUtxo = collateralUtxo,
      _blockHeader = blockHeader,
      _signatures = signatures.toList,
    )
}

object VoteTxTest extends Properties("Vote Tx Test") {
    import MultiNodeConfig.*

    val _ = property("Vote Tx") = runDefault(
      for {
          mnc <- ask
          builder <- pick(genVoteTxBuilder(mnc))
          tx <- failLeft(builder.result)
          // Verify VoteTx structure
          _ <- assertWith(
            tx.voteUtxoSpent == builder.voteUtxo,
            "Spent vote UTXO should match recipe input"
          )
          _ <- assertWith(tx.voteUtxoProduced != null, "Vote UTXO produced should not be null")
          _ <- assertWith(tx.tx != null, "Transaction should not be null")
      } yield true
    )

}
