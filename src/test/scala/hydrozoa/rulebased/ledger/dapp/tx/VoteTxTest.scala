package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.utxo.VoteUtxo
import hydrozoa.multisig.ledger.virtual.commitment.TrustedSetup
import hydrozoa.rulebased.ledger.l1.dapp.utxo.RuleBasedTreasuryUtxo
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.{
    BlockTypeL2,
    OnchainBlockHeader,
    cip67DisputeTokenPrefix,
    given
}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
import hydrozoa.rulebased.ledger.l1.script.plutus.{
    DisputeResolutionScript,
    RuleBasedTreasuryScript,
    RuleBasedTreasuryValidator
}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.UnresolvedDatum
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteDetails, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.VoteTx
import org.scalacheck.{Arbitrary, Gen, Prop, Test as ScalaCheckTest}
import scalus.builtin.Builtins.serialiseData
import scalus.builtin.Data.toData
import scalus.builtin.{BLS12_381_G2_Element, ByteString}
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.api.v3.{PubKeyHash, TokenName}
import scalus.prelude.{List as SList, Option as SOption}
import scalus.|>
import test.*

def genHeadParams: Gen[
  (
      ScriptHash,
      TokenName,
      NonEmptyList[TestPeer],
      NonEmptyList[VerificationKeyBytes],
      ByteString,
      BigInt
  )
] =
    for {
        // TODO: use native script?
        nativeScriptHash <- genScriptHash
        // This is 4 bytes shorter to accommodate CIP-67 prefixes
        // NB: we use the same token name _suffix_ for all head tokens so far, which is not the case in reality
        headTokenSuffix <- genByteStringOfN(28)
        peers <- genTestPeers
        peersVKs = peers.map(_.wallet.exportVerificationKeyBytes)
        // L2 consensus parameters hash
        params <- genByteStringOfN(32)
        // Major version upon switching to the rule-based regime
        versionMajor <- Arbitrary.arbitrary[BigInt].map(_.abs + 1)
    } yield (nativeScriptHash, headTokenSuffix, peers, peersVKs, params, versionMajor)

def genTreasuryUnresolvedDatum(
    headMp: PolicyId,
    disputeId: TokenName,
    peersVKs: NonEmptyList[VerificationKeyBytes],
    params: ByteString,
    versionMajor: BigInt
): Gen[UnresolvedDatum] =
    for {
        deadlineVoting <- Arbitrary.arbitrary[BigInt].map(_.abs + 1000000)
        setup = TrustedSetup
            .takeSrsG2(10)
            .map(p2 => BLS12_381_G2_Element(p2).toCompressedByteString)
    } yield UnresolvedDatum(
      headMp = headMp,
      disputeId = disputeId,
      peers = SList.from(peersVKs.map(_.bytes).toList),
      peersN = BigInt(peersVKs.length),
      deadlineVoting = deadlineVoting,
      versionMajor = versionMajor,
      params = params,
      setup = setup
    )

def genRuleBasedTreasuryUtxo(
    headMp: PolicyId,
    beaconTokenName: TokenName,
    unresolvedDatum: UnresolvedDatum
): Gen[RuleBasedTreasuryUtxo] =
    for {
        txId <- genTxId
        spp = ShelleyPaymentPart.Script(RuleBasedTreasuryScript.compiledScriptHash)
        scriptAddr = ShelleyAddress(Mainnet, spp, ShelleyDelegationPart.Null)

        beaconTokenAssetName = AssetName(beaconTokenName)
        beaconToken = singleton(headMp, beaconTokenAssetName)
        adaAmount <- Arbitrary
            .arbitrary[Coin]
            .map(c => Coin(math.abs(c.value) + 1000000L)) // Ensure minimum ADA

    } yield RuleBasedTreasuryUtxo(
      beaconTokenName = beaconTokenAssetName,
      txId = txId,
      addr = scriptAddr,
      datum = Unresolved(unresolvedDatum),
      value = Value(adaAmount) + beaconToken
    )

def genVoteNoVoteDatum(peersVKs: NonEmptyList[VerificationKeyBytes]): Gen[VoteDatum] =
    for {
        key <- Gen.choose(0, peersVKs.length)
        link = (key + 1) % peersVKs.length + 1
        peer = if key == 0 then None else Some(peersVKs.toList(key).pubKeyHash)
    } yield VoteDatum(
      key = key,
      link = link,
      peer = peer.map(SOption.Some(_)).getOrElse(SOption.None),
      voteStatus = VoteStatus.NoVote
    )

def genVoteUtxo(headMp: PolicyId, voteTokenName: TokenName, voteDatum: VoteDatum): Gen[VoteUtxo] =
    for {
        txId <- genTxId
        spp = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash)
        scriptAddr = ShelleyAddress(Mainnet, spp, ShelleyDelegationPart.Null)

        voteTokenAssetName = AssetName(voteTokenName)
        voteToken = singleton(headMp, voteTokenAssetName)

        voteOutput = Babbage(
          address = scriptAddr,
          // Sufficient ADA for minUTxO + vote/tallying fees
          value = Value(Coin(10_000_000L)) + voteToken,
          datumOption = Some(Inline(voteDatum.toData)),
          scriptRef = None
        )
    } yield VoteUtxo(Utxo[L1](UtxoId[L1](txId), Output[L1](voteOutput)))

def genOnchainBlockHeader(versionMajor: BigInt): Gen[OnchainBlockHeader] =
    for {
        blockNum <- Arbitrary.arbitrary[BigInt].map(_.abs + 1) // Ensure positive
        timeCreation <- Arbitrary.arbitrary[BigInt].map(_.abs) // PosixTime as BigInt
        versionMinor <- Arbitrary.arbitrary[BigInt].map(_.abs)
        commitment <- genByteStringOfN(48) // KZG commitment (G1 compressed point)
    } yield OnchainBlockHeader(
      blockNum = blockNum,
      blockType = BlockTypeL2.Minor,
      timeCreation = timeCreation,
      versionMajor = versionMajor,
      versionMinor = versionMinor,
      commitment = commitment
    )

def signBlockHeader(
    blockHeader: OnchainBlockHeader,
    peers: NonEmptyList[TestPeer]
): List[Ed25519Signature] = {
    val bs = blockHeader.toData |> serialiseData |> (_.bytes) |> IArray.from
    peers.toList.map(peer => peer.wallet.createEd25519Signature(bs))
}

def mkVoteDetails(header: OnchainBlockHeader): VoteDetails =
    VoteDetails(
      commitment = header.commitment,
      versionMinor = header.versionMinor
    )

def genVoteTxRecipe(
    estimatedFee: Coin = Coin(5_000_000L)
): Gen[VoteTx.Recipe] =
    for {

        // Common head parameters
        (headScriptHash, headTokensSuffix, peers, peersVKs, paramsHash, versionMajor) <-
            genHeadParams

        // Generate a treasury UTXO to use a reference input
        beaconTokenName = cip67BeaconTokenPrefix.concat(headTokensSuffix)
        voteTokenName = cip67DisputeTokenPrefix.concat(headTokensSuffix)

        treasuryDatum <- genTreasuryUnresolvedDatum(
          headScriptHash,
          voteTokenName,
          peersVKs,
          paramsHash,
          versionMajor
        )
        treasuryUtxo <- genRuleBasedTreasuryUtxo(headScriptHash, beaconTokenName, treasuryDatum)

        // Generate a vote UTXO with NoVote status (input)
        voteDatum <- genVoteNoVoteDatum(peersVKs)
        voteUtxo <- genVoteUtxo(headScriptHash, voteTokenName, voteDatum)

        // Generate an onchain block header and sign using peers' wallets
        blockHeader <- genOnchainBlockHeader(versionMajor)
        signatures = signBlockHeader(blockHeader, peers)
        // Make vote details
        newVoteDetails = mkVoteDetails(blockHeader)

        // Create builder context
        allUtxos = Map(
          voteUtxo.utxo.input.untagged -> voteUtxo.utxo.output.untagged,
          treasuryUtxo.toUtxo._1 -> treasuryUtxo.toUtxo._2
        )
        context = unsignedTxBuilderContext(allUtxos)

    } yield VoteTx.Recipe(
      voteUtxo = voteUtxo,
      treasuryUtxo = treasuryUtxo,
      blockHeader = blockHeader,
      signatures = signatures,
      newVoteDetails = newVoteDetails,
      context = context
    )

class VoteTxTest extends munit.ScalaCheckSuite {
    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(10_000)
    }

    test("Recipe generator works") {
        val exampleRecipe = genVoteTxRecipe().sample.get
        println(exampleRecipe)
    }

    property("Vote tx builds")(
      Prop.forAll(genVoteTxRecipe()) { recipe =>
          VoteTx.build(recipe) match {
              case Left(e)   => throw RuntimeException(s"Build failed $e")
              case Right(tx) => println(HexUtil.encodeHexString(tx.tx.toCbor))
          }
      }
    )

}
