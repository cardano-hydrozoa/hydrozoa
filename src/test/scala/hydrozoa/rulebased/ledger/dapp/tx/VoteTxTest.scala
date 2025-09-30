package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
import com.bloxbean.cardano.client.util.HexUtil
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.utxo.OwnVoteUtxo
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
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.VoteTx
import hydrozoa.rulebased.ledger.l1.tx.VoteTx.BuildError
import hydrozoa.rulebased.ledger.l1.tx.VoteTx.BuildError.SomeBalancingError
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

import java.time.Instant

def genHeadParams: Gen[
  (
      ScriptHash,
      TokenName,
      NonEmptyList[TestPeer],
      NonEmptyList[VerificationKeyBytes],
      ByteString,
      BigInt,
      TransactionHash
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
        versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
        // Fallback tx id - should be common for the vote utxo and treasury utxo
        fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
    } yield (nativeScriptHash, headTokenSuffix, peers, peersVKs, params, versionMajor, fallbackTxId)

def genTreasuryUnresolvedDatum(
    headMp: PolicyId,
    disputeId: TokenName,
    peersVKs: NonEmptyList[VerificationKeyBytes],
    params: ByteString,
    versionMajor: BigInt
): Gen[UnresolvedDatum] =
    for {
        deadlineVoting <- Gen
            .choose(600, 1800)
            .map(BigInt(_))
            .map(_.abs + Instant.now().getEpochSecond())
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
    fallbackTxId: TransactionHash,
    headMp: PolicyId,
    beaconTokenName: TokenName,
    unresolvedDatum: UnresolvedDatum
): Gen[RuleBasedTreasuryUtxo] =
    for {
        adaAmount <- Arbitrary
            .arbitrary[Coin]
            .map(c => Coin(math.abs(c.value) + 1000000L)) // Ensure minimum ADA

        // Treasury is always the first output of the fallback tx
        txId = TransactionInput(fallbackTxId, 0)
        spp = ShelleyPaymentPart.Script(RuleBasedTreasuryScript.compiledScriptHash)
        scriptAddr = ShelleyAddress(Mainnet, spp, ShelleyDelegationPart.Null)

        beaconTokenAssetName = AssetName(beaconTokenName)
        beaconToken = singleton(headMp, beaconTokenAssetName)
    } yield RuleBasedTreasuryUtxo(
      beaconTokenName = beaconTokenAssetName,
      txId = txId,
      addr = scriptAddr,
      datum = Unresolved(unresolvedDatum),
      value = Value(adaAmount) + beaconToken
    )

/** key != 0
  *
  * @param peersVKs
  * @return
  */
def genPeerVoteDatum(peersVKs: NonEmptyList[VerificationKeyBytes]): Gen[VoteDatum] =
    for {
        // key == 0 is the default `NoVote`, here we need a datum for OwnVoteUtxo
        key <- Gen.choose(1, peersVKs.length)
        link = (key + 1) % (peersVKs.length + 1)
        peer = Some(peersVKs.toList(key - 1).pubKeyHash)
    } yield VoteDatum(
      key = key,
      link = link,
      peer = peer.map(SOption.Some(_)).getOrElse(SOption.None),
      voteStatus = VoteStatus.NoVote
    )

def genVoteUtxo(
    fallbackTxId: TransactionHash,
    numberOfPeers: Int,
    headMp: PolicyId,
    voteTokenName: TokenName,
    voteDatum: VoteDatum
): Gen[OwnVoteUtxo] =
    for {
        outputIx <- Gen.choose(1, numberOfPeers)
        txId = TransactionInput(fallbackTxId, outputIx)
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
    } yield OwnVoteUtxo(
      AddrKeyHash(voteDatum.peer.get.hash),
      Utxo[L1](UtxoId[L1](txId), Output[L1](voteOutput))
    )

def genOnchainBlockHeader(versionMajor: BigInt): Gen[OnchainBlockHeader] =
    for {
        blockNum <- Gen.choose(10L, 20L).map(BigInt(_))
        timeCreation <- Gen.choose(1750000000L, 1760000000L).map(BigInt(_))
        versionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
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

def genCollateralUtxo(peer: TestPeer): Gen[(TransactionInput, Babbage)] =
    for {
        txId <- genTxId
    } yield (
      txId,
      Babbage(
        address = peer.address,
        value = Value(Coin(5_000_000L)),
        datumOption = None,
        scriptRef = None
      )
    )

def genVoteTxRecipe(
    estimatedFee: Coin = Coin(5_000_000L)
): Gen[VoteTx.Recipe] =
    for {

        // Common head parameters
        (
          headScriptHash,
          headTokensSuffix,
          peers,
          peersVKs,
          paramsHash,
          versionMajor,
          fallbackTxId
        ) <-
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
        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          fallbackTxId,
          headScriptHash,
          beaconTokenName,
          treasuryDatum
        )

        // Generate a vote UTXO with NoVote status (input)
        voteDatum <- genPeerVoteDatum(peersVKs)
        voteUtxo <- genVoteUtxo(
          fallbackTxId,
          peers.length,
          headScriptHash,
          voteTokenName,
          voteDatum
        )

        // Generate an onchain block header and sign using peers' wallets
        blockHeader <- genOnchainBlockHeader(versionMajor)
        signatures = signBlockHeader(blockHeader, peers)
        // Make vote details

        collateralUtxo <- genCollateralUtxo(peers.toList(voteDatum.key.intValue - 1))

        // Create builder context
        allUtxos = Map(
          voteUtxo.utxo.input.untagged -> voteUtxo.utxo.output.untagged,
          treasuryUtxo.toUtxo._1 -> treasuryUtxo.toUtxo._2,
          collateralUtxo._1 -> collateralUtxo._2
        )
        context = unsignedTxBuilderContext(allUtxos)

    } yield VoteTx.Recipe(
      voteUtxo = voteUtxo,
      treasuryUtxo = treasuryUtxo,
      collateralUtxo = Utxo[L1](UtxoId(collateralUtxo._1), Output(collateralUtxo._2)),
      blockHeader = blockHeader,
      signatures = signatures,
      // TODO: now sure how to do that properly
      ttl = 666,
      context = context
    )

class VoteTxTest extends munit.ScalaCheckSuite {
    private val log = Logger(getClass)

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
              case Left(e) => throw RuntimeException(s"Build failed $e")
              case Right(tx) =>
                  ()
              // println(HexUtil.encodeHexString(tx.tx.toCbor))
          }
      }
    )
}
