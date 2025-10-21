package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.cip67DisputeTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.{
    DisputeResolutionScript,
    RuleBasedTreasuryValidator
}
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteDetails, VoteStatus}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.dapp.utxo.TallyVoteUtxo
import org.scalacheck.{Gen, Prop, Test as ScalaCheckTest}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.api.v3.TokenName
import scalus.prelude.Option as SOption
import test.*

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
      peer = SOption.None, // Tally votes don't have peer field set
      voteStatus = VoteStatus.Vote(VoteDetails(commitment, versionMinor))
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
          peer = SOption.None,
          voteStatus = VoteStatus.Vote(VoteDetails(continuingCommitment, continuingVersionMinor))
        )

        removedDatum = VoteDatum(
          key = removedKey,
          link = nextLink,
          peer = SOption.None,
          voteStatus = VoteStatus.Vote(VoteDetails(removedCommitment, removedVersionMinor))
        )
    } yield (continuingDatum, removedDatum)

def genTallyVoteUtxo(
    fallbackTxId: TransactionHash,
    outputIndex: Int,
    headMp: PolicyId,
    voteTokenName: TokenName,
    voteDatum: VoteDatum,
    voter: AddrKeyHash
): Gen[TallyVoteUtxo] = {
    val txId = TransactionInput(fallbackTxId, outputIndex)
    val spp = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash)
    val scriptAddr = ShelleyAddress(Mainnet, spp, ShelleyDelegationPart.Null)

    val voteTokenAssetName = AssetName(voteTokenName)
    val voteToken = singleton(headMp, voteTokenAssetName)

    val voteOutput = Babbage(
      address = scriptAddr,
      // Sufficient ADA for minUTxO + tally fees
      value = Value(Coin(10_000_000L)) + voteToken,
      datumOption = Some(Inline(voteDatum.toData)),
      scriptRef = None
    )

    Gen.const(
      TallyVoteUtxo(
        voter = voter,
        Utxo[L1](UtxoId[L1](txId), Output[L1](voteOutput))
      )
    )
}

def genTallyTxRecipe(
    estimatedFee: Coin = Coin(5_000_000L)
): Gen[TallyTx.Recipe] =
    for {
        // Common head parameters
        (
          hns,
          headTokensSuffix,
          peers,
          peersVKs,
          paramsHash,
          versionMajor,
          fallbackTxId
        ) <-
            genHeadParams

        // Generate a treasury UTXO to use as reference input
        beaconTokenName = cip67BeaconTokenPrefix.concat(headTokensSuffix)
        voteTokenName = cip67DisputeTokenPrefix.concat(headTokensSuffix)

        treasuryDatum <- genTreasuryUnresolvedDatum(
            hns.policyId,
          voteTokenName,
          peersVKs,
          paramsHash,
          versionMajor
        )
        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          fallbackTxId,
            hns.policyId,
          beaconTokenName,
          treasuryDatum
        )

        // Generate compatible vote datums for tallying
        (continuingVoteDatum, removedVoteDatum) <- genCompatibleVoteDatums(peers.length)

        // Generate vote UTxOs with cast votes
        continuingVoteUtxo <- genTallyVoteUtxo(
          fallbackTxId,
          1, // Output index 1
            hns.policyId,
          voteTokenName,
          continuingVoteDatum,
          AddrKeyHash(peers.head.wallet.exportVerificationKeyBytes.pubKeyHash.hash)
        )

        removedVoteUtxo <- genTallyVoteUtxo(
          fallbackTxId,
          2, // Output index 2
            hns.policyId,
          voteTokenName,
          removedVoteDatum,
          AddrKeyHash(peers.toList(1).wallet.exportVerificationKeyBytes.pubKeyHash.hash)
        )

        collateralUtxo <- genCollateralUtxo(peers.head)

    } yield TallyTx.Recipe(
      continuingVoteUtxo = continuingVoteUtxo,
      removedVoteUtxo = removedVoteUtxo,
      treasuryUtxo = treasuryUtxo,
      collateralUtxo = Utxo[L1](UtxoId(collateralUtxo._1), Output(collateralUtxo._2)),
      validityEndSlot = 200,
      network = testNetwork,
      protocolParams = testProtocolParams,
      evaluator = testEvaluator,
      validators = testValidators
    )

class TallyTxTest extends munit.ScalaCheckSuite {

    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(10)
    }

    test("Tally recipe generator works") {
        val exampleRecipe = genTallyTxRecipe().sample.get
        println(s"Generated TallyTx recipe: $exampleRecipe")
    }

    property("Tally tx builds successfully")(
      Prop.forAll(genTallyTxRecipe()) { recipe =>
          TallyTx.build(recipe) match {
              case Left(e) =>
                  throw RuntimeException(s"TallyTx build failed: $e")
              case Right(tx) =>
                  // println(HexUtil.encodeHexString(tx.tx.toCbor))

                  // Basic smoke test assertions
                  assert(tx.continuingVoteUtxo != null)
                  assert(tx.removedVoteUtxo != null)
                  assert(tx.treasuryUtxo != null)
                  ()
          }
      }
    )
}
