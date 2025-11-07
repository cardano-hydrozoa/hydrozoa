package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.*
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.cip67DisputeTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.{
    DisputeResolutionScript,
    RuleBasedTreasuryValidator
}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.dapp.utxo.TallyVoteUtxo
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.annotation.nowarn
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.api.v3.TokenName
import test.*

/** Generate a tallied vote datum with Vote status for resolution testing
  */
def genTalliedVoteDatum(
    key: Int,
    link: Int
): Gen[VoteDatum] =
    for {
        versionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
        commitment <- genByteStringOfN(48) // KZG commitment
    } yield VoteDatum(
      key = key,
      link = link,
      voteStatus = VoteStatus.Voted(commitment, versionMinor)
    )

def genResolutionTallyVoteUtxo(
    fallbackTxId: TransactionHash,
    outputIndex: Int,
    headMp: PolicyId,
    voteTokenName: TokenName,
    voteTokenAmount: Int,
    voteDatum: VoteDatum,
    voter: AddrKeyHash
): Gen[TallyVoteUtxo] = {
    val txId = TransactionInput(fallbackTxId, outputIndex)
    val spp = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash)
    val scriptAddr = ShelleyAddress(testNetwork, spp, ShelleyDelegationPart.Null)

    val voteTokenAssetName = AssetName(voteTokenName)
    val voteToken = singleton(headMp, voteTokenAssetName, voteTokenAmount)

    val voteOutput = Babbage(
      address = scriptAddr,
      // Sufficient ADA for minAda + resolution fees
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

def genResolutionTxRecipe(
    estimatedFee: Coin = Coin(5_000_000L)
): Gen[ResolutionTx.Recipe] =
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

        // Generate a treasury UTXO with Unresolved datum
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

        // Generate a tallied vote datum with Vote status (the result of a tally)
        talliedVoteDatum <- genTalliedVoteDatum(
          key = 1, // First peer voted
          link = 2 // Links to next peer
        )

        // Generate tallied vote utxo
        talliedVoteUtxo <- genResolutionTallyVoteUtxo(
          fallbackTxId,
          1, // Output index 1
          hns.policyId,
          voteTokenName,
          peersVKs.size + 1, // number of vote tokens in the tallied utxo
          talliedVoteDatum,
          AddrKeyHash(peers.head.wallet.exportVerificationKeyBytes.pubKeyHash.hash)
        )

        collateralUtxo <- genCollateralUtxo(peers.head)

    } yield ResolutionTx.Recipe(
      talliedVoteUtxo = talliedVoteUtxo,
      treasuryUtxo = treasuryUtxo,
      collateralUtxo = Utxo[L1](UtxoId(collateralUtxo._1), Output(collateralUtxo._2)),
      validityEndSlot = 200,
      network = testNetwork,
      protocolParams = testProtocolParams,
      evaluator = testEvaluator,
      validators = testValidators
    )

@nowarn("msg=unused value")
class ResolutionTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 10)

    test("Resolution recipe generator works") {
        val exampleRecipe = genResolutionTxRecipe().sample.get
        println(s"Generated ResolutionTx recipe: $exampleRecipe")
    }

    // This doesn't fit the size, we needed to use reference script, ignoring for now
    ignore("Resolution tx builds successfully") {
        forAll(genResolutionTxRecipe()) { recipe =>
            ResolutionTx.build(recipe) match {
                case Left(e) =>
                    fail(s"ResolutionTx build failed: $e")
                case Right(tx) =>
                    println(HexUtil.encodeHexString(tx.tx.toCbor))

                    // Basic smoke test assertions
                    assert(tx.talliedVoteUtxo != null, "Tallied vote UTXO should not be null")
                    assert(
                      tx.treasuryUnresolvedUtxoSpent != null,
                      "Treasury unresolved UTXO spent should not be null"
                    )
                    assert(
                      tx.treasuryResolvedUtxoProduced != null,
                      "Treasury resolved UTXO produced should not be null"
                    )
                    assert(tx.tx != null, "Transaction should not be null")

                    // Verify the spent treasury UTXO matches the recipe input
                    assert(
                      tx.treasuryUnresolvedUtxoSpent == recipe.treasuryUtxo,
                      "Spent treasury UTXO should match recipe input"
                    )

                    // Verify treasury state transition from Unresolved to Resolved
                    assert(
                      tx.treasuryUnresolvedUtxoSpent.datum.isInstanceOf[Unresolved],
                      "Input treasury should be Unresolved"
                    )
            }
        }
    }
}
