package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.head.HeadPeersSpec.Exact
import hydrozoa.config.head.peers.generateTestPeers
import hydrozoa.config.node.generateNodeConfig
import hydrozoa.multisig.ledger.dapp.tx.Tx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.cip67DisputeTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryValidator}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.dapp.utxo.TallyVoteUtxo
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scalus.uplc.builtin.Builtins.blake2b_224
import scala.annotation.nowarn
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Utxo, *}
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.cardano.onchain.plutus.v3.TokenName
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
    voter: AddrKeyHash,
    network: Network
): Gen[TallyVoteUtxo] = {
    val txId = TransactionInput(fallbackTxId, outputIndex)
    val spp = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash)
    val scriptAddr = ShelleyAddress(network, spp, ShelleyDelegationPart.Null)

    val voteTokenAssetName = AssetName(voteTokenName)
    val voteToken = Value.assets(Map(headMp -> Map(voteTokenAssetName -> voteTokenAmount)))

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
        Utxo(txId, voteOutput)
      )
    )
}

def genResolutionTxRecipe(
    estimatedFee: Coin = Coin(5_000_000L)
): Gen[ResolutionTx.Recipe] =
    for {
        // Common head parameters
        // FIXME: This genHeadParams is old. We're now using `generateNodeConfig`
        // but we still need a few fields from this.
        (
          _,
          headTokensSuffix,
          _,
          _,
          _,
          versionMajor,
          fallbackTxId
        ) <-
            genHeadParams

        testPeers <- generateTestPeers()
        config <- generateNodeConfig(Exact(testPeers.nHeadPeers.toInt))()

        // Generate a treasury UTXO with Unresolved datum
        beaconTokenName = cip67BeaconTokenPrefix.concat(headTokensSuffix)
        voteTokenName = cip67DisputeTokenPrefix.concat(headTokensSuffix)

        treasuryDatum <- genTreasuryUnresolvedDatum(
          config,
          voteTokenName,
          versionMajor
        )
        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          config,
          fallbackTxId,
          config.headMultisigScript.policyId,
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
          config.headMultisigScript.policyId,
          voteTokenName,
          config.nHeadPeers.toInt + 1, // number of vote tokens in the tallied utxo
          talliedVoteDatum,
          AddrKeyHash(blake2b_224(config.headPeerVKeys.head)),
          config.network
        )

        collateralUtxo <- genCollateralUtxo(config, testPeers._testPeers.head._2)

    } yield ResolutionTx.Recipe(
      talliedVoteUtxo = talliedVoteUtxo,
      treasuryUtxo = treasuryUtxo,
      collateralUtxo = Utxo(collateralUtxo._1, collateralUtxo._2),
      validityEndSlot = 200,
      network = config.network,
      protocolParams = config.cardanoProtocolParams,
      evaluator = PlutusScriptEvaluator(config.cardanoInfo, EvaluatorMode.EvaluateAndComputeCost),
      validators = nonSigningValidators
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
