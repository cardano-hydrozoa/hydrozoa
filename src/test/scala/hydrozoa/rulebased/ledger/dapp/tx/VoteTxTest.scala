package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.*
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.cip67DisputeTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryValidator}
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.dapp.utxo.OwnVoteUtxo
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
import scalus.ledger.api.v3.{PubKeyHash, TokenName}
import scalus.prelude.Option as SOption
import test.*

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
        scriptAddr = ShelleyAddress(testNetwork, spp, ShelleyDelegationPart.Null)

        voteTokenAssetName = AssetName(voteTokenName)
        voteToken = singleton(headMp, voteTokenAssetName)

        voteOutput = Babbage(
          address = scriptAddr,
          // Sufficient ADA for minAda + vote/tallying fees
          value = Value(Coin(10_000_000L)) + voteToken,
          datumOption = Some(Inline(voteDatum.toData)),
          scriptRef = None
        )
    } yield OwnVoteUtxo(
      AddrKeyHash(voteDatum.peer.get.hash),
      Utxo[L1](UtxoId[L1](txId), Output[L1](voteOutput))
    )

def genVoteTxRecipe(
    estimatedFee: Coin = Coin(5_000_000L)
): Gen[VoteTx.Recipe] =
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

        // Generate a treasury UTXO to use a reference input
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

        // Generate a vote UTXO with NoVote status (input)
        voteDatum <- genPeerVoteDatum(peersVKs)
        voteUtxo <- genVoteUtxo(
          fallbackTxId,
          peers.length,
            hns.policyId,
          voteTokenName,
          voteDatum
        )

        // Generate an onchain block header and sign using peers' wallets
        blockHeader <- genOnchainBlockHeader(versionMajor)
        signatures = signBlockHeader(blockHeader, peers)
        // Make vote details

        collateralUtxo <- genCollateralUtxo(peers.toList(voteDatum.key.intValue - 1))

        // Create builder context (not needed for Recipe anymore)
        allUtxos = Map(
          voteUtxo.utxo.input.untagged -> voteUtxo.utxo.output.untagged,
          treasuryUtxo.toUtxo._1 -> treasuryUtxo.toUtxo._2,
          collateralUtxo._1 -> collateralUtxo._2
        )

    } yield VoteTx.Recipe(
      voteUtxo = voteUtxo,
      treasuryUtxo = treasuryUtxo,
      collateralUtxo = Utxo[L1](UtxoId(collateralUtxo._1), Output(collateralUtxo._2)),
      blockHeader = blockHeader,
      signatures = signatures,
      // TODO: now sure how to do that properly
      validityEndSlot = 200,
      network = testNetwork,
      protocolParams = testProtocolParams,
      evaluator = testEvaluator,
      validators = testValidators
    )

@nowarn("msg=unused value")
class VoteTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {
    // private val log = Logger(getClass)

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)

    test("Recipe generator works") {
        val exampleRecipe = genVoteTxRecipe().sample.get
        println(exampleRecipe)
    }

    test("Vote tx builds") {
        forAll(genVoteTxRecipe()) { recipe =>
            VoteTx.build(recipe) match {
                case Left(e) =>
                    fail(s"Build failed $e")
                case Right(tx) =>
                     //println(HexUtil.encodeHexString(tx.tx.toCbor))

                    // Verify VoteTx structure
                    assert(
                      tx.voteUtxoSpent == recipe.voteUtxo,
                      "Spent vote UTXO should match recipe input"
                    )
                    assert(tx.voteUtxoProduced != null, "Vote UTXO produced should not be null")
                    assert(tx.tx != null, "Transaction should not be null")
            }
        }
    }
}
