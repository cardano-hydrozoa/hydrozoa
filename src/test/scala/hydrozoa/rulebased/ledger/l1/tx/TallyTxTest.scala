package hydrozoa.rulebased.ledger.l1.tx

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus, given_ToData_VoteDatum}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.TallyVoteUtxo
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.annotation.nowarn
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import test.PeersNumberSpec.Exact
import test.TestPeersSpec

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
      voteStatus = VoteStatus.Voted(commitment, versionMinor)
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
          voteStatus = VoteStatus.Voted(continuingCommitment, continuingVersionMinor)
        )

        removedDatum = VoteDatum(
          key = removedKey,
          link = nextLink,
          voteStatus = VoteStatus.Voted(removedCommitment, removedVersionMinor)
        )
    } yield (continuingDatum, removedDatum)

def genTallyVoteUtxo(
    config: CardanoNetwork.Section & HasTokenNames & HeadPeers.Section,
    fallbackTxId: TransactionHash,
    outputIndex: Int,
    voteDatum: VoteDatum,
    voter: AddrKeyHash,
): Gen[TallyVoteUtxo] = {
    val txId = TransactionInput(fallbackTxId, outputIndex)
    val spp = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash)
    val scriptAddr = ShelleyAddress(config.network, spp, ShelleyDelegationPart.Null)

    val voteTokenAssetName = config.headTokenNames.voteTokenName
    val voteToken = Value.asset(config.headMultisigScript.policyId, voteTokenAssetName, 1)

    val voteOutput = Babbage(
      address = scriptAddr,
      // Sufficient ADA for minAda + tally fees
      value = Value(Coin(10_000_000L)) + voteToken,
      datumOption = Some(Inline(voteDatum.toData(using VoteState.given_ToData_VoteDatum))),
      scriptRef = None
    )

    Gen.const(
      TallyVoteUtxo(
        Utxo(txId, voteOutput)
      )
    )
}

def genTallyTxRecipe(
    estimatedFee: Coin = Coin(5_000_000L)
): Gen[TallyTx.Recipe] =
    for {

        // Test currently uses two peers
        multiNodeConfig <- MultiNodeConfig.generate(
          TestPeersSpec.default.withPeersNumberSpec(Exact(2))
        )()
        config = multiNodeConfig.headConfig

        versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
        treasuryDatum <- genTreasuryUnresolvedDatum(
          config,
          versionMajor
        )

        fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          config,
          fallbackTxId,
          treasuryDatum
        )

        // Generate compatible vote datums for tallying
        (continuingVoteDatum, removedVoteDatum) <- genCompatibleVoteDatums(config.nHeadPeers.toInt)

        // Generate a vote utxo with cast votes
        continuingVoteUtxo <- genTallyVoteUtxo(
          config,
          fallbackTxId,
          1, // Output index 1
          continuingVoteDatum,
          AddrKeyHash(blake2b_224(config.headPeers.headPeerVKeys.head)),
        )

        removedVoteUtxo <- genTallyVoteUtxo(
          config,
          fallbackTxId,
          2, // Output index 2
          removedVoteDatum,
          AddrKeyHash(blake2b_224(config.headPeers.headPeerVKeys.toList(1))),
        )

        collateralUtxo <- genCollateralUtxo(
          config,
          multiNodeConfig.addressOf(HeadPeerNumber.zero)
        )

    } yield TallyTx.Recipe(
      continuingVoteUtxo = continuingVoteUtxo,
      removedVoteUtxo = removedVoteUtxo,
      treasuryUtxo = treasuryUtxo,
      collateralUtxo = Utxo(collateralUtxo._1, collateralUtxo._2),
      network = config.network,
      protocolParams = config.cardanoProtocolParams,
      evaluator = PlutusScriptEvaluator(config.cardanoInfo, EvaluatorMode.EvaluateAndComputeCost),
    )

@nowarn("msg=unused value")
class TallyTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 10)

    test("Tally recipe generator works") {
        val exampleRecipe = genTallyTxRecipe().sample.get
        println(s"Generated TallyTx recipe: $exampleRecipe")
    }

    test("Tally tx builds successfully") {
        forAll(genTallyTxRecipe()) { recipe =>
            TallyTx.build(recipe) match {
                case Left(e) =>
                    fail(s"TallyTx build failed: $e")
                case Right(tx) =>
                    // println(HexUtil.encodeHexString(tx.tx.toCbor))

                    // Basic smoke test assertions
                    assert(tx.continuingVoteUtxo != null)
                    assert(tx.removedVoteUtxo != null)
                    assert(tx.treasuryUtxo != null)
            }
        }
    }
}
