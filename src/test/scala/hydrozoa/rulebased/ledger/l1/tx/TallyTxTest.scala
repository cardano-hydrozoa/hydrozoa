package hydrozoa.rulebased.ledger.l1.tx

import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.l1.tx.Tx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.cip67DisputeTokenPrefix
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
import hydrozoa.rulebased.ledger.l1.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryValidator}
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.TallyVoteUtxo
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.annotation.nowarn
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.cardano.onchain.plutus.v3.TokenName
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
    fallbackTxId: TransactionHash,
    outputIndex: Int,
    headMp: PolicyId,
    voteTokenName: TokenName,
    voteDatum: VoteDatum,
    voter: AddrKeyHash,
    network: Network
): Gen[TallyVoteUtxo] = {
    val txId = TransactionInput(fallbackTxId, outputIndex)
    val spp = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash)
    val scriptAddr = ShelleyAddress(network, spp, ShelleyDelegationPart.Null)

    val voteTokenAssetName = AssetName(voteTokenName)
    val voteToken = Value.assets(Map(headMp -> Map(voteTokenAssetName -> 1)))

    val voteOutput = Babbage(
      address = scriptAddr,
      // Sufficient ADA for minAda + tally fees
      value = Value(Coin(10_000_000L)) + voteToken,
      datumOption = Some(Inline(voteDatum.toData)),
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

        // This is 4 bytes shorter to accommodate CIP-67 prefixes
        // NB: we use the same token name _suffix_ for all head tokens so far, which is not the case in reality
        headTokensSuffix <- genByteStringOfN(28)
        // Generate a treasury UTXO to use as reference input
        beaconTokenName = cip67BeaconTokenPrefix.concat(headTokensSuffix)
        voteTokenName = cip67DisputeTokenPrefix.concat(headTokensSuffix)

        versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
        treasuryDatum <- genTreasuryUnresolvedDatum(
          config,
          voteTokenName,
          versionMajor
        )

        fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          config,
          fallbackTxId,
          config.headMultisigScript.policyId,
          beaconTokenName,
          treasuryDatum
        )

        // Generate compatible vote datums for tallying
        (continuingVoteDatum, removedVoteDatum) <- genCompatibleVoteDatums(config.nHeadPeers.toInt)

        // Generate a vote utxo with cast votes
        continuingVoteUtxo <- genTallyVoteUtxo(
          fallbackTxId,
          1, // Output index 1
          config.headMultisigScript.policyId,
          voteTokenName,
          continuingVoteDatum,
          AddrKeyHash(blake2b_224(config.headPeers.headPeerVKeys.head)),
          config.network
        )

        removedVoteUtxo <- genTallyVoteUtxo(
          fallbackTxId,
          2, // Output index 2
          config.headMultisigScript.policyId,
          voteTokenName,
          removedVoteDatum,
          AddrKeyHash(blake2b_224(config.headPeers.headPeerVKeys.toList(1))),
          config.network
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
      validators = nonSigningValidators
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
