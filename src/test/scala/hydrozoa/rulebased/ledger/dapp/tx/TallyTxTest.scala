package hydrozoa.rulebased.ledger.dapp.tx

//import cats.data.NonEmptyList
//import hydrozoa.*
//import hydrozoa.multisig.ledger.dapp.tx.Tx.Validators.nonSigningValidators
//import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.cip67DisputeTokenPrefix
//import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
//import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryValidator}
//import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteStatus}
//import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
//import hydrozoa.rulebased.ledger.dapp.utxo.TallyVoteUtxo
//import org.scalacheck.Gen
//import org.scalatest.funsuite.AnyFunSuite
//import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
//import scala.annotation.nowarn
//import scalus.uplc.builtin.ByteString
//import scalus.uplc.builtin.Data.toData
//import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
//import scalus.cardano.ledger.DatumOption.Inline
//import scalus.cardano.ledger.TransactionOutput.Babbage
//import scalus.cardano.ledger.{Utxo, *}
//import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
//import scalus.ledger.api.v3.TokenName
//import test.*
//
///** Generate a vote datum with a cast vote for tally testing
//  */
//def genCastVoteDatum(
//    key: Int,
//    link: Int,
//    versionMajor: BigInt
//): Gen[VoteDatum] =
//    for {
//        versionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
//        commitment <- genByteStringOfN(48) // KZG commitment
//    } yield VoteDatum(
//      key = key,
//      link = link,
//      voteStatus = VoteStatus.Voted(commitment, versionMinor)
//    )
//
///** Generate a pair of compatible vote datums for tallying
//  */
//def genCompatibleVoteDatums(peersN: Int): Gen[(VoteDatum, VoteDatum)] =
//    for {
//        continuingKey <- Gen.choose(0, peersN - 1)
//        removedKey = continuingKey + 1
//        nextLink = (removedKey + 1) % (peersN + 1)
//
//        // Generate independent commitments and versions for each vote
//        continuingVersionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
//        continuingCommitment <- genByteStringOfN(48)
//
//        removedVersionMinor <- Gen.choose(0L, 100L).map(BigInt(_))
//        removedCommitment <- genByteStringOfN(48)
//
//        continuingDatum = VoteDatum(
//          key = continuingKey,
//          link = removedKey, // Key constraint: continuing vote links to removed vote
//          voteStatus = VoteStatus.Voted(continuingCommitment, continuingVersionMinor)
//        )
//
//        removedDatum = VoteDatum(
//          key = removedKey,
//          link = nextLink,
//          voteStatus = VoteStatus.Voted(removedCommitment, removedVersionMinor)
//        )
//    } yield (continuingDatum, removedDatum)
//
//def genTallyVoteUtxo(
//    fallbackTxId: TransactionHash,
//    outputIndex: Int,
//    headMp: PolicyId,
//    voteTokenName: TokenName,
//    voteDatum: VoteDatum,
//    voter: AddrKeyHash
//): Gen[TallyVoteUtxo] = {
//    val txId = TransactionInput(fallbackTxId, outputIndex)
//    val spp = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash)
//    val scriptAddr = ShelleyAddress(testNetwork, spp, ShelleyDelegationPart.Null)
//
//    val voteTokenAssetName = AssetName(voteTokenName)
//    val voteToken = singleton(headMp, voteTokenAssetName)
//
//    val voteOutput = Babbage(
//      address = scriptAddr,
//      // Sufficient ADA for minAda + tally fees
//      value = Value(Coin(10_000_000L)) + voteToken,
//      datumOption = Some(Inline(voteDatum.toData)),
//      scriptRef = None
//    )
//
//    Gen.const(
//      TallyVoteUtxo(
//        voter = voter,
//        Utxo(txId, voteOutput)
//      )
//    )
//}
//
//def genTallyTxRecipe(
//    estimatedFee: Coin = Coin(5_000_000L)
//): Gen[TallyTx.Recipe] =
//    for {
//        // Common head parameters
//        (
//          hns,
//          headTokensSuffix,
//          peers,
//          peersVKs,
//          paramsHash,
//          versionMajor,
//          fallbackTxId
//        ) <-
//            genHeadParams
//
//        // Generate a treasury UTXO to use as reference input
//        beaconTokenName = cip67BeaconTokenPrefix.concat(headTokensSuffix)
//        voteTokenName = cip67DisputeTokenPrefix.concat(headTokensSuffix)
//
//        treasuryDatum <- genTreasuryUnresolvedDatum(
//          hns.policyId,
//          voteTokenName,
//          peersVKs,
//          paramsHash,
//          versionMajor
//        )
//        treasuryUtxo <- genRuleBasedTreasuryUtxo(
//          fallbackTxId,
//          hns.policyId,
//          beaconTokenName,
//          treasuryDatum
//        )
//
//        // Generate compatible vote datums for tallying
//        (continuingVoteDatum, removedVoteDatum) <- genCompatibleVoteDatums(peers.length)
//
//        // Generate a vote utxo with cast votes
//        continuingVoteUtxo <- genTallyVoteUtxo(
//          fallbackTxId,
//          1, // Output index 1
//          hns.policyId,
//          voteTokenName,
//          continuingVoteDatum,
//          AddrKeyHash(peers.head.wallet.exportVerificationKeyBytes.pubKeyHash.hash)
//        )
//
//        removedVoteUtxo <- genTallyVoteUtxo(
//          fallbackTxId,
//          2, // Output index 2
//          hns.policyId,
//          voteTokenName,
//          removedVoteDatum,
//          AddrKeyHash(peers.toList(1).wallet.exportVerificationKeyBytes.pubKeyHash.hash)
//        )
//
//        collateralUtxo <- genCollateralUtxo(peers.head)
//
//    } yield TallyTx.Recipe(
//      continuingVoteUtxo = continuingVoteUtxo,
//      removedVoteUtxo = removedVoteUtxo,
//      treasuryUtxo = treasuryUtxo,
//      collateralUtxo = Utxo(collateralUtxo._1, collateralUtxo._2),
//      validityEndSlot = 200,
//      network = testNetwork,
//      protocolParams = testProtocolParams,
//      evaluator = testEvaluator,
//      validators = nonSigningValidators
//    )
//
//@nowarn("msg=unused value")
//class TallyTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {
//
//    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
//        PropertyCheckConfiguration(minSuccessful = 10)
//
//    test("Tally recipe generator works") {
//        val exampleRecipe = genTallyTxRecipe().sample.get
//        println(s"Generated TallyTx recipe: $exampleRecipe")
//    }
//
//    test("Tally tx builds successfully") {
//        forAll(genTallyTxRecipe()) { recipe =>
//            TallyTx.build(recipe) match {
//                case Left(e) =>
//                    fail(s"TallyTx build failed: $e")
//                case Right(tx) =>
//                    // println(HexUtil.encodeHexString(tx.tx.toCbor))
//
//                    // Basic smoke test assertions
//                    assert(tx.continuingVoteUtxo != null)
//                    assert(tx.removedVoteUtxo != null)
//                    assert(tx.treasuryUtxo != null)
//            }
//        }
//    }
//}
