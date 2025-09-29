package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
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

//    // TODO: replace with variant that is not generated
//    val dummyAddr: ShelleyAddress = genPubkeyAddr().sample.get
//
//    test("Test minAda violation in the treasury") {
//        ////
//        // General data setup
//
//        // Seed UTxO with 100 ADA
//        val seedUtxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
//            .focus(_._2.value.coin.value)
//            .replace(100_000_000L)
//
//        val peers = NonEmptyList.fromListUnsafe(List(Alice, Bob))
//        val peerVKeys = peers.map(_.wallet.exportVerificationKeyBytes)
//
//        // This recipe should have exactly the min ADA
//        val recipeSucceed = InitializationTx.Recipe(
//          seedUtxos = NonEmptyList.one(seedUtxo),
//          initialDeposit = minInitTreasuryAda,
//          peers = peerVKeys,
//          context = unsignedTxBuilderContext(Map(seedUtxo)),
//          changeAddress = Alice.address
//        )
//
//        // This recipe should have 1 lovelace less than the minimum acceptable ADA
//        val recipeFail = recipeSucceed.focus(_.initialDeposit).modify(_ - Coin(1L))
//
//        InitializationTx.build(recipeFail) match {
//            case Left(
//                  InitializationTx.BuildError.OtherScalusTransactionException(
//                    e: TransactionException.OutputsHaveNotEnoughCoinsException
//                  )
//                ) =>
//                ()
//            case Right(_) => throw RuntimeException("Build succeeded, but should have failed")
//            case Left(e)  => throw RuntimeException(s"Build failed, but for the wrong reason: $e")
//        }
//
//        InitializationTx.build(recipeSucceed) match {
//            case Left(e)  => throw RuntimeException("Build failed but should have succeeded")
//            case Right(_) => ()
//        }
//    }
//
////    test("MinAda incoherence between l1 and l2") {
////        ???
////    }
//
//    test("Enough ada for minAda in treasury and change utxo, but insufficient ada to pay for fee") {
//        val seedUtxo = {
//            val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
//            utxo.focus(_._2.value.coin).replace(minPubkeyAda() + minInitTreasuryAda)
//        }
//
//        val recipe = InitializationTx.Recipe(
//          seedUtxos = NonEmptyList.one(seedUtxo),
//          initialDeposit = minInitTreasuryAda,
//          peers = NonEmptyList.one(Alice.wallet.exportVerificationKeyBytes),
//          context = unsignedTxBuilderContext(Map.from(List(seedUtxo))),
//          changeAddress = Alice.address
//        )
//
//        InitializationTx.build(recipe) match {
//            case Left(
//                  InitializationTx.BuildError.OtherScalusBalancingError(
//                    e: TxBalancingError.InsufficientFunds
//                  )
//                ) =>
//                ()
//            case Right(_) => throw RuntimeException("Build succeeded, but should have failed")
//            case Left(e)  => throw RuntimeException(s"Build failed, but for the wrong reason: $e")
//        }
//    }

    property("Vote tx builds")(
      Prop.forAll(genVoteTxRecipe()) { recipe =>
          // InitializationTx.build(recipe) match {
          //    case Left(e) => throw RuntimeException(s"Build failed $e")
          //    case Right(tx) =>
          //        val headMultisigScript = HeadMultisigScript(recipe.peers)
          //        val headTokenName = mkHeadTokenName(recipe.seedUtxos.map(_._1))
          //
          //        val bytes = tx.tx.toCbor
          //        given OriginalCborByteArray = OriginalCborByteArray(bytes)
          //        (tx.tx == Cbor
          //            .decode(bytes)
          //            .to[Transaction]
          //            .value) :| "Cbor round-tripping failed"
          //        &&
          //        (tx.tx.body.value.fee.value != 0L) :| "Tx Fee should not be 0"
          //        && (tx.tx.body.value.outputs.size === 2) :| "Initialization tx should have a treasury output and" +
          //            "change output"
          //            &&
          //            (tx.treasuryProduced.toUtxo._2 ==
          //                tx.tx.body.value.outputs.head.value) :|
          //            "treasury output in InitializationTx value not coherent with actual transaction produced"
          //            && (
          //              tx.tx.witnessSet.nativeScripts.head == headMultisigScript.script
          //            ) :| "Head multisig script not as expected"
          //            && (tx.treasuryProduced.headTokenName == headTokenName) :| "Unexpected head token name in treasury output"
          //            && (tx.treasuryProduced.toUtxo._2.value.assets.assets
          //                .get(headMultisigScript.policyId)
          //                .get(
          //                  headTokenName
          //                ) === 1L) :| "treasury output does not contain correct head token"
          //            && {
          //                val actual = tx.treasuryProduced.toUtxo._2.value
          //                val expected = Value(
          //                  coin = recipe.initialDeposit,
          //                  multiAsset = MultiAsset(assets =
          //                      SortedMap(
          //                        (headMultisigScript.policyId, SortedMap((headTokenName, 1L)))
          //                      )
          //                  )
          //                )
          //                (actual == expected) :| s"Unexpected treasury value. Actual: $actual, expected: $expected"
          //            }
          //            && tx.tx.auxiliaryData.contains(
          //              MD.apply(Initialization, headMultisigScript.address(Mainnet))
          //            )
          //            :| "Unexpected metadata"
          // }
      }
    )

}
