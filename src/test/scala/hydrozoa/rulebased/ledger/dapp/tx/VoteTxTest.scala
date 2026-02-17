package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.config.head.HeadPeersSpec.Exact
import hydrozoa.config.head.peers.generateTestPeers
import hydrozoa.config.node.generateNodeConfig
import hydrozoa.multisig.ledger.dapp.tx.Tx.Validators.nonSigningValidators
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.dapp.state.VoteState.VoteStatus.AwaitingVote
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.dapp.utxo.OwnVoteUtxo
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.annotation.nowarn
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.crypto.ed25519.VerificationKey
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.TokenName

/** key != 0
  *
  * @param peersVKs
  * @return
  */
def genPeerVoteDatum(peersVKs: NonEmptyList[VerificationKey]): Gen[VoteDatum] =
    for {
        // key == 0 is the default `NoVote`, here we need a datum for OwnVoteUtxo
        key <- Gen.choose(1, peersVKs.length)
        link = (key + 1) % (peersVKs.length + 1)
        peer = PubKeyHash(blake2b_224(peersVKs.toList(key - 1)))
    } yield VoteDatum(
      key = key,
      link = link,
      voteStatus = VoteStatus.AwaitingVote(peer)
    )

// TODO: Determine what *Config.Section this should take
def genVoteUtxo(
    fallbackTxId: TransactionHash,
    numberOfPeers: Int,
    headMp: PolicyId,
    voteTokenName: TokenName,
    voteDatum: VoteDatum,
    network: Network
): Gen[OwnVoteUtxo] =
    for {
        outputIx <- Gen.choose(1, numberOfPeers)
        txId = TransactionInput(fallbackTxId, outputIx)
        spp = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash)
        scriptAddr = ShelleyAddress(network, spp, ShelleyDelegationPart.Null)

        voteTokenAssetName = AssetName(voteTokenName)
        voteToken = Value.assets(Map(headMp -> Map(voteTokenAssetName -> 1)))

        voteOutput = Babbage(
          address = scriptAddr,
          // Sufficient ADA for minAda + vote/tallying fees
          value = Value(Coin(10_000_000L)) + voteToken,
          datumOption = Some(Inline(voteDatum.toData)),
          scriptRef = None
        )
    } yield OwnVoteUtxo(
      AddrKeyHash(voteDatum.voteStatus.asInstanceOf[AwaitingVote].peer.hash),
      Utxo(txId, voteOutput)
    )

def genVoteTxRecipe(
    estimatedFee: Coin = Coin(5_000_000L)
): Gen[VoteTx.Recipe] =
    for {

        // Common head parameters
        testPeers <- generateTestPeers()
        nodeConfig <- generateNodeConfig(Exact(testPeers.headPeers.nHeadPeers.toInt))()

        versionMajor <- Gen.choose(1L, 99L).map(BigInt(_))
        // Generate a treasury UTXO to use a reference input
        treasuryDatum <- genTreasuryUnresolvedDatum(
          nodeConfig,
          nodeConfig.headTokenNames.voteTokenName.bytes,
          versionMajor
        )
        fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)

        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          config = nodeConfig,
          fallbackTxId = fallbackTxId,
          headMp = nodeConfig.headMultisigScript.policyId,
          treasuryTokenName = nodeConfig.headConfig.headTokenNames.treasuryTokenName.bytes,
          treasuryDatum
        )

        // Generate a vote UTXO with NoVote status (input)
        voteDatum <- genPeerVoteDatum(nodeConfig.headPeerVKeys)
        voteUtxo <- genVoteUtxo(
          fallbackTxId = fallbackTxId,
          numberOfPeers = nodeConfig.nHeadPeers.toInt,
          headMp = nodeConfig.headMultisigScript.policyId,
          voteTokenName = nodeConfig.headTokenNames.voteTokenName.bytes,
          voteDatum = voteDatum,
          network = nodeConfig.network
        )

        // Generate an onchain block header and sign using peers' wallets
        blockHeader <- genOnchainBlockHeader(versionMajor)
        signatures = signBlockHeader(blockHeader, testPeers._testPeers.map(_._2))
        // Make vote details

        collateralUtxo <- genCollateralUtxo(
          nodeConfig,
          testPeers._testPeers.map(_._2).toList(voteDatum.key.intValue - 1)
        )

        // Create builder context (not needed for Recipe anymore)
        allUtxos = Map(
          voteUtxo.utxo.input -> voteUtxo.utxo.output,
          treasuryUtxo.asTuple._1 -> treasuryUtxo.asTuple._2,
          collateralUtxo._1 -> collateralUtxo._2
        )

    } yield VoteTx.Recipe(
      voteUtxo = voteUtxo,
      treasuryUtxo = treasuryUtxo,
      collateralUtxo = Utxo(collateralUtxo._1, collateralUtxo._2),
      blockHeader = blockHeader,
      signatures = signatures,
      // TODO: now sure how to do that properly
      validityEndSlot = 200,
      network = nodeConfig.network,
      protocolParams = nodeConfig.cardanoProtocolParams,
      evaluator =
          PlutusScriptEvaluator(nodeConfig.cardanoInfo, EvaluatorMode.EvaluateAndComputeCost),
      validators = nonSigningValidators
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
                    // println(HexUtil.encodeHexString(tx.tx.toCbor))

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
