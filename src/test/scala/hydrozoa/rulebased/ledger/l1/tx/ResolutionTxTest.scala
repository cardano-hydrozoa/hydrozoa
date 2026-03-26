package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.TallyVoteUtxo
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.Data.toData

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
    config: HeadPeers.Section & HasTokenNames & CardanoNetwork.Section,
    fallbackTxId: TransactionHash,
    outputIndex: Int,
    voteDatum: VoteDatum,
    voter: AddrKeyHash,
): Gen[TallyVoteUtxo] = {
    val txId = TransactionInput(fallbackTxId, outputIndex)
    val spp = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash)
    val scriptAddr = ShelleyAddress(config.network, spp, ShelleyDelegationPart.Null)

    val voteTokenAssetName = config.headTokenNames.voteTokenName
    val voteToken = Value.asset(
      policyId = config.headMultisigScript.policyId,
      assetName = voteTokenAssetName,
      amount = config.nHeadPeers.convert + 1
    )

    val voteOutput = Babbage(
      address = scriptAddr,
      // Sufficient ADA for minAda + resolution fees
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

// Feel free to trim down the config argument
def genResolutionTxBuilder(multiNodeConfig: MultiNodeConfig): Gen[ResolutionTx.Build] =
    val config = multiNodeConfig.headConfig

    for {
        fallbackTxId <- Arbitrary.arbitrary[TransactionHash]
        // Generate a treasury UTXO with Unresolved datum

        treasuryDatum <- genTreasuryUnresolvedDatum(
          config,
          BigInt(10)
        )
        treasuryUtxo <- genRuleBasedTreasuryUtxo(
          config,
          fallbackTxId,
          treasuryDatum
        )

        // Generate a tallied vote datum with Vote status (the result of a tally)
        talliedVoteDatum <- genTalliedVoteDatum(
          key = 1, // First peer voted
          link = 2 // Links to next peer
        )

        // Generate tallied vote utxo
        talliedVoteUtxo <- genResolutionTallyVoteUtxo(
          config,
          fallbackTxId,
          1, // Output index 1
          talliedVoteDatum,
          voter = AddrKeyHash(blake2b_224(config.headPeerVKeys.head))
        )

        collateralUtxo <- genCollateralUtxo(
          config,
          multiNodeConfig.addrKeyHashOf(HeadPeerNumber.zero)
        )

    } yield ResolutionTx.Build(multiNodeConfig.nodeConfigs.head._2)(
      _talliedVoteUtxo = talliedVoteUtxo,
      _treasuryUtxo = treasuryUtxo,
      _collateralUtxo = collateralUtxo,
    )

object ResolutionTxTest extends Properties("Resolution Tx Test") {
    import MultiNodeConfig.*

    val _ = property("Resolution Builder generator works") = runDefault(for {
        config <- ask
        builder <- pick(genResolutionTxBuilder(config))
        tx <- failLeft(builder.result)
        // Basic smoke test assertions
        _ <- assertWith(tx.talliedVoteUtxo != null, "Tallied vote UTXO should not be null")
        _ <- assertWith(
          tx.treasuryUnresolvedUtxoSpent != null,
          "Treasury unresolved UTXO spent should not be null"
        )
        _ <- assertWith(
          tx.treasuryResolvedUtxoProduced != null,
          "Treasury resolved UTXO produced should not be null"
        )
        _ <- assertWith(tx.tx != null, "Transaction should not be null")

        // Verify the spent treasury UTXO matches the recipe input
        _ <- assertWith(
          tx.treasuryUnresolvedUtxoSpent == builder.treasuryUtxo,
          "Spent treasury UTXO should match recipe input"
        )

        // Verify treasury state transition from Unresolved to Resolved
        _ <- assertWith(
          tx.treasuryUnresolvedUtxoSpent.datum.isInstanceOf[Unresolved],
          "Input treasury should be Unresolved"
        )
    } yield true)
}
