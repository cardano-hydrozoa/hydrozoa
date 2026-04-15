package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.Voted
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.{VoteOutput, VoteUtxo}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.uplc.builtin.Builtins.blake2b_224

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
    voteDatum: VoteDatum,
    voter: AddrKeyHash,
)(using
    config: HeadPeers.Section & HasTokenNames & CardanoNetwork.Section
): Gen[VoteUtxo[Voted]] = {
    val txId = TransactionInput(fallbackTxId, outputIndex)
    val scriptAddr = HydrozoaBlueprint.mkDisputeAddress(config.network)

    val voteTokenAssetName = config.headTokenNames.voteTokenName
    val voteToken = Value.asset(
      policyId = config.headMultisigScript.policyId,
      assetName = voteTokenAssetName,
      amount = config.nHeadPeers.convert + 1
    )

    val voteOutput: VoteOutput[Voted] = VoteOutput(
      key = voteDatum.key,
      link = voteDatum.link,
      coin = Coin(10_000_000),
      voteTokens = PositiveInt.unsafeApply(config.nHeadPeers + 1),
      status = voteDatum.voteStatus.asInstanceOf[Voted]
    )

    Gen.const(
      VoteUtxo(input = txId, voteOutput)
    )
}

// Feel free to trim down the config argument
def genResolutionTxBuilder(using multiNodeConfig: MultiNodeConfig): Gen[ResolutionTx.Build] =
    given config: HeadConfig = multiNodeConfig.headConfig

    for {
        fallbackTxId <- Arbitrary.arbitrary[TransactionHash]
        // Generate a treasury UTXO with Unresolved datum

        treasuryDatum <- genTreasuryUnresolvedDatum(
          BigInt(10)
        )
        treasuryUtxo <- genRuleBasedTreasuryUtxo(
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
          fallbackTxId,
          1, // Output index 1
          talliedVoteDatum,
          voter = AddrKeyHash(blake2b_224(config.headPeerVKeys.head))
        )

        collateralUtxo <- genCollateralUtxo(
          multiNodeConfig.addrKeyHashOf(HeadPeerNumber.zero)
        )

    } yield ResolutionTx.Build(
      talliedVoteUtxo = talliedVoteUtxo,
      treasuryUtxo = treasuryUtxo,
      collateralUtxo = collateralUtxo,
    )(using multiNodeConfig.nodeConfigs.head._2)

object ResolutionTxTest extends Properties("Resolution Tx Test") {
    import MultiNodeConfig.*

    val _ = property("Resolution Builder generator works") = runDefault(for {
        config <- ask
        _ <- {
            given MultiNodeConfig = config
            for {
                builder <- pick(genResolutionTxBuilder)
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
                  tx.treasuryUnresolvedUtxoSpent.treasuryOutput.datum.isInstanceOf[Unresolved],
                  "Input treasury should be Unresolved"
                )
            } yield ()
        }

    } yield true)
}
