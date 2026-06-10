package hydrozoa.integration.rbr.property

import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryUtxo, VoteOutput, VoteUtxo}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import org.scalacheck.Gen
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.{Coin, DatumOption, TransactionHash, TransactionInput, Utxos}
import scalus.cardano.onchain.plutus.v3.PubKeyHash
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.bls12_381.G2Element
import test.Generators.Hydrozoa.genEvacuationMap

import scala.concurrent.duration.FiniteDuration

/** All UTxOs that exist in the shared mock backend at the start of the dispute scenario. This
  * mimics what FallbackTx.Build would produce, but is constructed directly.
  */
case class InitialDisputeUtxos(
    treasury: RuleBasedTreasuryUtxo,
    // One AwaitingVote UTxO per peer, indexed by HeadPeerNumber
    peerVoteUtxos: Map[HeadPeerNumber, VoteUtxo[VoteStatus.AwaitingVote]],
    // Pre-voted default vote: key=0, link=1
    defaultVoteUtxo: VoteUtxo[VoteStatus.Voted],
    // ADA-only UTxO at each peer's head address, used as plutus script collateral
    collateralUtxos: Map[HeadPeerNumber, CollateralUtxo],
    // Reference script UTxOs from MultiNodeConfig
    refScriptUtxos: Map[TransactionInput, scalus.cardano.ledger.TransactionOutput],
    // Evacuation map generated at fallback — used by EvacuationActor as evacuationMapAtFallback
    evacuationMap: EvacuationMap,
):
    // The KZG commitment all peers should vote for in the happy-path scenario
    def kzgCommitment: VoteState.KzgCommitment = defaultVoteUtxo.voteOutput.status.commitment

    def allUtxos(using env: MultiNodeConfig): Utxos =
        // Any NodeConfig satisfies VoteOutputConfig; all share the same headConfig for output construction
        given voteOutputConfig: hydrozoa.rulebased.ledger.l1.utxo.VoteOutputConfig =
            env.nodeConfigs.values.head

        val treasuryEntry =
            treasury.utxoId -> treasury.treasuryOutput.toOutput(using env.headConfig)
        val peerVotes = peerVoteUtxos.values.map(v => v.input -> v.toUtxo.output)
        val defaultVote = defaultVoteUtxo.input -> defaultVoteUtxo.toUtxo.output
        val collaterals = collateralUtxos.values.map(c =>
            c.input -> c.collateralOutput.toOutput(using env.headConfig)
        )
        (Map(treasuryEntry, defaultVote) ++ peerVotes ++ collaterals ++ refScriptUtxos).toMap

object InitialDisputeUtxos:
    /** @param now
      *   simulated "now", used to calculate the voting deadline. This should either be from
      *   TestControl (currently not possible due to
      *   https://github.com/cloudmark/cats-actors/issues/43) or hard-coded at some point in the
      *   future to get test determinism.
      * @param votingDuration
      *   overrides env.votingDuration so tests can use short windows
      * @param nEvacs
      *   number of entries in the evacuation map
      * @param env
      * @return
      */
    def gen(
        fallbackTxId: TransactionHash,
        now: QuantizedInstant,
        votingDuration: FiniteDuration,
        nEvacs: Int = 100,
    )(using env: MultiNodeConfig): Gen[InitialDisputeUtxos] =
        val nPeers = env.headConfig.nHeadPeers.convert
        val votingDeadline: BigInt = now.toPosixTime + votingDuration.toMillis

        // [SYNTHETIC] KZG G2 setup (reuses the same approach as the existing DisputeActorTest)
        val setup = TrustedSetup
            .takeSrsG2(65)
            .map(p2 => G2Element(p2).toCompressedByteString)

        // Evacuation UTxOs carry a fixed sentinel datum ("evacuation") rather than real
        // per-output data. The DisputeClassifier abstraction function keys on this sentinel
        // to identify evacuation outputs without needing to decode production-format datums.
        val genEvacuationSentinelDatum: Gen[Option[DatumOption]] =
            Gen.const(Some(Inline(ByteString.fromString("evacuation").toData)))

        for

            evacMap <- genEvacuationMap(nEvacs, genDatum = genEvacuationSentinelDatum)(using
              env.headConfig
            )

            treasuryValue = evacMap.totalValue

            // [SYNTHETIC] Treasury UTxO at output index 0 of the (synthetic) fallback tx
            treasury <- CommonGenerators.genRuleBasedTreasuryUtxo(
              fallbackTxId,
              Unresolved(
                deadlineVoting = votingDeadline,
                versionMajor = BigInt(1),
                setupG2 = setup
              ),
              Gen.const(treasuryValue)
            )(using env.headConfig)

            // [SYNTHETIC] Peer vote UTxOs at output indices nPeers+1 .. 2*nPeers
            // Key/link chain: peer i → key=i+1, link=i+2 (last peer links back to 0)
            peerVoteUtxos = env.headConfig.headPeerVKeys.toList.zipWithIndex.map { (vkey, i) =>
                val key = BigInt(i + 1)
                val link = if i < nPeers - 1 then BigInt(i + 2) else BigInt(0)
                val voteOutput = VoteOutput[VoteStatus.AwaitingVote](
                  key,
                  link,
                  Coin.ada(5),
                  PositiveInt.unsafeApply(1),
                  VoteStatus.AwaitingVote(PubKeyHash(blake2b_224(vkey)))
                )
                val input = TransactionInput(fallbackTxId, nPeers + 1 + i)
                HeadPeerNumber(i) -> VoteUtxo(input, voteOutput)
            }.toMap

            // [SYNTHETIC] Default vote at output index 2*nPeers+1: key=0, link=1, pre-voted
            defaultVoteUtxo =
                val voteOutput = VoteOutput[VoteStatus.Voted](
                  BigInt(0),
                  BigInt(1),
                  env.headConfig.collectiveContingency.defaultVoteDeposit
                      + env.headConfig.collectiveContingency.minAdaForTreasury,
                  PositiveInt.unsafeApply(1),
                  VoteStatus.Voted(evacMap.kzgCommitment, BigInt(0))
                )
                VoteUtxo(TransactionInput(fallbackTxId, 2 * nPeers + 1), voteOutput)

            // [SYNTHETIC] ADA-only collateral UTxO per peer, at each peer's own head address.
            // Collateral outputs carry a fixed sentinel datum ("collateral") so the abstraction
            // function can identify them without decoding production-format datums.
            collateralUtxos <- Gen
                .sequence[List[(HeadPeerNumber, CollateralUtxo)], (HeadPeerNumber, CollateralUtxo)](
                  env.nodePrivateConfigs.toList.map { (peerId, peerConfig) =>
                      CommonGenerators
                          .genCollateralUtxo(
                            peerConfig.ownWallet.exportVerificationKey.addrKeyHash
                          )(using env.headConfig)
                          .map(peerId -> _)
                  }
                )
                .map(_.toMap)

            refScriptUtxos = env.nodeConfigs.values
                .flatMap(_.scriptReferenceUtxos.toList.map(_.toTuple))
                .toMap
        yield InitialDisputeUtxos(
          treasury,
          peerVoteUtxos,
          defaultVoteUtxo,
          collateralUtxos,
          refScriptUtxos,
          evacMap
        )
