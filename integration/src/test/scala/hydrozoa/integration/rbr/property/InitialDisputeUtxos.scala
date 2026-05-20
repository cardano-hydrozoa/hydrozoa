package hydrozoa.integration.rbr.property

// GODO:
//  Good morning, Peter here.
//  We're moving on to _property_ tests in the Rulebased regime. I've realized that these are distinct, and prior to,
//  the model-based tests in the following way:
//  - The model based tests go through generating _and evolving_ a `Model` and a `SUT` according to a sequence of c
//    commands...
//  - ...but property tests don't. They just generate and initial state and let things run. And this is exactly what
//     we want as the first step towards integration testing the DisputeActor, EvacuationActor, and RBR actor.
//     These actors run _autonomously_ -- without user input driving their behavior -- so commands are an unncessary
//     complexity for the first pass of testing.
//
//  However, we still want to share and re-use some of the previous things we built both for:
//  - effectful property testing (PropertyM, TestM, and MultiNodeConfigTestM)
//  - Mocks (CardanoBackendMock, TestControl)
//  - Ground-truth to model abstraction ideas (Abstraction.scala, still WIP)
//
//  Doing this sets up nicely for more advanced model-based tests where we can constrain the system and
//  view/validate/deterministically control the actual _path_ that the SUT takes from the initial state to a terminal
//  state via the petri net model.
//
//  But for now:
//  - We want a MultiNodeConfigTestM to initialize a dispute scenario with two peers:
//    - A DisputeActor genuinely instantiated within a TestControl actor system (as opposed to DisputeActorTest,
//    which does it purely)
//    - Each dispute actor should _share_ an associated mock cardano backend which has all the requisite utxos that
//      appear after a fallback transaction -- unresolved treasury, vote utxos, collateral utxos -- as well
//      as some utxos that each actor "owns" (at their own head address) and some random unspendable ambient utxos.
//      - They must share it so that they can see eachother's updates, but...
//        - Is the mock backend, as implemented, thread safe? Can two peers share it? Maybe we check this first.
//    - We also need a KZG setup and evacuation commitment shared across all actor in order to manage disputes.
//  - Then, the test should run both dispute actors autonomously. They should both run to "completion", but...
//    - What actually _is_ completion? The dispute actors just keep going in case rollbacks occur. We'd need to emit
//      some sort of message -- perhaps a log message, perhaps an actor message -- to signify that the dispute actor
//      as _successfully_ reached what it believes to be a terminal state.
//  - At the end of this, we should be able to query the cardano backend for its current `Utxos`, and then use
//    and abstraction function (similar to the one in Abstraction.scala) to do a disjoint-classification
//    (with AmbientPlaceId as default) of every utxo in the set.
//
//  Please leave this comment intact, and feel free to append below this line with a section marked "Claude" to remind
//  yourself of any WIP ideas, TODOs, future questions/comments/concerns/etc.

// --- Claude ---
//
// Generators for the synthetic post-fallback dispute initial state.
//
// Key design decisions:
// - The initial UTxO set is [SYNTHETIC]: constructed directly rather than via FallbackTx.Build.
//   This makes the tests more targeted and faster to construct at the cost of not exercising FallbackTx itself.
// - The voting deadline is passed in as a parameter (a QuantizedInstant read inside TestControl), so the
//   clock-aware test can anchor the deadline to the simulated time rather than the real wall clock.
// - Collateral UTxOs are generated per-peer at each peer's own head address, which is what DisputeActor queries
//   when it needs plutus script collateral.
// - Vote UTxOs follow the key/link chain established by VoteDatum.apply + VoteDatum.default:
//   - Default: key=0, link=1, pre-voted with the evacuation commitment
//   - Peer i (0-indexed): key=i+1, link=i+2 (or 0 for the last peer)
//   - Output indices mirror FallbackTx output ordering:
//     [0] treasury, [1..n] collaterals, [n+1..2n] peer votes, [2n+1] default vote

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

// All UTxOs that exist in the shared mock backend at the start of the dispute scenario.
// [SYNTHETIC] This mimics what FallbackTx.Build would produce, but is constructed directly.
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

    // The voting deadline is passed in from the TestControl-aware caller so it is anchored to the
    // simulated clock rather than System.currentTimeMillis().
    def gen(
        fallbackTxId: TransactionHash, // [SYNTHETIC] stands in for the real fallback tx hash
        now: QuantizedInstant, // simulated "now", read from inside TestControl
        votingDuration: FiniteDuration, // overrides env.votingDuration so tests can use short windows
        nEvacs: Int = 100, // [SYNTHETIC] number of entries in the evacuation map
    )(using env: MultiNodeConfig): Gen[InitialDisputeUtxos] =
        val nPeers = env.headConfig.nHeadPeers.convert
        val votingDeadline: BigInt = now.toPosixTime + votingDuration.toMillis

        // [SYNTHETIC] KZG G2 setup (reuses the same approach as the existing DisputeActorTest)
        val setup = TrustedSetup
            .takeSrsG2(64)
            .map(p2 => G2Element(p2).toCompressedByteString)

        val genEvacuationSentinelDatum: Gen[Option[DatumOption]] =
            Gen.const(Some(Inline(ByteString.fromString("evacuation").toData)))

        for

            evacMap <- genEvacuationMap(nEvacs, genDatum =  genEvacuationSentinelDatum)(using env.headConfig)

            treasuryValue = evacMap.totalValue
            
            // [SYNTHETIC] Treasury UTxO at output index 0 of the (synthetic) fallback tx
            treasury <- CommonGenerators.genRuleBasedTreasuryUtxo(
              fallbackTxId,
              Unresolved(
                deadlineVoting = votingDeadline,
                versionMajor = BigInt(1),
                setup = setup
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
                  Coin.ada(5),
                  PositiveInt.unsafeApply(1),
                  VoteStatus.Voted(evacMap.kzgCommitment, BigInt(0))
                )
                VoteUtxo(TransactionInput(fallbackTxId, 2 * nPeers + 1), voteOutput)

            // [SYNTHETIC] ADA-only collateral UTxO per peer, at each peer's own head address
            collateralUtxos <- Gen
                .sequence[List[(HeadPeerNumber, CollateralUtxo)], (HeadPeerNumber, CollateralUtxo)](
                  env.nodePrivateConfigs.toList.map { (peerId, peerConfig) =>
                      CommonGenerators
                          .genCollateralUtxo(
                            peerConfig.ownHeadWallet.exportVerificationKey.addrKeyHash
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
