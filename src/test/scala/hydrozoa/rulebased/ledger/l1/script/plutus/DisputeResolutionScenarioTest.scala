package hydrozoa.rulebased.ledger.l1.script.plutus

import cats.effect.unsafe.implicits.global
import cps.*
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.{addrKeyHash, pubKeyHash}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.rulebased.ledger.l1.DisputeActorTestHelpers.{mkBallotBoxUtxoPure, mkRuleBasedTreasuryPure}
import hydrozoa.rulebased.ledger.l1.state.StandaloneEvacuationCommitmentOnchain
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.Voted
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genCollateralUtxo
import hydrozoa.rulebased.ledger.l1.tx.{ResolutionTx, TallyTx, VoteTx}
import hydrozoa.rulebased.ledger.l1.utxo.BallotBox
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.{genByteStringOfN, given}
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.{CardanoMutator, State, UtxoEnv}
import scalus.testing.{ImmutableEmulator, Scenario}

/** Resolution-safety check for the rule-based dispute (test "A").
  *
  * Invariant: the dispute resolves to the **highest version actually voted, regardless of tally
  * order and regardless of which peers vote**. The ballot-box ring starts with key 0 carrying a
  * default `Voted(X1, v1)` (so a `Voted` survivor — and thus a resolvable head — always exists),
  * and keys 1..N-1 all start as `AwaitingVote`. The decision *whether each peer votes* is made
  * **inside the scenario**: for every awaiting box the `Scenario` monad branches (`choices(true,
  * false)`) — `true` submits a real [[VoteTx]] (`AwaitingVote -> Voted(X2, v2)`), `false` is the
  * peer that never calls submit, leaving its box `AwaitingVote` (it loses under `maxVote`: Voted >
  * AwaitingVote > Abstain).
  *
  * After voting, the scenario advances past the voting deadline (`Scenario.sleep`) — votes need
  * `to <= deadline`, the tally needs `from >= deadline` — re-reads the boxes, tallies the ring down
  * to one box (`tallyDown`, branching over every legal `(continuing, removed)` pair), and resolves.
  * The resolved treasury must commit to **X2 if any peer voted in that branch, else the default
  * X1** — asserted on **every** leaf.
  *
  * One scenario therefore explores all `2^(N-1)` vote subsets × `(N-1)!` tally orders. This is a
  * deterministic example test (not generative): the multi-peer fixture is materialized once from a
  * fixed seed — the house pattern for non-property tests that still need a [[MultiNodeConfig]] (cf.
  * `StackComposerRecoveryTest`); branching lives in the `Scenario` monad, not in ScalaCheck.
  *
  * Box count is `min(3, nHeadPeers + 1)` (capped low to keep the exhaustive fan-out cheap — 8
  * leaves; the 4+ box space is covered by a bounded-trace ScalaCheck Commands test). The final
  * tallied box must hold exactly `nHeadPeers + 1` vote tokens (Resolve checks it). Each vote
  * consumes-and-recreates its collateral (unlike `TallyTx`, which only references it), so every
  * awaiting box gets its **own** collateral utxo; the tally/resolve chain reuses one separate
  * collateral.
  */
class DisputeResolutionScenarioTest extends AnyFunSuite {

    /** A single, deterministic multi-peer fixture (seeded). */
    private val env =
        MultiNodeConfig.generateWithCoil().pureApply(Gen.Parameters.default, Seed(0L))

    /** Materialize a generator deterministically from a fixed seed. */
    private def fixed[A](gen: Gen[A], seed: Long): A =
        gen.pureApply(Gen.Parameters.default, Seed(seed))

    private val config: NodeConfig = env.nodeConfigs.head._2
    private val ownWallet: PeerWallet = env.nodePrivateConfigs.head._2.ownWallet
    private val ownKeyHash = ownWallet.exportVerificationKey.addrKeyHash
    private val peerPkh = ownWallet.exportVerificationKey.pubKeyHash
    private val disputeAddr: Address = HydrozoaBlueprint.mkDisputeAddress(env.headConfig.network)

    private val treasuryToken = Value.asset(
      env.headConfig.headMultisigScript.policyId,
      env.headConfig.headTokenNames.treasuryTokenName,
      1
    )
    private val fallbackTxId = fixed(Arbitrary.arbitrary[TransactionHash], 1)
    private val x1Commitment = fixed(genByteStringOfN(48), 2)
    private val x2Commitment = fixed(genByteStringOfN(48).suchThat(_ != x1Commitment), 3)
    private val versionMajor = BigInt(100)
    private val x2Minor = BigInt(2)
    private val now = realTimeQuantizedInstant(env.headConfig.slotConfig).unsafeRunSync()

    // Final tallied box must hold exactly nHeadPeers + 1 vote tokens (Resolve checks this), so the
    // box count is bounded by the token budget (each box needs >= 1 token).
    private val totalTokens = BigInt(env.headConfig.nHeadPeers.convert + 1)
    // Capped at 3 boxes to keep the exhaustive fan-out cheap: 2^2 vote subsets × 2! tally orders =
    // 8 leaves. The 4+ box space is covered by a bounded-trace ScalaCheck Commands test instead.
    private val numBoxes = math.min(3, totalTokens.toInt)

    // Treasury (Unresolved). Deadline in the FUTURE so votes (to <= deadline) are valid now and the
    // tally (from >= deadline) becomes valid once the scenario sleeps past it.
    private val treasury = mkRuleBasedTreasuryPure(
      versionMajor,
      treasuryToken + Value(Coin.ada(100)),
      TransactionInput(fallbackTxId, 0),
      votingDeadline = now.toPosixTime + 600_000
    )
    private val deadlineSlot: Slot = treasury.parseVotingDeadline(using config).toOption.get

    // One collateral per awaiting box (each vote consumes its own) plus one for the tally/resolve
    // chain (TallyTx only references collateral, so the chain can reuse a single one).
    private val voteCollaterals: List[CollateralUtxo] =
        (0 until numBoxes - 1).toList.map(i =>
            fixed(genCollateralUtxo(ownKeyHash)(using env.headConfig), 10L + i)
        )
    private val collateral = fixed(genCollateralUtxo(ownKeyHash)(using env.headConfig), 4)

    // The standalone evacuation commitment every voter signs (version X2 = (versionMajor, 2),
    // committing to x2Commitment). The on-chain Vote check verifies the all-peer multisig over it.
    private val sec = StandaloneEvacuationCommitmentOnchain(
      headId = env.headConfig.headTokenNames.treasuryTokenName.bytes,
      versionMajor = versionMajor,
      versionMinor = x2Minor,
      commitment = x2Commitment
    )
    private val signatures = env.multisignHeader(sec).toList
    private val coilSignatures = env.multisignHeaderCoil(sec)

    // Ring 0 -> 1 -> ... -> (numBoxes-1) -> 0.
    //  - key 0 = default Voted(X1, 1) (guarantees a Voted survivor; absorbs the token slack).
    //  - keys 1.. = AwaitingVote (each may or may not vote, decided inside the scenario).
    private val boxStatuses: List[VoteStatus] =
        Voted(x1Commitment, 1) :: List.fill(numBoxes - 1)(VoteStatus.AwaitingVote(peerPkh))
    private val boxTokens: List[BigInt] =
        (totalTokens - (numBoxes - 1)) :: List.fill(numBoxes - 1)(BigInt(1))

    private val boxUtxos: List[Utxo] = (0 until numBoxes).toList.map { i =>
        mkBallotBoxUtxoPure(
          env.headConfig,
          BigInt(i),
          BigInt((i + 1) % numBoxes),
          boxStatuses(i),
          TransactionInput(fallbackTxId, i + 1),
          nVoteTokens = boxTokens(i)
        )
    }

    private val initialUtxos: Utxos = (
      Map(
        (treasury.utxoId, treasury.treasuryOutput.toOutput(using config)),
        (collateral.input, collateral.collateralOutput.toOutput(using env))
      )
          ++ voteCollaterals.map(c => c.input -> c.collateralOutput.toOutput(using env))
          ++ boxUtxos.map(u => u.input -> u.output)
          ++ config.scriptReferenceUtxos.toList.map(_.toTuple)
    )

    /** The awaiting boxes (keys 1..), each paired with its own collateral for voting. */
    private val awaitingBoxes: List[(BallotBox[VoteStatus.AwaitingVote], CollateralUtxo)] =
        boxUtxos.tail
            .map { u =>
                BallotBox
                    .parse(u)(using config)
                    .fold(e => throw new AssertionError(e.toString), identity)
                    .asInstanceOf[BallotBox[VoteStatus.AwaitingVote]]
            }
            .zip(voteCollaterals)

    /** Voting phase: for each awaiting box, branch on vote / don't-vote. A vote submits a real
      * [[VoteTx]] (signed by the box's peer, spending the box's own collateral), turning it into
      * `Voted(X2)`; not voting leaves it `AwaitingVote`. Returns whether *any* box voted on this
      * branch.
      */
    private def voteEach(
        boxes: List[(BallotBox[VoteStatus.AwaitingVote], CollateralUtxo)],
        anyVoted: Boolean
    ): Scenario[Boolean] =
        async[Scenario] {
            boxes match {
                case Nil => anyVoted
                case (box, voteCollateral) :: rest =>
                    val doVote = Scenario.choices(true, false).await
                    val votedNow =
                        if doVote then {
                            val voteTx = VoteTx
                                .Build(
                                  box,
                                  treasury,
                                  voteCollateral,
                                  sec,
                                  signatures,
                                  coilSignatures
                                )
                                .result(using config)
                                .toOption
                                .get
                            val r = Scenario.submit(ownWallet.signTx(voteTx.tx)).await
                            Scenario.guard(r.isRight).await
                            true
                        } else false
                    voteEach(rest, anyVoted || votedNow).await
            }
        }

    /** The full scenario: vote (branched) -> sleep past the deadline -> tally down (branched) ->
      * resolve. Yields the resolved datum paired with whether this branch cast any vote.
      */
    private val scenario: Scenario[(RuleBasedTreasuryDatum, Boolean)] =
        async[Scenario] {
            val anyVoted = voteEach(awaitingBoxes, false).await
            val cur = Scenario.now.await
            val target = deadlineSlot.slot + 1
            if target > cur then Scenario.sleep(target - cur).await
            val emu = Scenario.currentEmulator.await
            val datum = tallyDown(allBoxesAt(emu, disputeAddr)).await
            (datum, anyVoted)
        }

    test("resolution resolves to highest voted version, any tally order and any vote subset") {
        val emulator0 = mkEmulator(initialUtxos, now.toSlot)
        val results =
            Await.result(Scenario.runAll(emulator0.toEmulator)(scenario), Duration(600, "s"))

        // 2^(N-1) vote subsets × (N-1)! tally orders.
        val expectedBranches = (1 << (numBoxes - 1)) * (1 until numBoxes).product
        val _ = assert(
          results.size == expectedBranches,
          s"expected $expectedBranches leaves (2^${numBoxes - 1} vote subsets × " +
              s"${numBoxes - 1}! tally orders), got ${results.size}"
        )
        val _ = assert(
          results.forall { case (_, (datum, anyVoted)) =>
              datum match {
                  case RuleBasedTreasuryDatum.Resolved(c, (_, m), _) =>
                      if anyVoted then c == x2Commitment && m == x2Minor
                      else c == x1Commitment && m == BigInt(1)
                  case _ => false
              }
          },
          "each leaf must resolve to X2 iff it cast any vote, else the default X1; " +
              s"got ${results.map(_._2._2).groupBy(identity).view.mapValues(_.size).toMap}"
        )
    }

    /** Mirror the Mock execution model: single CardanoMutator under EvaluateAndComputeCost. */
    private def mkEmulator(initialUtxos: Utxos, slot: Slot): ImmutableEmulator =
        ImmutableEmulator(
          state = State(utxos = initialUtxos),
          env = UtxoEnv(
            slot.slot,
            env.headConfig.cardanoProtocolParams,
            certState = CertState.empty,
            env.headConfig.network
          ),
          slotConfig = env.headConfig.slotConfig,
          evaluatorMode = EvaluateAndComputeCost,
          validators = Seq.empty,
          mutators = Seq(CardanoMutator)
        )

    /** All ballot boxes currently at `address` in the emulator's UTxO set. */
    private def allBoxesAt(
        emulator: ImmutableEmulator,
        address: Address
    ): List[BallotBox[VoteStatus]] =
        emulator.utxos.toList
            .filter((_, o) => o.address == address)
            .flatMap((i, o) => BallotBox.parse(Utxo(i, o))(using config).toOption)

    /** The ballot box with the given `key` currently at `address` in the emulator's UTxO set. */
    private def findBoxByKey(
        emulator: ImmutableEmulator,
        address: Address,
        key: BigInt
    ): Option[BallotBox[VoteStatus]] =
        allBoxesAt(emulator, address).find(_.ballotBoxOutput.key == key)

    /** Valid `(continuing, removed)` tally pairs among `boxes`: the removed box's key must equal
      * the continuing box's link and be strictly greater (the linked-list contraction rule). The
      * Scenario branches over these — so the choice is *which boxes to tally*, not a label, and it
      * enumerates every valid tally order by construction.
      */
    private def tallyPairs(
        boxes: List[BallotBox[VoteStatus]]
    ): List[(BallotBox[VoteStatus], BallotBox[VoteStatus])] =
        for {
            cont <- boxes
            rem <- boxes
            if rem.ballotBoxOutput.key == cont.ballotBoxOutput.link
                && rem.ballotBoxOutput.key > cont.ballotBoxOutput.key
        } yield (cont, rem)

    /** Recursive contraction: branch over every legal tally pair at each step, thread the emulator
      * forward, and resolve the single survivor. Returns the resolved datum on each branch.
      */
    private def tallyDown(bs: List[BallotBox[VoteStatus]]): Scenario[RuleBasedTreasuryDatum] =
        async[Scenario] {
            if bs.size == 1 then {
                val resolution = ResolutionTx
                    .Build(bs.head.asInstanceOf[BallotBox[Voted]], treasury, collateral)(using
                      config
                    )
                    .result
                    .toOption
                    .get
                val r = Scenario.submit(ownWallet.signTx(resolution.tx)).await
                Scenario.guard(r.isRight).await
                resolution.treasuryResolvedUtxoProduced.treasuryOutput.datum
            } else {
                val (cont, rem) = Scenario.fromCollection(tallyPairs(bs)).await
                val tally = TallyTx
                    .Build(cont, rem, treasury, collateral)(using config)
                    .result
                    .toOption
                    .get
                val r = Scenario.submit(ownWallet.signTx(tally.tx)).await
                Scenario.guard(r.isRight).await
                val emu = Scenario.currentEmulator.await
                val survivor = findBoxByKey(emu, disputeAddr, cont.ballotBoxOutput.key).get
                val rest = bs.filterNot { b =>
                    b.ballotBoxOutput.key == cont.ballotBoxOutput.key ||
                    b.ballotBoxOutput.key == rem.ballotBoxOutput.key
                }
                tallyDown(survivor :: rest).await
            }
        }
}
