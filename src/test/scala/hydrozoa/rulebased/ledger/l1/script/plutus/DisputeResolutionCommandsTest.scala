package hydrozoa.rulebased.ledger.l1.script.plutus

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.{addrKeyHash, pubKeyHash}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.lib.number.PositiveInt.given
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.rulebased.ledger.l1.DisputeTestFixtures.{mkBallotBoxUtxoPure, mkRegimeUtxoPure, mkRuleBasedTreasuryPure}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer}
import hydrozoa.rulebased.ledger.l1.state.StandaloneEvacuationCommitmentOnchain
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.{AwaitingVote, Voted}
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genCollateralUtxo
import hydrozoa.rulebased.ledger.l1.tx.{ResolutionTx, TallyTx, VoteTx}
import hydrozoa.rulebased.ledger.l1.utxo.BallotBox
import org.scalacheck.commands.Commands
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import scala.collection.immutable.TreeSet
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.{genByteStringOfN, given}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{CardanoMutator, State, UtxoEnv}
import scalus.cardano.node.{BlockchainReader, UtxoSource}
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.testing.{ContractStepVariations, ContractTestActor, ImmutableEmulator, StepAction}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.{fromData, toData}

/** Bounded-trace ScalaCheck Commands exploration of the rule-based dispute (test "D").
  *
  * This is the random-trace companion to [[DisputeResolutionScenarioTest]] (test A). Test A
  * **exhaustively** enumerates every `2^(N-1)` vote subset × `(N-1)!` tally order with an exact
  * model oracle, but pays a full BLS resolution per leaf — so it is capped at `min(3, …)` boxes (8
  * leaves). This test instead drives the **same** dispute flow as a ScalaCheck `Commands` state
  * machine and lets ScalaCheck **sample** random command sequences, which scales to `min(4, …)`
  * boxes (the 4-box space is `2^3 × 3! = 48` leaves — already expensive to enumerate, cheap to
  * sample).
  *
  * Candidate transactions are generated with scalus-testkit's [[ContractStepVariations]] /
  * [[ContractTestActor]] (`step`, below), but the `Commands` machine itself is hand-rolled rather
  * than [[scalus.testing.ContractScalaCheckCommands]]: that adapter's Sut is the mutable
  * `scalus.cardano.node.Emulator`, whose `setSlot` drops `evaluatorMode` back to `Validate` (a
  * scalus bug — see [[EmuHolder]]), under which the BLS `ResolutionTx` is rejected. Here both the
  * model `State` and the `Sut` are an [[ImmutableEmulator]] (which preserves the mode), so
  * resolution is actually exercised.
  *
  * The fixture mirrors test A: a ballot-box ring whose key 0 carries a default `Voted(X1, 1)` (a
  * `Voted` survivor — and thus a resolvable head — always exists) and whose keys 1..N-1 start
  * `AwaitingVote`. Three kinds of [[ContractTestActor]] interact with the emulator:
  *   - a per-box **vote** actor (submits a real [[VoteTx]] `AwaitingVote -> Voted(X2, 2)` while the
  *     deadline has not passed),
  *   - a **tally** actor (offers a [[TallyTx]] for every legal `(continuing, removed)` pair once
  *     the deadline has passed), and
  *   - a **resolve** actor (submits a [[ResolutionTx]] once a single `Voted` box remains).
  *
  * An **attacker** actor additionally injects abuse variants while voting is open, all derived from
  * an otherwise-valid vote: forge a version/commitment the peers never multisigned, redirect the
  * box to the attacker's own address, **skim its vote token** into the attacker's output (a
  * re-balanced value-changing attack), or **inflate its weight** by minting an extra vote token
  * onto the box. Each is kept phase-1 balanced so the real defense — not a phase-1 fee/balance
  * check — does the rejecting (the value attacks by the dispute script's value-preservation check;
  * the inflation by the head's minting policy, which an attacker cannot satisfy). A ledger
  * rejection is the expected, passing outcome; were one accepted, the safety invariants below are
  * its oracle — a forged version trips version-legitimacy, a redirected/skimmed/minted token trips
  * token conservation.
  *
  * A **tally-phase** attacker mutates the surviving box of an otherwise-valid [[TallyTx]] (which
  * merges `rem` into `cont`): forge its merged version to a non-castable one (rejected by the
  * dispute script's `HighestVoteCheck` — the survivor must equal `maxVote(cont, rem)`) or redirect
  * it to the attacker (rejected by the continuing-output address/value check). An accepted forge
  * trips version-legitimacy; an accepted redirect trips token conservation.
  *
  * Two **resolve-phase** abuses fire on the [[ResolutionTx]] — the one transaction that co-spends
  * the tallied ballot box (dispute script) AND the still-Unresolved treasury (treasury script):
  *   - '''cross-script evacuate-during-resolve''': swap the treasury input's `Resolve` redeemer for
  *     an `Evacuate` redeemer, trying to drain the treasury in the same tx that resolves the
  *     dispute (recomputing the script-data hash and bumping the fee so the *treasury script*, not
  *     a phase-1 check, does the rejecting). The treasury `Evacuate` branch hard-requires a
  *     Resolved datum and rejects; an accepted one would leave the treasury's address empty
  *     mid-resolution, caught by the (strengthened) resolution invariant.
  *   - '''different evacuation map''': forge the resolved treasury output's `evacuationActive` to a
  *     commitment nobody voted for. The treasury script requires it to equal the winning vote's
  *     commitment and rejects; an accepted one would commit to a non-castable map, caught by
  *     resolution-legitimacy.
  *
  * A slot delay that crosses the voting deadline is always offered, so a trace naturally
  * interleaves a random vote subset (pre-deadline) with a random tally order (post-deadline) before
  * resolving — exactly test A's space, sampled rather than enumerated.
  *
  * After every accepted transaction the following **structural safety invariants** are checked
  * (each is local to the current on-chain state — no trace history needed):
  *   1. '''Token conservation''' — while ballot boxes remain, their vote tokens sum to the original
  *      total. Combined with `Resolve`'s own check that the survivor holds exactly `nHeadPeers + 1`
  *      tokens, this means no peer's box can be silently dropped during tallying.
  *   2. '''Version legitimacy''' — every `Voted` box commits to one of the two castable versions
  *      (`(X1, 1)` or `(X2, 2)`); no tally fabricates a third.
  *   3. '''Resolution legitimacy''' — once a resolved treasury appears it commits to a legitimate
  *      version and the ballot boxes are fully consumed.
  *
  * The exact "resolves to the highest version actually voted" oracle stays in test A (it needs the
  * per-branch model of whether each peer voted, which a chain-derived state cannot recover after
  * resolution). The two tests are complementary: A = exact oracle, exhaustive, small; D =
  * structural safety, sampled, larger and arbitrarily interleaved.
  */
object DisputeResolutionCommandsTest extends Properties("RBR Dispute Resolution Commands") {

    private given ExecutionContext = ExecutionContext.global

    // ScalaCheck knobs (tunable): how many random traces to run, and the max trace length. A trace
    // votes a random subset (pre-deadline), crosses the deadline, tallies in a random order, and
    // resolves — at most (N-1) votes + 1 wait + (N-1) tallies + 1 resolve of "real" work, padded
    // with no-op slot advances. Kept modest because each tally/resolve runs a BLS-heavy script.
    private val numTraces = 15
    private val maxTraceLength = 20

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withMinSuccessfulTests(numTraces)
            .withMaxSize(maxTraceLength)
            .withMaxDiscardRatio(50)

    /** A single, deterministic multi-peer fixture (seeded) — the house pattern (cf. test A). */
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
    private val treasuryAddr: Address = HydrozoaBlueprint.mkTreasuryAddress(env.headConfig.network)

    private val treasuryToken = env.headConfig.treasuryToken
    private val fallbackTxId = fixed(Arbitrary.arbitrary[TransactionHash], 1)
    private val x1Commitment = fixed(genByteStringOfN(48), 2)
    private val x2Commitment = fixed(genByteStringOfN(48).suchThat(_ != x1Commitment), 3)
    // A commitment that was never multisigned (neither X1 nor X2) — used by the abuse variations.
    private val forgedCommitment =
        fixed(genByteStringOfN(48).suchThat(c => c != x1Commitment && c != x2Commitment), 5)
    private val versionMajor = BigInt(100)
    private val x2Minor = BigInt(2)
    private val now = realTimeQuantizedInstant(env.headConfig.slotConfig).unsafeRunSync()

    // Final tallied box must hold exactly nHeadPeers + 1 vote tokens (Resolve checks this), so the
    // box count is bounded by the token budget (each box needs >= 1 token). Capped at 4 (vs A's 3):
    // the 48-leaf, 4-box space is sampled here, not enumerated.
    private val totalTokens = BigInt(env.headConfig.nHeadPeers.convert + 1)
    private val numBoxes = math.min(4, totalTokens.toInt)

    // Treasury (Unresolved). Deadline in the FUTURE so votes (to <= deadline) are valid now and the
    // tally (from >= deadline) becomes valid once a slot delay crosses it. The Unresolved treasury
    // is only *referenced* by VoteTx/TallyTx (consumed by ResolutionTx), so it stays a valid static
    // reference until resolution — captured here rather than re-extracted each step.
    private val treasury = mkRuleBasedTreasuryPure(
      env.headConfig,
      versionMajor,
      treasuryToken + Value(Coin.ada(100)),
      TransactionInput(fallbackTxId, 0),
      votingDeadline = now.toPosixTime + 600_000
    )
    private val deadlineSlot: Slot = treasury.parseVotingDeadline(using config).toOption.get

    // The regime utxo (HRWT beacon + head-identity datum) referenced by every dispute-flow tx.
    private val regimeUtxo =
        mkRegimeUtxoPure(TransactionInput(fixed(Arbitrary.arbitrary[TransactionHash], 6), 0))

    // A single slot delay this large always crosses the voting deadline from the start slot. Offered
    // as the [[scalus.testing.StepAction.Wait]] that moves a trace from the voting phase to the
    // tally phase. (The framework's slot-advance command does not re-extract our model, so actors
    // read the *live* slot from the reader rather than a model flag — see `slotPast`.)
    private val votingWindowSlots: Long = (deadlineSlot.slot - now.toSlot.slot) + 1

    // One collateral per awaiting box (each vote consumes its own) plus one for the tally/resolve
    // chain (TallyTx only references collateral, so the chain can reuse a single one).
    private val voteCollaterals: List[CollateralUtxo] =
        (0 until numBoxes - 1).toList.map(i =>
            fixed(genCollateralUtxo(ownKeyHash)(using env.headConfig), 10L + i)
        )
    private val collateral = fixed(genCollateralUtxo(ownKeyHash)(using env.headConfig), 4)

    /** The collateral dedicated to the vote on box `key` (keys 1..N-1 -> voteCollaterals 0..N-2).
      */
    private def voteCollateralFor(key: BigInt): CollateralUtxo = voteCollaterals(key.toInt - 1)

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
    //  - keys 1.. = AwaitingVote (each may or may not vote across a trace).
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
        (collateral.input, collateral.collateralOutput.toOutput(using env)),
        regimeUtxo.toUtxo(using env.headConfig).toTuple
      )
          ++ voteCollaterals.map(c => c.input -> c.collateralOutput.toOutput(using env))
          ++ boxUtxos.map(u => u.input -> u.output)
          ++ config.scriptReferenceUtxos.toList.map(_.toTuple)
    )

    /** Abstract model of the dispute, extracted from the chain after each accepted transaction.
      *
      * Note: the slot is deliberately NOT part of the model — the framework's slot-advance command
      * does not re-extract this state, so a model slot would go stale after a `Wait`. Actors read
      * the live slot from the reader instead; the slot delay is gated on `boxes`/`resolved` here.
      *
      * @param boxes
      *   ballot boxes currently at the dispute address
      * @param resolved
      *   the resolved treasury's `(commitment, versionMinor)`, once a resolved treasury exists
      */
    private case class DisputeModel(
        boxes: List[BallotBox[VoteStatus]],
        resolved: Option[(ByteString, BigInt)]
    )

    /** Ballot boxes currently at the dispute address, parsed from the reader's UTxO set. */
    private def boxesAt(reader: BlockchainReader)(using
        ExecutionContext
    ): Future[List[BallotBox[VoteStatus]]] =
        reader
            .findUtxos(UtxoSource.FromAddress(disputeAddr))
            .map(_.getOrElse(Map.empty))
            .map(_.toList.flatMap((i, o) => BallotBox.parse(Utxo(i, o))(using config).toOption))

    /** The resolved treasury's `(commitment, versionMinor)`, if a resolved treasury exists. */
    private def resolvedAt(reader: BlockchainReader)(using
        ExecutionContext
    ): Future[Option[(ByteString, BigInt)]] =
        reader
            .findUtxos(UtxoSource.FromAddress(treasuryAddr))
            .map(_.getOrElse(Map.empty))
            .map(
              _.values.toList
                  .flatMap { out =>
                      out.datumOption match {
                          case Some(Inline(d)) =>
                              scala.util
                                  .Try(fromData[RuleBasedTreasuryDatum](d))
                                  .toOption
                                  .collect { case RuleBasedTreasuryDatum.Resolved(_, c, (_, m)) =>
                                      (c, m)
                                  }
                          case _ => None
                      }
                  }
                  .headOption
            )

    private def extractState(reader: BlockchainReader)(using
        ExecutionContext
    ): Future[DisputeModel] =
        for {
            boxes <- boxesAt(reader)
            resolved <- resolvedAt(reader)
        } yield DisputeModel(boxes, resolved)

    /** Whether the live emulator slot (read from the reader) is past the voting deadline. */
    private def slotPast(reader: BlockchainReader)(using ExecutionContext): Future[Boolean] =
        reader.currentSlot.map(_ > deadlineSlot.slot)

    /** Vote actor for box `key`: while voting is open, if that box is still awaiting a vote, submit
      * a real [[VoteTx]] (signed by the box's peer, spending the box's own collateral) flipping it
      * to `Voted(X2, 2)`.
      */
    private def voteActor(key: BigInt): ContractTestActor[DisputeModel] =
        ContractTestActor.simple(
          s"vote-$key",
          (reader, s) =>
              slotPast(reader).map { past =>
                  if past then None
                  else
                      s.boxes
                          .find(b =>
                              b.ballotBoxOutput.key == key
                                  && b.ballotBoxOutput.status.isInstanceOf[AwaitingVote]
                          )
                          .flatMap { b =>
                              VoteTx
                                  .Build(
                                    b.asInstanceOf[BallotBox[AwaitingVote]],
                                    treasury,
                                    regimeUtxo,
                                    voteCollateralFor(key),
                                    sec,
                                    signatures,
                                    coilSignatures
                                  )
                                  .result(using config)
                                  .toOption
                                  .map(v => ownWallet.signTx(v.tx))
                          }
              }
        )

    /** Tally actor: once the deadline has passed, offer a [[TallyTx]] for every legal `(continuing,
      * removed)` pair (the framework picks one per step, so this enumerates every tally order).
      */
    private val tallyActor: ContractTestActor[DisputeModel] =
        ContractTestActor.multi(
          "tally",
          (reader, s) =>
              slotPast(reader).map { past =>
                  if !past then Seq.empty
                  else
                      tallyPairs(s.boxes).flatMap { (cont, rem) =>
                          TallyTx
                              .Build(cont, rem, treasury, regimeUtxo, collateral)(using config)
                              .result
                              .toOption
                              .map(t => ownWallet.signTx(t.tx))
                      }
              }
        )

    /** Resolve actor: once a single `Voted` box remains (past the deadline), submit a
      * [[ResolutionTx]].
      */
    private val resolveActor: ContractTestActor[DisputeModel] =
        ContractTestActor.simple(
          "resolve",
          (reader, s) =>
              slotPast(reader).map { past =>
                  if !past then None
                  else
                      s.boxes match {
                          case List(b) if b.ballotBoxOutput.status.isInstanceOf[Voted] =>
                              ResolutionTx
                                  .Build(
                                    b.asInstanceOf[BallotBox[Voted]],
                                    treasury,
                                    regimeUtxo,
                                    collateral
                                  )(using config)
                                  .result
                                  .toOption
                                  .map(rx => ownWallet.signTx(rx.tx))
                          case _ => None
                      }
              }
        )

    // --- Phase 1 abuse variations -------------------------------------------------------------
    // Each attack takes an otherwise-valid VoteTx, mutates ONLY its continuing ballot-box output
    // (so the transaction stays phase-1 balanced) and re-signs. The dispute script must reject it;
    // were any accepted, a safety invariant below would fire — that invariant is the attack's oracle.

    /** An address the attacker controls (its own key address), to redirect a stolen ballot box to.
      */
    private val attackerAddr: Address = collateral.collateralOutput.toOutput(using env).address

    /** A `Value` holding exactly one vote token (no ADA) — the unit moved by the value-skim attack.
      */
    private val voteTokenValue: Value =
        Value.asset(
          env.headConfig.headMultisigScript.policyId,
          env.headConfig.headTokenNames.voteTokenName,
          1
        )

    /** Rewrite every Babbage output at `addr` via `f`. */
    private def mapOutputAt(tx: Transaction, addr: Address)(f: Babbage => Babbage): Transaction = {
        val body = tx.body.value
        val outputs = body.outputs.map { sized =>
            sized.value match {
                case b: Babbage if b.address == addr => Sized[TransactionOutput](f(b))
                case _                               => sized
            }
        }
        tx.copy(body = KeepRaw(body.copy(outputs = outputs)))
    }

    /** Rewrite the single continuing ballot-box output (the one at the dispute address) via `f`. */
    private def mapVoteOutput(tx: Transaction)(f: Babbage => Babbage): Transaction =
        mapOutputAt(tx, disputeAddr)(f)

    private def setVoteDatum(d: VoteDatum)(tx: Transaction): Transaction =
        mapVoteOutput(tx)(_.copy(datumOption = Some(Inline(toData(d)))))

    /** Value-changing attack with re-balancing: move the ballot box's vote token OUT of the
      * continuing output and INTO the attacker's (collateral) output, leaving the transaction
      * balanced. The continuing output's value no longer matches `voteInput.value`, so the script
      * rejects (`VoteVoteOutputExists`); if it were accepted, the dispute address would be a vote
      * token short — token-conservation fires.
      */
    private def skimVoteToken(tx: Transaction): Transaction = {
        val body = tx.body.value
        val attackerIdx = body.outputs.indexWhere(_.value match {
            case b: Babbage => b.address == attackerAddr
            case _          => false
        })
        val outputs = body.outputs.zipWithIndex.map { case (sized, i) =>
            sized.value match {
                case b: Babbage if b.address == disputeAddr =>
                    Sized[TransactionOutput](b.copy(value = b.value - voteTokenValue))
                case b: Babbage if i == attackerIdx =>
                    Sized[TransactionOutput](b.copy(value = b.value + voteTokenValue))
                case _ => sized
            }
        }
        tx.copy(body = KeepRaw(body.copy(outputs = outputs)))
    }

    /** Vote-inflation attack: mint one extra vote token and pile it onto the continuing ballot-box
      * output, so the box would carry TWO vote tokens — inflating that peer's vote weight. The
      * transaction stays balanced (the mint matches the extra output token). It must be rejected:
      * minting vote tokens needs the head multisig policy, and the vote script also requires the
      * continuing output to preserve `voteInput.value`. If it were accepted, the dispute address
      * would carry more than the minted total and token-conservation fires.
      */
    private def inflateVote(tx: Transaction): Transaction = {
        val body = tx.body.value
        val mint = Mint(body.mint.fold(voteTokenValue.assets)(_ + voteTokenValue.assets))
        // The mint + extra output token grow the tx, raising the min fee; bump it (charged to the
        // attacker's collateral output) so the mint policy / vote script — not a phase-1 fee check —
        // does the rejecting.
        val feeBump = Coin.ada(2)
        val attackerIdx = body.outputs.indexWhere(_.value match {
            case b: Babbage => b.address == attackerAddr
            case _          => false
        })
        val outputs = body.outputs.zipWithIndex.map { case (sized, i) =>
            sized.value match {
                case b: Babbage if b.address == disputeAddr =>
                    Sized[TransactionOutput](b.copy(value = b.value + voteTokenValue))
                case b: Babbage if i == attackerIdx =>
                    Sized[TransactionOutput](b.copy(value = b.value - Value(feeBump)))
                case _ => sized
            }
        }
        tx.copy(body =
            KeepRaw(body.copy(outputs = outputs, mint = Some(mint), fee = body.fee + feeBump))
        )
    }

    /** All vote-abuse variants for `box`: forge a higher (un-multisigned) version, forge a foreign
      * commitment, redirect the ballot box to the attacker's address, skim its vote token into the
      * attacker's output, and inflate its vote weight by minting an extra token.
      */
    private def voteAttacks(box: BallotBox[AwaitingVote]): List[Transaction] =
        VoteTx
            .Build(
              box,
              treasury,
              regimeUtxo,
              voteCollateralFor(box.ballotBoxOutput.key),
              sec,
              signatures,
              coilSignatures
            )
            .result(using config)
            .toOption
            .toList
            .flatMap { vt =>
                val k = box.ballotBoxOutput.key
                val l = box.ballotBoxOutput.link
                List[Transaction => Transaction](
                  setVoteDatum(VoteDatum(k, l, Voted(x2Commitment, BigInt(3)))),
                  setVoteDatum(VoteDatum(k, l, Voted(forgedCommitment, x2Minor))),
                  tx => mapVoteOutput(tx)(_.copy(address = attackerAddr)),
                  skimVoteToken,
                  inflateVote
                ).map(mutate => ownWallet.signTx(mutate(vt.tx)))
            }

    /** Attacker actor: while voting is open, submit abuse variants for one awaiting box. Each must
      * be rejected by the dispute script (the Commands postcondition treats a rejection as a pass);
      * acceptance of any would be caught by token-conservation or version-legitimacy.
      */
    private val attackerActor: ContractTestActor[DisputeModel] =
        ContractTestActor.multi(
          "attacker",
          (reader, s) =>
              slotPast(reader).map { past =>
                  if past then Seq.empty
                  else
                      s.boxes
                          .collectFirst {
                              case b if b.ballotBoxOutput.status.isInstanceOf[AwaitingVote] =>
                                  b.asInstanceOf[BallotBox[AwaitingVote]]
                          }
                          .toList
                          .flatMap(voteAttacks)
              }
        )

    /** Cross-script "evacuate during resolve" confusion. The [[ResolutionTx]] is the one
      * transaction that co-spends the tallied ballot box (dispute script) AND the still-Unresolved
      * treasury (treasury script). This takes that transaction and swaps the treasury input's
      * `Resolve` redeemer for an `Evacuate` redeemer — attempting to drain the treasury in the same
      * tx that resolves the dispute. The treasury script's `Evacuate` branch hard-requires a
      * Resolved datum, so it must reject; were it accepted, the treasury would vanish from its
      * address while the boxes are consumed, which the (strengthened) resolution invariant catches.
      */
    private def evacuateDuringResolve(box: BallotBox[Voted]): Option[Transaction] =
        ResolutionTx
            .Build(box, treasury, regimeUtxo, collateral)(using config)
            .result
            .toOption
            .map { rt =>
                val tx0 = rt.tx
                val b0 = tx0.body.value
                // Swap the treasury input's Resolve redeemer for Evacuate.
                val treasuryIdx = b0.inputs.toIndexedSeq.indexOf(treasury.utxoId)
                val evacuateData =
                    toData(
                      TreasuryRedeemer.Evacuate(
                        EvacuateRedeemer(SList.Nil, forgedCommitment, BigInt(0))
                      )
                    )
                val ws0 = tx0.witnessSetRaw.value
                val swapped = ws0.redeemers.toList.flatMap(_.value.toSeq).map { r =>
                    if r.tag == RedeemerTag.Spend && r.index == treasuryIdx then
                        r.copy(data = evacuateData)
                    else r
                }
                val ws1 = ws0.copy(redeemers = Some(KeepRaw(Redeemers.from(swapped))))
                // Two phase-1 checks would otherwise reject this for the WRONG reason — we want the
                // treasury script to do the rejecting: (1) the swapped redeemer invalidates the
                // body's scriptDataHash, so recompute it; (2) the larger Evacuate redeemer raises the
                // min fee, so bump the fee, charged to the attacker's own collateral output.
                val newHash = ScriptDataHashGenerator.computeScriptDataHash(
                  ws1,
                  env.headConfig.cardanoProtocolParams,
                  TreeSet(Language.PlutusV3),
                  ws1.redeemers,
                  ws1.plutusData
                )
                val feeBump = Coin.ada(2)
                val attackerOutIdx = b0.outputs.indexWhere(_.value match {
                    case b: Babbage => b.address == attackerAddr
                    case _          => false
                })
                val outs = b0.outputs.zipWithIndex.map { case (sized, i) =>
                    sized.value match {
                        case b: Babbage if i == attackerOutIdx =>
                            Sized[TransactionOutput](b.copy(value = b.value - Value(feeBump)))
                        case _ => sized
                    }
                }
                val b1 = b0.copy(outputs = outs, fee = b0.fee + feeBump, scriptDataHash = newHash)
                ownWallet.signTx(tx0.copy(body = KeepRaw(b1), witnessSetRaw = KeepRaw(ws1)))
            }

    /** "Different evacuation map" attack: forge the resolved treasury's `evacuationActive` to a
      * commitment nobody voted for, substituting a different evacuation map at resolution. The
      * treasury script requires `evacuationActive === the winning vote's commitment`
      * (`ResolveUtxoActiveCheck`), so it must reject; were it accepted, the resolved treasury would
      * commit to a map nobody chose, which resolution-legitimacy (commitment ∈ {X1, X2}) catches.
      * (Inline output datums are not covered by the script-data hash, so no recompute is needed.)
      */
    private def forgeResolvedMap(box: BallotBox[Voted]): Option[Transaction] =
        ResolutionTx
            .Build(box, treasury, regimeUtxo, collateral)(using config)
            .result
            .toOption
            .map { rt =>
                val mutated = mapOutputAt(rt.tx, treasuryAddr) { b =>
                    b.datumOption match {
                        case Some(Inline(d)) =>
                            fromData[RuleBasedTreasuryDatum](d) match {
                                case r: RuleBasedTreasuryDatum.Resolved =>
                                    val forged: RuleBasedTreasuryDatum =
                                        r.copy(evacuationActive = forgedCommitment)
                                    b.copy(datumOption = Some(Inline(toData(forged))))
                                case _ => b
                            }
                        case _ => b
                    }
                }
                ownWallet.signTx(mutated)
            }

    /** Resolve-phase attacker: once a single `Voted` box remains, attempt the cross-script
      * evacuate-during-resolve and the substitute-a-different-evacuation-map attack. Both must be
      * rejected by the treasury script.
      */
    private val resolveAttacker: ContractTestActor[DisputeModel] =
        ContractTestActor.multi(
          "resolve-attacker",
          (reader, s) =>
              slotPast(reader).map { past =>
                  if !past then Seq.empty
                  else
                      s.boxes match {
                          case List(b) if b.ballotBoxOutput.status.isInstanceOf[Voted] =>
                              val voted = b.asInstanceOf[BallotBox[Voted]]
                              List(evacuateDuringResolve(voted), forgeResolvedMap(voted)).flatten
                          case _ => Seq.empty
                      }
              }
        )

    /** Valid `(continuing, removed)` tally pairs among `boxes`: the removed box's key must equal
      * the continuing box's link and be strictly greater (the linked-list contraction rule).
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

    /** Forge the surviving ballot box's merged vote to a non-castable version. The tally script's
      * `HighestVoteCheck` requires the survivor's voteStatus to equal `maxVote(cont, rem)`, so this
      * is rejected; an accepted forge trips version-legitimacy.
      */
    private def forgeSurvivorVersion(tx: Transaction): Transaction =
        mapVoteOutput(tx) { b =>
            b.datumOption match {
                case Some(Inline(d)) =>
                    val vd = fromData[VoteDatum](d)
                    b.copy(datumOption =
                        Some(Inline(toData(vd.copy(voteStatus = Voted(x2Commitment, BigInt(3))))))
                    )
                case _ => b
            }
        }

    /** Tally-phase attacks on the surviving box output of an otherwise-valid TallyTx (which merges
      * `rem` into `cont`): forge its merged version, or redirect it (with its tokens) to the
      * attacker. The dispute script's Tally branch must reject; an accepted forge trips
      * version-legitimacy, an accepted redirect trips token conservation.
      */
    private def tallyAttacks(
        cont: BallotBox[VoteStatus],
        rem: BallotBox[VoteStatus]
    ): List[Transaction] =
        TallyTx
            .Build(cont, rem, treasury, regimeUtxo, collateral)(using config)
            .result
            .toOption
            .toList
            .flatMap { tt =>
                List[Transaction => Transaction](
                  forgeSurvivorVersion,
                  tx => mapVoteOutput(tx)(_.copy(address = attackerAddr))
                ).map(mutate => ownWallet.signTx(mutate(tt.tx)))
            }

    /** Tally-phase attacker: once the deadline has passed and a tally is possible, mutate the
      * survivor of the first legal `(cont, rem)` pair. Must be rejected by the dispute script.
      */
    private val tallyAttacker: ContractTestActor[DisputeModel] =
        ContractTestActor.multi(
          "tally-attacker",
          (reader, s) =>
              slotPast(reader).map { past =>
                  if !past then Seq.empty
                  else
                      tallyPairs(s.boxes).take(1).flatMap { case (cont, rem) =>
                          tallyAttacks(cont, rem)
                      }
              }
        )

    private val step: ContractStepVariations[DisputeModel] =
        ContractStepVariations.fromActors[DisputeModel](
          extract = reader => extractState(reader),
          actors = (1 until numBoxes).toList.map(k => voteActor(BigInt(k)))
              ++ List(tallyActor, resolveActor, attackerActor, resolveAttacker, tallyAttacker),
          // ALWAYS offer a deadline-crossing slot delay. This serves two purposes: it lets a trace
          // move from the voting phase to the tally phase, and — crucially — it keeps the state
          // machine non-terminating. ScalaCheck Commands DISCARDS a whole generated sequence if
          // `genCommand` ever yields no actions before the requested length is reached; once a
          // dispute resolves no transaction is left to submit, so without an always-available action
          // every resolving trace would be discarded (and only short, non-resolving traces would
          // run). A redundant slot advance is a harmless no-op that keeps `allActions` non-empty.
          delays = _ => Seq(votingWindowSlots)
        )

    /** Structural safety invariants, checked after every accepted transaction (see class doc). */
    private val checkInvariants: (BlockchainReader, DisputeModel) => Future[Prop] =
        (_, s) =>
            Future.successful {
                val tokensConserved =
                    if s.boxes.isEmpty then Prop.passed
                    else
                        Prop(
                          s.boxes
                              .map(b => BigInt(b.ballotBoxOutput.voteTokens: Int))
                              .sum == totalTokens
                        ) :| s"vote tokens conserved (expected $totalTokens)"

                def legit(c: ByteString, m: BigInt): Boolean =
                    (c == x1Commitment && m == BigInt(1)) || (c == x2Commitment && m == x2Minor)

                val versionsLegit =
                    Prop(s.boxes.forall(_.ballotBoxOutput.status match {
                        case Voted(c, m) => legit(c, m)
                        case _           => true
                    })) :| "every Voted box commits to a castable version"

                val resolutionLegit = s.resolved match {
                    case Some((c, m)) =>
                        (Prop(legit(c, m)) :| "resolved to a castable version") &&
                        (Prop(s.boxes.isEmpty) :| "ballot boxes consumed once resolved")
                    // No ballot boxes left means resolution happened: a resolved treasury MUST then
                    // be present at the treasury address. If it is not, the treasury escaped during
                    // resolution (e.g. a successful evacuate-during-resolve cross-script attack).
                    case None =>
                        Prop(s.boxes.nonEmpty) :| "resolution must leave a treasury at its address"
                }

                tokensConserved && versionsLegit && resolutionLegit
            }

    /** Mirror the Mock execution model: single CardanoMutator under EvaluateAndComputeCost. */
    private def mkEmulator(): ImmutableEmulator =
        ImmutableEmulator(
          state = State(utxos = initialUtxos),
          env = UtxoEnv(
            now.toSlot.slot,
            env.headConfig.cardanoProtocolParams,
            certState = CertState.empty,
            env.headConfig.network
          ),
          slotConfig = env.headConfig.slotConfig,
          evaluatorMode = EvaluateAndComputeCost,
          validators = Seq.empty,
          mutators = Seq(CardanoMutator)
        )

    /** Mutable Sut for ScalaCheck Commands, backed by an [[ImmutableEmulator]].
      *
      * We deliberately do NOT use scalus-testkit's [[scalus.testing.ContractScalaCheckCommands]],
      * whose Sut is the mutable `scalus.cardano.node.Emulator`. That emulator's `setSlot` rebuilds
      * its `Context` without `evaluatorMode`, silently reverting to the default `Validate` after
      * the first slot advance — under which hydrozoa's BLS-heavy `ResolutionTx` (built to rely on
      * `EvaluateAndComputeCost`) is rejected, so resolution would never be exercised. Driving the
      * Sut through an `ImmutableEmulator` (whose `submit`/`advanceSlot` both preserve the mode)
      * sidesteps that scalus bug; the framework's model side is already an `ImmutableEmulator` and
      * is fine.
      */
    private final class EmuHolder(initial: ImmutableEmulator) {
        private var emu: ImmutableEmulator = initial
        def current: ImmutableEmulator = emu
        def submit(tx: Transaction): Boolean =
            emu.submit(tx) match {
                case Right((_, e)) => emu = e; true
                case Left(_)       => false
            }
        def advance(slots: Long): Unit = emu = emu.advanceSlot(slots)
    }

    /** The ScalaCheck Commands state machine over the dispute. The abstract `State` is the model
      * [[ImmutableEmulator]] (threaded by `nextState`); the `Sut` is an [[EmuHolder]] (advanced by
      * `run`). Command generation and invariant checking reuse `step`/`extractState`/
      * `checkInvariants`. Invariants are evaluated inside `run` on the Sut's post-transaction
      * state, which both keeps them independent of ScalaCheck's pre/post-`nextState` convention and
      * checks the real executed state.
      */
    private object disputeCommands extends Commands {
        import scala.concurrent.Await
        import scala.concurrent.duration.Duration
        private val to: Duration = Duration(120, "seconds")

        override type State = ImmutableEmulator
        override type Sut = EmuHolder

        override def genInitialState: Gen[State] = Gen.const(mkEmulator())
        override def newSut(state: State): Sut = new EmuHolder(state)
        override def destroySut(sut: Sut): Unit = ()
        override def initialPreCondition(state: State): Boolean = true
        override def canCreateNewSut(
            newState: State,
            initSuts: Iterable[State],
            runningSuts: Iterable[Sut]
        ): Boolean = true

        override def genCommand(state: State): Gen[Command] = {
            val model = Await.result(extractState(state.asReader), to)
            val actions = Await.result(step.allActions(state.asReader, model), to)
            if actions.isEmpty then Gen.fail
            else
                Gen.oneOf(actions).map {
                    case StepAction.Submit(tx) => SubmitCmd(tx)
                    case StepAction.Wait(n)    => WaitCmd(n)
                }
        }

        /** Submit a transaction; if the ledger accepts it, evaluate the safety invariants. A
          * rejection is acceptable (it is the expected outcome for an invalid/attack transaction).
          */
        case class SubmitCmd(tx: Transaction) extends Command {
            override type Result = Prop
            override def run(sut: Sut): Prop =
                if sut.submit(tx) then
                    Await.result(
                      checkInvariants(
                        sut.current.asReader,
                        Await.result(extractState(sut.current.asReader), to)
                      ),
                      to
                    )
                else Prop.passed
            override def nextState(state: State): State =
                state.submit(tx) match {
                    case Right((_, e)) => e
                    case Left(_)       => state
                }
            override def preCondition(state: State): Boolean = true
            override def postCondition(state: State, result: Try[Prop]): Prop =
                result match {
                    case Success(p) => p
                    case Failure(e) => Prop.exception(e)
                }
        }

        /** Advance the slot (the action that crosses the voting deadline and keeps the machine
          * non-terminating so resolving traces are not discarded).
          */
        case class WaitCmd(slots: Long) extends Command {
            override type Result = Unit
            override def run(sut: Sut): Unit = sut.advance(slots)
            override def nextState(state: State): State = state.advanceSlot(slots)
            override def preCondition(state: State): Boolean = slots > 0
            override def postCondition(state: State, result: Try[Unit]): Prop =
                result match {
                    case Success(_) => Prop.passed
                    case Failure(e) => Prop.exception(e)
                }
        }
    }

    val _ = property(
      "random vote/tally/resolve interleavings preserve the dispute safety invariants"
    ) = disputeCommands.property()
}
