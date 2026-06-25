package hydrozoa.rulebased.ledger.l1.script.plutus

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.{addrKeyHash, pubKeyHash}
import hydrozoa.rulebased.ledger.l1.DisputeActorTestHelpers.{mkBallotBoxUtxo, mkRuleBasedTreasury}
import hydrozoa.rulebased.ledger.l1.state.StandaloneEvacuationCommitmentOnchain
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genCollateralUtxo
import hydrozoa.rulebased.ledger.l1.tx.VoteTx
import hydrozoa.rulebased.ledger.l1.utxo.BallotBox
import org.scalacheck.{Arbitrary, Gen, Properties, Test}
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State, UtxoEnv}

/** Negative tests for [[DisputeResolutionValidator]]: malformed votes the validator (or the
  * builder) must reject.
  *
  * Each is a single transaction, so there is no need for an emulator or the `Scenario` monad — we
  * run the tx straight through [[CardanoMutator]] (the ledger rule that executes the Plutus script)
  * and check it's rejected. Each builds an otherwise-valid vote via [[VoteTx.Build]], then mutates
  * one field to make it illegal and re-signs:
  *
  *   - E1 vote on another peer's reserved box → VoteMustBeSignedByPeer
  *   - E2 vote past deadlineVoting (finite-but-too-late upper bound) → VoteTimeValidityCheck
  *   - E3 vote with an OPEN validity interval (no upper bound) → VoteTimeValidityCheck (the
  *     distinct non-finite `case _ => fail` path; you can't dodge the deadline with an unbounded
  *     range)
  *   - E4 re-vote a box that is already Voted (terminal-state re-entry) → the builder's
  *     VoteAlreadyCast (refused before a transaction is even formed)
  *   - E5 swap ONLY the staking part of the continuing vote output's address (same payment script,
  *     the attacker's stake key) → VoteVoteOutputExists. The on-chain `Eq[Address]` ANDs the
  *     payment AND staking credential, so the address is checked in full — a redirect that keeps
  *     the payment part but captures staking rights on the locked utxo is still rejected.
  *
  * Each script-level assertion checks the rejection is a *script* failure mentioning the
  * validator's own message, so a reject cannot pass for an incidental ledger reason.
  */
object DisputeVoteAttackTest extends Properties("Dispute Vote Attack") {

    import MultiNodeConfig.*

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withMinSuccessfulTests(5)

    /** Replace the tx's required signers (to model a vote signed by the wrong peer). */
    private def withRequiredSigners(tx: Transaction, signers: Set[AddrKeyHash]): Transaction =
        tx.copy(body = KeepRaw(tx.body.value.copy(requiredSigners = TaggedSortedSet.from(signers))))

    /** Push the tx's validity upper bound to `slot` (to model a vote past the deadline). */
    private def withValidityEnd(tx: Transaction, slot: Long): Transaction =
        tx.copy(body = KeepRaw(tx.body.value.copy(ttl = Some(slot))))

    /** Remove the tx's validity upper bound entirely (an open/unbounded interval — to model
      * crafting the validity range so no finite `to` can be checked against the deadline).
      */
    private def withoutValidityEnd(tx: Transaction): Transaction =
        tx.copy(body = KeepRaw(tx.body.value.copy(ttl = None)))

    /** Swap ONLY the staking (delegation) part of the continuing vote output's address: keep its
      * value and payment script, but attach the attacker's own stake key. The honest vote output
      * has no delegation part (Null), so attaching one grows the output by ~28 bytes — pay that
      * extra size out of the attacker's own change output (index 0, the collateral return) and bump
      * the fee, so the *script* rejects the staking mismatch rather than a phase-1 fee check.
      * Models capturing staking rights/rewards on the locked ballot-box utxo without touching its
      * payment credential.
      */
    private def withSwappedStakingPart(
        tx: Transaction,
        voteValue: Value,
        stake: AddrKeyHash,
        feeBump: Coin
    ): Transaction = {
        val body = tx.body.value
        val delegation = ShelleyDelegationPart.Key(StakeKeyHash.fromByteString(stake))
        val newOutputs = body.outputs.zipWithIndex.map { case (sized, idx) =>
            val out = sized.value
            out.address match {
                case sh: ShelleyAddress if out.value == voteValue =>
                    val withStake = sh.copy(delegation = delegation)
                    Sized(out match {
                        case b: TransactionOutput.Babbage => b.copy(address = withStake)
                        case s: TransactionOutput.Shelley => s.copy(address = withStake)
                    })
                case _ if idx == 0 => Sized(out.withValue(out.value - Value(feeBump)))
                case _             => sized
            }
        }
        tx.copy(body =
            KeepRaw(body.copy(outputs = newOutputs, fee = Coin(body.fee.value + feeBump.value)))
        )
    }

    /** True iff the tx was rejected by Plutus script evaluation with a message mentioning `needle`
      * — i.e. the validator (not an incidental ledger rule) caught the attack.
      */
    private def rejectedByValidator(
        result: Either[TransactionException, State],
        needle: String
    ): Boolean =
        result match {
            case Left(e: TransactionException.PlutusScriptValidationException) =>
                (e.explain + " " + e.logs.mkString(" ")).contains(needle)
            case _ => false
        }

    def voteAttacks: MultiNodeConfigTestM[Boolean] = for {
        env <- ask

        treasuryToken = Value.asset(
          env.headConfig.headMultisigScript.policyId,
          env.headConfig.headTokenNames.treasuryTokenName,
          1
        )
        fallbackTxId <- pick(Arbitrary.arbitrary[TransactionHash])
        nEvacs <- pick(Gen.choose(0, 10))
        evacMap <- pick(test.Generators.Hydrozoa.genEvacuationMap(nEvacs)(using env))
        versionMajor = BigInt(100)
        versionMinor = BigInt(2)
        now <- lift(realTimeQuantizedInstant(env.headConfig.slotConfig))

        ruleBasedTreasury <- mkRuleBasedTreasury(
          versionMajor,
          evacMap.totalValue + treasuryToken,
          TransactionInput(fallbackTxId, 0),
          votingDeadline = now.toPosixTime + 600_000
        )

        config = env.nodeConfigs.head._2
        ownWallet = env.nodePrivateConfigs.head._2.ownWallet
        ownKeyHash = ownWallet.exportVerificationKey.addrKeyHash
        otherWallet = env.nodePrivateConfigs.values
            .filter(_.ownWallet.exportVerificationKey != ownWallet.exportVerificationKey)
            .head
            .ownWallet

        ownVoteUtxo <- mkBallotBoxUtxo(
          1,
          2,
          VoteStatus.AwaitingVote(ownWallet.exportVerificationKey.pubKeyHash),
          TransactionInput(fallbackTxId, env.headConfig.nHeadPeers + 1)
        )
        otherVoteUtxo <- mkBallotBoxUtxo(
          2,
          3,
          VoteStatus.AwaitingVote(otherWallet.exportVerificationKey.pubKeyHash),
          TransactionInput(fallbackTxId, env.headConfig.nHeadPeers + 2)
        )

        collateralUtxo <- pick(genCollateralUtxo(ownKeyHash)(using env.headConfig))

        sec = StandaloneEvacuationCommitmentOnchain(
          headId = env.headConfig.headTokenNames.treasuryTokenName.bytes,
          versionMajor = versionMajor,
          versionMinor = versionMinor,
          commitment = evacMap.kzgCommitment
        )
        signatures = env.multisignHeader(sec).toList
        coilSignatures = env.multisignHeaderCoil(sec)

        ownBallotBox <- failLeft(
          BallotBox
              .parse(ownVoteUtxo)(using config)
              .map(_.asInstanceOf[BallotBox[VoteStatus.AwaitingVote]])
        )
        otherBallotBox <- failLeft(
          BallotBox
              .parse(otherVoteUtxo)(using config)
              .map(_.asInstanceOf[BallotBox[VoteStatus.AwaitingVote]])
        )

        buildVote = (box: BallotBox[VoteStatus.AwaitingVote]) =>
            VoteTx
                .Build(
                  uncastBallotBox = box,
                  treasuryUtxo = ruleBasedTreasury,
                  collateralUtxo = collateralUtxo,
                  sec = sec,
                  signatures = signatures,
                  coilSignatures = coilSignatures
                )
                .result(using config)

        ownVoteTx <- failLeft(buildVote(ownBallotBox))
        otherVoteTx <- failLeft(buildVote(otherBallotBox))

        initialUtxos: Utxos = Map(
          (ruleBasedTreasury.utxoId, ruleBasedTreasury.treasuryOutput.toOutput(using config)),
          (ownVoteUtxo.input, ownVoteUtxo.output),
          (otherVoteUtxo.input, otherVoteUtxo.output),
          (collateralUtxo.input, collateralUtxo.collateralOutput.toOutput(using env))
        ) ++ config.scriptReferenceUtxos.toList.map(_.toTuple)

        // The validator runs in CardanoMutator.transit under EvaluateAndComputeCost. Both attacks
        // are checked against this same base state (each is independent — no state threading).
        state = State(utxos = initialUtxos)
        context = Context(
          fee = Coin.zero,
          env = UtxoEnv(
            now.toSlot.slot,
            env.headConfig.cardanoProtocolParams,
            certState = CertState.empty,
            env.headConfig.network
          ),
          slotConfig = env.headConfig.slotConfig,
          evaluatorMode = EvaluateAndComputeCost
        )

        // E1: a peer votes on ANOTHER peer's reserved box. We set the required signer to ourselves
        // and sign — so the ledger's witness check passes, but the validator's AwaitingVote(peer)
        // check must reject it.
        e1Tx = ownWallet.signTx(withRequiredSigners(otherVoteTx.tx, Set(ownKeyHash)))
        e1Result = CardanoMutator.transit(context, state, e1Tx)
        _ <- assertWith(
          msg =
              s"E1: voting on another peer's box must be rejected by the validator, got $e1Result",
          condition = rejectedByValidator(e1Result, "signed by peer")
        )

        // E2: vote past deadlineVoting — push the validity upper bound beyond the deadline.
        honestTtl = ownVoteTx.tx.body.value.ttl.getOrElse(now.toSlot.slot)
        e2Tx = ownWallet.signTx(withValidityEnd(ownVoteTx.tx, honestTtl + 100))
        e2Result = CardanoMutator.transit(context, state, e2Tx)
        _ <- assertWith(
          msg = s"E2: vote past deadlineVoting must be rejected by the validator, got $e2Result",
          condition = rejectedByValidator(e2Result, "deadlineVoting")
        )

        // E3: craft an OPEN validity interval (no upper bound). The validator needs a finite upper
        // bound to compare against deadlineVoting, so an unbounded `to` hits the `case _ => fail`
        // path — you cannot dodge the deadline check with an open validity range.
        e3Tx = ownWallet.signTx(withoutValidityEnd(ownVoteTx.tx))
        e3Result = CardanoMutator.transit(context, state, e3Tx)
        _ <- assertWith(
          msg =
              s"E3: a vote with an open (unbounded) validity range must be rejected, got $e3Result",
          condition = rejectedByValidator(e3Result, "deadlineVoting")
        )

        // E4: re-vote a box that is already Voted (terminal-state re-entry). The builder's
        // parseAndVote sees a non-AwaitingVote status and refuses with VoteAlreadyCast before a
        // transaction is even formed.
        votedUtxo <- mkBallotBoxUtxo(
          3,
          4,
          VoteStatus.Voted(evacMap.kzgCommitment, versionMinor),
          TransactionInput(fallbackTxId, env.headConfig.nHeadPeers + 3)
        )
        votedBox <- failLeft(
          BallotBox
              .parse(votedUtxo)(using config)
              .map(_.asInstanceOf[BallotBox[VoteStatus.AwaitingVote]])
        )
        _ <- assertWith(
          msg = "E4: re-voting an already-Voted box must be refused (VoteAlreadyCast)",
          condition = buildVote(votedBox) match {
              case Left(VoteTx.Build.Error.VoteAlreadyCast) => true
              case _                                        => false
          }
        )

        // E5: swap ONLY the staking part of the continuing vote output's address (same payment
        // script, attacker's stake key). The on-chain Eq[Address] ANDs payment AND staking
        // credential, so this subtle redirect — payment matches, staking differs — must still be
        // caught by the continuing-output check rather than slip through.
        e5Tx = ownWallet.signTx(
          withSwappedStakingPart(ownVoteTx.tx, ownVoteUtxo.output.value, ownKeyHash, Coin(5000))
        )
        e5Result = CardanoMutator.transit(context, state, e5Tx)
        _ <- assertWith(
          msg =
              s"E5: a staking-credential-only swap on the vote output must be rejected, got $e5Result",
          condition =
              rejectedByValidator(e5Result, "one continuing vote output with the same value")
        )
    } yield true

    val _ = property("dispute vote attacks (negative)") = runWithCoil()(voteAttacks)
}
