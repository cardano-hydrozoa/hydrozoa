package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.effects.{PartitionIndex, WithinPartitionIndex}
import hydrozoa.multisig.ledger.stack.{Stack, StackEffects, StandaloneEvacuationCommitment}

/** Pure derivation of the per-effect signing material for a closed [[Stack.Unsigned]].
  *
  * Single source of truth, shared by:
  *   - [[hydrozoa.multisig.consensus.StackComposer]] — to produce its own hard-acks via
  *     [[hydrozoa.multisig.consensus.peer.HeadPeerWallet]].
  *   - [[hydrozoa.multisig.consensus.SlowConsensusActor]] — to reconstruct the exact same keyed
  *     message set and verify every remote peer's per-effect signature against it.
  *
  * Determinism note: effect derivation is byte-identical across head peers (a core Hydrozoa
  * invariant), so every peer computes the same plan from the same `Stack.Unsigned` — that's
  * precisely what makes the slow-side per-effect signatures comparable across peers.
  *
  * Partition-index keys use list index as a proxy (single-partition is the common case;
  * multi-partition exercises the same code paths with multiple entries) — kept byte-for-byte
  * identical to the pre-refactor inline logic that lived in `StackComposer.buildHandoff`.
  */
sealed trait HardAckSigningPlan

object HardAckSigningPlan {

    /** 2-phase stack: a settlement and/or a finalization is present. Round 1 signs every effect
      * except the first settlement / finalization (the round-2 unlock).
      */
    final case class TwoPhase(
        round1: HardAck.SigningInputs.Round1Regular,
        round2: HardAck.SigningInputs.Round2Regular
    ) extends HardAckSigningPlan

    /** 1-phase minor-only stack: no settlement, no finalization. */
    final case class Sole(sole: HardAck.SigningInputs.Sole) extends HardAckSigningPlan

    /** Initial stack (stack 0). Structurally 2-phase, but the unlock is exogenous: round 1 signs
      * the locally-derived fallback tx body, round 2 signs the exogenous initialization tx body +
      * the per-peer individual key witnesses already carried on it (operator-supplied funding for
      * utxos spent from individual addresses). Verified/handled by the same 2-phase cell machinery
      * as [[TwoPhase]].
      */
    final case class Initial(
        round1: HardAck.SigningInputs.Round1Initial,
        round2: HardAck.SigningInputs.Round2Initial
    ) extends HardAckSigningPlan

    /** Derive the plan from a closed stack — total over [[StackEffects]].
      *
      * @throws IllegalStateException
      *   only for impossible-by-construction cases (a 2-phase regular stack with neither settlement
      *   nor finalization; a minor-only stack with no evac commitment).
      */
    def from(unsigned: Stack.Unsigned): HardAckSigningPlan =
        unsigned.effects match {
            case e: StackEffects.Initial =>
                // Pure: both bodies come straight off the stack. The individual witnesses are
                // the operator-supplied vkey witnesses already attached to the exogenous init
                // tx (the multisig sig itself is the round-2 `initTxSig`, aggregated by
                // consensus). `headId`-style per-peer wallet signing is not needed here — the
                // wallet only signs the init/fallback bodies.
                HardAckSigningPlan.Initial(
                  round1 = HardAck.SigningInputs.Round1Initial(fallback = e.fallbackTx.tx),
                  round2 = HardAck.SigningInputs.Round2Initial(
                    initTx = e.initializationTx.tx,
                    individualWitnesses =
                        e.initializationTx.tx.witnessSetRaw.value.vkeyWitnesses.toSet.toList
                  )
                )
            case effects: StackEffects.Regular =>
                regularPlan(unsigned, effects)
        }

    /** The 2-phase / sole plan for a non-initial (regular) stack. */
    private def regularPlan(
        unsigned: Stack.Unsigned,
        effects: StackEffects.Regular
    ): HardAckSigningPlan = {
        // Header signing bytes by blockNum — an evac commitment commits a *block header* (KZG
        // lives on the header), so a peer signs the full minor header, the same shape a
        // soft-ack signs.
        val headerBytesByBlock: Map[BlockNumber, BlockHeader.Minor.Onchain.Serialized] =
            unsigned.results.toList.map { r =>
                r.brief.blockNum -> r.brief.header.signingBytes
            }.toMap
        def evacHeaderBytes(
            ec: StandaloneEvacuationCommitment
        ): (BlockNumber, BlockHeader.Minor.Onchain.Serialized) =
            ec.committedBlockNum -> headerBytesByBlock(ec.committedBlockNum)

        val refundsIn = effects.refunds.zipWithIndex.map { case (tx, i) =>
            (PartitionIndex.zero, WithinPartitionIndex(i)) -> tx.tx
        }.toMap

        val twoPhase = effects.settlements.nonEmpty || effects.finalization.isDefined

        if twoPhase then {
            val firstUnlockIsSettlement = effects.settlements.nonEmpty
            // Round-1 signs every effect EXCEPT the round-2 unlock (the first settlement, or
            // the finalization when there's no settlement).
            val round1Settlements = effects.settlements.zipWithIndex.collect {
                case (tx, i) if !(firstUnlockIsSettlement && i == 0) =>
                    PartitionIndex(i) -> tx.tx
            }.toMap
            val round1Fallbacks = effects.fallbacks.zipWithIndex.map { case (tx, i) =>
                PartitionIndex(i) -> tx.tx
            }.toMap
            val round1Rollouts = effects.rollouts.zipWithIndex.map { case (tx, i) =>
                (PartitionIndex.zero, WithinPartitionIndex(i)) -> tx.tx
            }.toMap
            val round1EvacCommit = effects.evacCommit.map(evacHeaderBytes)
            // At most one Final per stack. The finalization is signed in round 1 only when a
            // settlement precedes it (settlement is then the unlock); otherwise the
            // finalization itself is the round-2 unlock and is signed there.
            val round1Finalization =
                if firstUnlockIsSettlement then effects.finalization.map(_.tx)
                else None
            val unlockTx = effects.settlements.headOption
                .map(_.tx)
                .orElse(effects.finalization.map(_.tx))
                .getOrElse(
                  throw new IllegalStateException(
                    "HardAckSigningPlan.from: 2-phase stack has no settlement and no finalization"
                  )
                )
            TwoPhase(
              round1 = HardAck.SigningInputs.Round1Regular(
                settlements = round1Settlements,
                fallbacks = round1Fallbacks,
                rollouts = round1Rollouts,
                refunds = refundsIn,
                evacCommit = round1EvacCommit,
                finalization = round1Finalization
              ),
              round2 = HardAck.SigningInputs.Round2Regular(unlock = unlockTx)
            )
        } else {
            // Minor-only stack: exactly one TrailingMinors partition ⇒ exactly one
            // standalone evac commitment.
            val ec = effects.evacCommit.getOrElse(
              throw new IllegalStateException(
                "HardAckSigningPlan.from: minor-only stack must have exactly one evac commit, " +
                    "but evacCommit is None"
              )
            )
            Sole(
              HardAck.SigningInputs.Sole(
                refunds = refundsIn,
                evacCommit = evacHeaderBytes(ec)
              )
            )
        }
    }
}
