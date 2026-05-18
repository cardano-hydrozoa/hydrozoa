package hydrozoa.multisig.consensus.ack

import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.effects.{PartitionIndex, WithinPartitionIndex}
import hydrozoa.multisig.ledger.l1.tx.InitializationTx
import hydrozoa.multisig.ledger.stack.{Stack, StackEffects, StandaloneEvacuationCommitment}
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.crypto.ed25519.VerificationKey

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
      * the locally-derived fallback tx body; round 2 signs the exogenous init tx body (the head
      * **multisig** contribution) and, per peer, attaches an individual `VKeyWitness` iff the init
      * tx spends an input at that peer's own individual address — those individual witnesses are
      * produced by each wallet during round-2 consensus and collected across peers, NOT carried on
      * the exogenous tx. Verified/handled by the same 2-phase cell machinery as [[TwoPhase]] (see
      * [[spendsFromIndividualAddress]]).
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
                // Pure: only the bodies come off the stack. The per-peer individual witnesses
                // are NOT derivable here — each head peer's wallet produces its own during
                // round-2 consensus (see `spendsFromIndividualAddress`), and they are collected
                // across peers' round-2 acks. The pure plan just hands over the rich
                // InitializationTx so wallet + verifier can resolve its spent inputs.
                HardAckSigningPlan.Initial(
                  round1 = HardAck.SigningInputs.Round1Initial(fallback = e.fallbackTx.tx),
                  round2 = HardAck.SigningInputs.Round2Initial(initTx = e.initializationTx)
                )
            case effects: StackEffects.Regular =>
                regularPlan(unsigned, effects)
        }

    /** Deterministic per-peer predicate, shared by the wallet (which decides whether to attach its
      * own individual `VKeyWitness`) and [[hydrozoa.multisig.consensus.SlowConsensusActor]] (which
      * enforces the iff / no-extra-witness rule when verifying a remote peer's round-2 Initial
      * ack): does the initial stack's init tx spend any input locked at the peer's individual
      * (key-hash) address?
      *
      * Cardano L1 rejects a tx carrying a vkey witness that signs nothing it spends, so a peer MUST
      * contribute an individual witness exactly when this is true and MUST NOT otherwise. (The init
      * tx's *multisig* contribution — minting the head tokens — is the separate round-2
      * `initTxSig`, always present.)
      */
    def spendsFromIndividualAddress(
        initTx: InitializationTx,
        vkey: VerificationKey
    ): Boolean = {
        val kh = vkey.addrKeyHash
        initTx.tx.body.value.inputs.toSeq.exists { in =>
            initTx.resolvedUtxos.utxos.get(in).exists { out =>
                out.address match {
                    case sa: ShelleyAddress =>
                        sa.payment match {
                            case ShelleyPaymentPart.Key(h) => h == kh
                            case _: ShelleyPaymentPart     => false
                        }
                    case _ => false
                }
            }
        }
    }

    /** The 2-phase / sole plan for a non-initial (regular) stack. */
    private def regularPlan(
        unsigned: Stack.Unsigned,
        effects: StackEffects.Regular
    ): HardAckSigningPlan = {
        // Header signing bytes by blockNum — the evac-commit hard-ack must bind the block's
        // KZG commitment (that IS the standalone evac record). KZG is deliberately kept in
        // the fast-consensus brief for now (TRANSITIONAL — see the note on
        // BlockHeader.Fields.HasKzgCommitment), so `signingBytes` already carries it and
        // these bytes happen to coincide with the soft-ack domain today; once KZG moves
        // slow-side the slow side will need its own KZG-bearing serialization here.
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
