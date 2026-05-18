package hydrozoa.multisig.consensus.ack

import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.effects.{PartitionIndex, WithinPartitionIndex}
import hydrozoa.multisig.ledger.l1.tx.InitializationTx
import hydrozoa.multisig.ledger.stack.{Stack, StackEffects, StandaloneEvacuationCommitment}
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.Transaction
import scalus.crypto.ed25519.VerificationKey

/** The deterministic, **flat** signing material for a closed [[Stack.Unsigned]]: for every L1
  * effect in the stack, the exact bytes that get signed (tx body / evac-commit header bytes). No
  * round structure — signing is one-shot. The split into round-1 / round-2 hard-acks is a
  * *consensus* concern (atomicity of the L1 unlock), handled separately by [[HardAckRoundPlan]]; it
  * is deliberately NOT modelled here.
  *
  * This is the single source of truth shared by:
  *   - [[hydrozoa.multisig.consensus.StackComposer]] /
  *     [[hydrozoa.multisig.consensus.peer.HeadPeerWallet]] — to produce this peer's signatures, and
  *   - [[hydrozoa.multisig.consensus.SlowConsensusActor]] — to reconstruct the byte-identical
  *     material and verify every remote peer's per-effect signature.
  *
  * Determinism: effect derivation is byte-identical across head peers (a core Hydrozoa invariant),
  * so every peer computes the same inputs from the same `Stack.Unsigned` — that is exactly what
  * makes the per-effect signatures comparable across peers.
  *
  * Partition-index keys use list index as a proxy (single-partition is the common case;
  * multi-partition exercises the same code paths with multiple entries).
  */
sealed trait StackEffectsSigningInputs

object StackEffectsSigningInputs {

    /** Which regular-stack effect is the round-2 *unlock* (the L1 entry point all other effects
      * depend on). Selecting it is a property of the effects; the round *framing* that uses it
      * lives in [[HardAckRoundPlan]].
      */
    enum Unlock:
        /** The first settlement (`settlements(PartitionIndex.zero)`). */
        case FirstSettlement

        /** The finalization tx (a stack with a Final block and no settlement). */
        case Finalization

    /** Stack 1+. All effects, flat — the unlock is NOT excluded here (that is round framing).
      *
      * @param unlock
      *   `Some` ⇒ the stack contains a settlement / finalization (2-phase); `None` ⇒ minor-only
      *   (1-phase / sole). `evacCommit` is guaranteed present in the `None` (minor-only) case.
      */
    final case class Regular(
        settlements: Map[PartitionIndex, Transaction],
        fallbacks: Map[PartitionIndex, Transaction],
        rollouts: Map[(PartitionIndex, WithinPartitionIndex), Transaction],
        refunds: Map[(PartitionIndex, WithinPartitionIndex), Transaction],
        evacCommit: Option[(BlockNumber, BlockHeader.Minor.Onchain.Serialized)],
        finalization: Option[Transaction],
        unlock: Option[Unlock]
    ) extends StackEffectsSigningInputs

    /** Stack 0. The init tx is exogenous; round framing signs the fallback then the init tx — see
      * [[HardAckRoundPlan]]. The rich [[InitializationTx]] is kept so the wallet/verifier can
      * resolve the init tx's spent inputs for the individual-witness rule
      * ([[spendsFromIndividualAddress]]).
      */
    final case class Initial(
        fallback: Transaction,
        initTx: InitializationTx
    ) extends StackEffectsSigningInputs

    /** Derive the flat signing inputs from a closed stack — total over [[StackEffects]]. */
    def from(unsigned: Stack.Unsigned): StackEffectsSigningInputs =
        unsigned.effects match {
            case e: StackEffects.Initial =>
                Initial(fallback = e.fallbackTx.tx, initTx = e.initializationTx)
            case effects: StackEffects.Regular =>
                regular(unsigned, effects)
        }

    private def regular(
        unsigned: Stack.Unsigned,
        effects: StackEffects.Regular
    ): Regular = {
        // Evac-commit hard-ack binds the block's KZG commitment (that IS the standalone evac
        // record). KZG is deliberately kept in the fast-consensus brief for now (TRANSITIONAL
        // — see the note on BlockHeader.Fields.HasKzgCommitment), so `signingBytes` already
        // carries it and these bytes happen to coincide with the soft-ack domain today; once
        // KZG moves slow-side the slow side will need its own KZG-bearing serialization here.
        val headerBytesByBlock: Map[BlockNumber, BlockHeader.Minor.Onchain.Serialized] =
            unsigned.results.toList.map { r =>
                r.brief.blockNum -> r.brief.header.signingBytes
            }.toMap
        def evacHeaderBytes(
            ec: StandaloneEvacuationCommitment
        ): (BlockNumber, BlockHeader.Minor.Onchain.Serialized) =
            ec.committedBlockNum -> headerBytesByBlock(ec.committedBlockNum)

        Regular(
          settlements = effects.settlements.zipWithIndex.map { case (tx, i) =>
              PartitionIndex(i) -> tx.tx
          }.toMap,
          fallbacks = effects.fallbacks.zipWithIndex.map { case (tx, i) =>
              PartitionIndex(i) -> tx.tx
          }.toMap,
          rollouts = effects.rollouts.zipWithIndex.map { case (tx, i) =>
              (PartitionIndex.zero, WithinPartitionIndex(i)) -> tx.tx
          }.toMap,
          refunds = effects.refunds.zipWithIndex.map { case (tx, i) =>
              (PartitionIndex.zero, WithinPartitionIndex(i)) -> tx.tx
          }.toMap,
          evacCommit = effects.evacCommit.map(evacHeaderBytes),
          finalization = effects.finalization.map(_.tx),
          unlock =
              if effects.settlements.nonEmpty then Some(Unlock.FirstSettlement)
              else if effects.finalization.isDefined then Some(Unlock.Finalization)
              else None
        )
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
}
