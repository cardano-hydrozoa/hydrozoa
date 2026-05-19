package hydrozoa.multisig.ledger.stack

import cats.data.NonEmptyList
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, InitializationTx}
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.crypto.ed25519.VerificationKey

/** Necessary L1 effects for a closed stack, derived locally by each peer.
  *
  * Signing state is explicit at the type level, mirroring
  * [[hydrozoa.multisig.ledger.block.BlockEffects]]:
  *
  *   - [[StackEffects.Unsigned]] — bodies as derived, no aggregated hard-ack signatures. Held by
  *     [[Stack.Unsigned]] / [[Stack.Round1Confirmed]].
  *   - [[StackEffects.HardConfirmed]] — every head peer's hard-ack signature aggregated in: tx
  *     bodies carry the multisig `VKeyWitness`es; the standalone evac commitment carries every
  *     peer's header signature ([[StandaloneEvacuationCommitment.MultiSigned]]). Held by
  *     [[Stack.HardConfirmed]]; L1-submittable / dispute-usable as is.
  *
  * Not every effect is a transaction: settlement / fallback / rollout / refund / finalization are
  * txs (hard-acked per tx body), but a standalone evac commitment commits a *block header* (KZG
  * lives on the header) and is hard-acked with a header signature, not a tx-body signature — its
  * multisigned form is the set of all peers' header signatures, usable in the rule-based regime's
  * vote tx (see [[StandaloneEvacuationCommitment]]).
  *
  * **Never wire-broadcast** — only [[StackBrief]] travels on the wire. Each peer derives the
  * effects from its own [[hydrozoa.multisig.ledger.block.BlockResult]] stream + the leader's brief;
  * deterministic derivation makes the resulting bytes byte-identical across peers.
  */
sealed trait StackEffects

object StackEffects:

    /** Effects as locally derived — no aggregated hard-ack signatures yet. */
    sealed trait Unsigned extends StackEffects

    object Unsigned:

        /** Stack 0 only. The initialization tx is exogenous (head config).
          *
          *   - Round 1 signs the fallback tx body (locally derived from initTx + head params).
          *   - Round 2 signs the init tx body + each peer's individual key witnesses for utxos
          *     spent from its individual addresses.
          */
        final case class Initial(
            initializationTx: InitializationTx,
            fallbackTx: FallbackTx
        ) extends Unsigned

        /** Stack 1+. Regular slow-consensus flow, **partition-indexed** (PR #446 review): one
          * [[PartitionEffects]] per [[StackPartition]], in stack order. The partition is the spine
          * — settlement / fallback / rollout / refund / finalization / SEC all live inside their
          * owning partition, not in parallel flat lists.
          *
          * @param partitions
          *   the stack's partitions' effects, in order; head-based kinds (`Minor` / `Major` /
          *   `Final`). The round-2 *unlock* is selected structurally over this list (first Major
          *   partition's settlement; else the Final partition's finalization) — see the shared
          *   unlock-selection function, not a separate plan.
          */
        final case class Regular(
            partitions: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment]]
        ) extends Unsigned

    /** Effects with every head peer's hard-ack signature aggregated in: tx bodies carry the
      * multisig `VKeyWitness`es; the standalone evac commitment carries every peer's header
      * signature. L1-submittable (txs) / dispute-usable (evac commitment) as is.
      *
      * The tx wrapper types are unchanged from [[Unsigned]] — the multisig `VKeyWitness`es live
      * inside each [[hydrozoa.multisig.ledger.l1.tx.Tx]] body. Only the evac member differs in
      * type: an [[StandaloneEvacuationCommitment.MultiSigned]] (the dormant record + all peers'
      * header signatures) rather than the bare record.
      */
    sealed trait HardConfirmed extends StackEffects

    object HardConfirmed:

        /** Stack 0, hard-confirmed. @see [[Unsigned.Initial]]. */
        final case class Initial(
            initializationTx: InitializationTx,
            fallbackTx: FallbackTx
        ) extends HardConfirmed

        /** Stack 1+, hard-confirmed. @see [[Unsigned.Regular]]. Same partition spine; the tx
          * wrappers carry their multisig `VKeyWitness`es in place, and each partition's SEC is a
          * [[StandaloneEvacuationCommitment.MultiSigned]] (the only type difference vs
          * [[Unsigned.Regular]]).
          */
        final case class Regular(
            partitions: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]]
        ) extends HardConfirmed

    /** Deterministic per-peer predicate, shared by the wallet (decides whether to attach its own
      * individual `VKeyWitness`) and [[hydrozoa.multisig.consensus.SlowConsensusActor]] (enforces
      * the iff / no-extra-witness rule when verifying a remote peer's round-2 Initial ack): does
      * the initial stack's init tx spend any input locked at the peer's individual (key-hash)
      * address? Cardano L1 rejects a vkey witness that signs nothing it spends, so a peer MUST
      * contribute an individual witness exactly when this holds and MUST NOT otherwise. (The init
      * tx's *multisig* contribution — minting the head tokens — is the separate round-2 init-tx
      * signature, always present.)
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
