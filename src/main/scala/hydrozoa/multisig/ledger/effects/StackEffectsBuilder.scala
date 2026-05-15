package hydrozoa.multisig.ledger.effects

import cats.implicits.*
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockResult, BlockType}
import hydrozoa.multisig.ledger.l1.L1LedgerM
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, RefundTx, RolloutTx, SettlementTx}
import hydrozoa.multisig.ledger.stack.StackEffects

/** Slow-side effect derivation: from the partitioned stack content, produce the necessary L1 effect
  * transactions. Wraps the existing [[L1LedgerM]] tx-construction helpers (`mkSettlementTxSeq`,
  * `finalizeLedger`) and adds slow-side concerns:
  *
  *   - Treasury rotation across stacks (re-enabled in M10 once slow consensus drives it).
  *   - Standalone evacuation-commitment construction for trailing-minor partitions.
  *
  * Pure functions of `(L1LedgerM state at stack open) + List[Partition]` — no JointLedger lookup,
  * all required state threaded through inputs.
  *
  * **Block-by-block construction:** per the slow-consensus plan, effects are built per-block
  * (mirroring the deleted `mkBlockEffectsIntermediate` from the fast path), not aggregated
  * per-partition. The partition is only used for round-1/round-2 framing (which settlement is the
  * unlock) and for evac-commit compression on TrailingMinors partitions.
  */
object StackEffectsBuilder {

    /** Build the regular-stack effect bundle for a non-initial stack.
      *
      * Per-block dispatch:
      *   - **Minor**: emit any post-dated refund txs the block carried (deposit refunds set up at
      *     registration time).
      *   - **Major**: TODO — call `L1LedgerM.mkSettlementTxSeq(...)` for settlement + fallback +
      *     rollouts using the block's `payoutObligations` + absorbed-deposit list.
      *   - **Final**: TODO — call `L1LedgerM.finalizeLedger(...)` for finalization tx + rollouts;
      *     no refunds (drained via finalization).
      *
      * Per-partition compression:
      *   - **TrailingMinors**: TODO — build a [[StandaloneEvacCommitTx]] over the partition's final
      *     cumulative evac-map state. The "necessary effects" rule keeps only the LAST standalone
      *     evac commit per such partition.
      *
      * The first settlement / finalization across the produced lists is the round-2 unlock; the
      * caller (StackComposer) is responsible for marking it as such when building the HardAck
      * payload.
      *
      * **Current scope:** minor-only stacks fully handled (refund txs collected; no settlement /
      * fallback / rollout; standalone evac commit deferred to next slice). Stacks containing Major
      * / Final blocks fall through to TODOs.
      */
    def deriveRegular(
        partitions: List[Partition]
    ): L1LedgerM[StackEffects.Regular] = {
        val allBlocks: List[BlockResult] = partitions.flatMap(_.blocks.toList)

        // Refunds: per-Minor block, take its post-dated refund txs.
        // Major / Final drain refunds via settlement / finalization (no per-block refund txs).
        val refunds: List[RefundTx] = allBlocks.flatMap { b =>
            b.brief match {
                case _: BlockType.Minor => b.postDatedRefundTxs
                case _: BlockType.Major => Nil
                case _: BlockType.Final => Nil
            }
        }

        // Settlement / fallback / rollout — per Major block. Walk in stack order and call
        // mkSettlementTxSeq, which advances the L1LedgerM treasury for the next major.
        val majorBlocks: List[BlockResult] = allBlocks.collect {
            case b if b.brief.isInstanceOf[BlockBrief.Major] => b
        }

        majorBlocks
            .traverse { b =>
                val majorBrief = b.brief.asInstanceOf[BlockBrief.Major]
                L1LedgerM.mkSettlementTxSeq(
                  nextKzg = majorBrief.header.kzgCommitment,
                  absorbedDeposits = b.absorbedDeposits,
                  payoutObligations = b.payoutObligations.toVector,
                  blockCreationEndTime = majorBrief.header.endTime,
                  competingFallbackValidityStart = b.competingFallbackTxTime
                )
            }
            .map { seqs =>
                val settlements: List[SettlementTx] = seqs.map(_.settlementTx)
                val fallbacks: List[FallbackTx] = seqs.map(_.fallbackTx)
                val rollouts: List[RolloutTx] = seqs.flatMap(_.rolloutTxs)

                // TODO(next slice): per-partition Final + TrailingMinors construction
                // (finalization tx, standalone evac commit).
                StackEffects.Regular(
                  settlements = settlements,
                  fallbacks = fallbacks,
                  rollouts = rollouts,
                  refunds = refunds,
                  evacCommits = Nil,
                  finalization = None
                )
            }
    }

    /** Build the initial-stack (stack 0) effect bundle. The init tx is exogenous (head config); the
      * fallback tx is derived locally from `(initTx + headParams)`.
      *
      * TODO(slow-consensus): implement. Inputs come from the head config / bootstrap data; this
      * function should be callable without an L1LedgerM action since stack 0 happens before any
      * treasury rotation. Likely signature change once StackComposer's `Bootstrap` is in.
      */
    def deriveInitial: StackEffects.Initial =
        ??? // PR2: implement initialization-stack effect construction
}
