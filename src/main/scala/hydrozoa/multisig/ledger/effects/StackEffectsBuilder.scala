package hydrozoa.multisig.ledger.effects

import cats.implicits.*
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockResult, BlockType}
import hydrozoa.multisig.ledger.l1.L1LedgerM
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, FinalizationTx, RefundTx, RolloutTx, SettlementTx}
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
      *   - **Major**: call `L1LedgerM.mkSettlementTxSeq(...)` for settlement + fallback + rollouts
      *     using the block's `payoutObligations` + absorbed-deposit list. Treasury rotates through
      *     `L1LedgerM` state.
      *   - **Final**: call `L1LedgerM.finalizeLedger(...)` for finalization tx + rollouts. No
      *     refunds (drained via finalization). At most one Final per stack.
      *
      * Per-partition compression:
      *   - **TrailingMinors**: TODO — build a [[StandaloneEvacCommitTx]] over the partition's final
      *     cumulative evac-map state. The "necessary effects" rule keeps only the LAST standalone
      *     evac commit per such partition.
      *
      * The first settlement / finalization across the produced lists is the round-2 unlock; the
      * caller (StackComposer) is responsible for marking it as such when building the HardAck
      * payload.
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

        // Per-Major and per-Final, call the corresponding L1LedgerM helper. Walk in stack
        // order; treasury advances through L1LedgerM state on each call. At most one Final
        // block per stack (it terminates the chain), so we model the Final as Option[..].

        // Final must be the last block in the stack if present. Build a helper that emits the
        // L1LedgerM action for either a Major (SettlementTxSeq) or a Final (FinalizationTxSeq).
        sealed trait BlockOutput
        final case class FromMajor(seq: SettlementTxSeqOut) extends BlockOutput
        final case class FromFinal(seq: FinalizationTxSeqOut) extends BlockOutput

        // Type aliases to keep traverse local. We avoid pulling out the full enum imports.
        type SettlementTxSeqOut = hydrozoa.multisig.ledger.l1.txseq.SettlementTxSeq
        type FinalizationTxSeqOut = hydrozoa.multisig.ledger.l1.txseq.FinalizationTxSeq

        val unlockBlocks: List[BlockResult] = allBlocks.collect {
            case b if b.brief.isInstanceOf[BlockBrief.Major] => b
            case b if b.brief.isInstanceOf[BlockBrief.Final] => b
        }

        unlockBlocks
            .traverse { b =>
                b.brief match {
                    case majorBrief: BlockBrief.Major =>
                        L1LedgerM
                            .mkSettlementTxSeq(
                              nextKzg = majorBrief.header.kzgCommitment,
                              absorbedDeposits = b.absorbedDeposits,
                              payoutObligations = b.payoutObligations.toVector,
                              blockCreationEndTime = majorBrief.header.endTime,
                              competingFallbackValidityStart = b.competingFallbackTxTime
                            )
                            .map(FromMajor.apply: SettlementTxSeqOut => BlockOutput)
                    case _: BlockBrief.Final =>
                        L1LedgerM
                            .finalizeLedger(
                              payoutObligationsRemaining = b.payoutObligations.toVector,
                              competingFallbackValidityStart = b.competingFallbackTxTime
                            )
                            .map(FromFinal.apply: FinalizationTxSeqOut => BlockOutput)
                    case _: BlockBrief.Minor =>
                        // unreachable: unlockBlocks only contains Major / Final
                        L1LedgerM.pure[BlockOutput](
                          throw new IllegalStateException(
                            "unlockBlocks contained a non-Major non-Final brief"
                          )
                        )
                }
            }
            .map { outs =>
                val settlements: List[SettlementTx] = outs.collect { case FromMajor(s) =>
                    s.settlementTx
                }
                val fallbacks: List[FallbackTx] = outs.collect { case FromMajor(s) =>
                    s.fallbackTx
                }
                val majorRollouts: List[RolloutTx] = outs.collect { case FromMajor(s) =>
                    s.rolloutTxs
                }.flatten
                val finalizations: List[FinalizationTx] = outs.collect { case FromFinal(f) =>
                    f.finalizationTx
                }
                val finalRollouts: List[RolloutTx] = outs.collect { case FromFinal(f) =>
                    f.rolloutTxs
                }.flatten

                // TODO(next slice): TrailingMinors → StandaloneEvacCommitTx construction.
                StackEffects.Regular(
                  settlements = settlements,
                  fallbacks = fallbacks,
                  rollouts = majorRollouts ++ finalRollouts,
                  refunds = refunds,
                  evacCommits = Nil,
                  finalization = finalizations.headOption
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
