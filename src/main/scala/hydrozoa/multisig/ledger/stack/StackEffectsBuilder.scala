package hydrozoa.multisig.ledger.stack

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockResult, BlockType}
import hydrozoa.multisig.ledger.l1.L1LedgerM
import hydrozoa.multisig.ledger.l1.tx.RefundTx

/** Slow-side effect derivation: from the head-based [[StackPartition]]s of a regular stack, produce
  * **one [[PartitionEffects]] per partition** (PR #446 review — the partition is the effects
  * spine). An L1 effect is NOT always a transaction:
  *
  *   - **Transactions** — settlement / fallback / rollout / refund / finalization. Built via the
  *     existing [[L1LedgerM]] tx-construction helpers (`mkSettlementTxSeq`, `finalizeLedger`).
  *   - **Standalone evacuation commitments (SEC)** — NOT transactions. The dormant record
  *     [[StandaloneEvacuationCommitment]] presented to the L1 dispute-resolution scripts in the
  *     rules-based regime (only after a fallback) — never submitted immediately, never a treasury
  *     mutation. Per partition:
  *     - **Minor** partition → SEC for the latest minor, **mandatory**.
  *     - **Major** partition → SEC for the last trailing minor, **iff** the partition has >= 1
  *       trailing minor (NOT redundant with the settlement: its L1 execution is not guaranteed, and
  *       the post-settlement minors advance KZG past the settlement snapshot).
  *     - **Final** partition → none (Final is alone, no minors).
  *
  * Treasury rotation across stacks happens only via settlement / finalization (in partition order,
  * through `L1LedgerM` state), never via an SEC.
  *
  * Pure function of `(L1LedgerM state at stack open) + NonEmptyList[StackPartition]` — no
  * JointLedger lookup; all required state (incl. each SEC's minor header bytes) threaded through
  * inputs / carried on the effect itself.
  */
object StackEffectsBuilder {

    /** Build the regular-stack effect bundle: one [[PartitionEffects]] per [[StackPartition]], in
      * stack order, classified by partition kind:
      *
      *   - **Minor** partition -> [[PartitionEffects.Minor]]: mandatory SEC for the latest minor +
      *     those minors' post-dated refund txs.
      *   - **Major** partition -> [[PartitionEffects.Major]]: `mkSettlementTxSeq` (settlement +
      *     fallback + rollouts) for the opening Major + the trailing minors' refunds + an SEC for
      *     the last trailing minor iff the partition has >= 1 trailing minor.
      *   - **Final** partition -> [[PartitionEffects.Final]]: `finalizeLedger` (finalization +
      *     rollouts) for the lone Final block.
      *
      * Settlement / finalization run in partition order so the treasury rotates correctly through
      * `L1LedgerM` state. The round-2 *unlock* is selected structurally over the partition list by
      * the shared unlock-selection function (not chosen here).
      */
    def deriveRegular(
        partitions: NonEmptyList[StackPartition]
    ): L1LedgerM[StackEffects.Unsigned.Regular] = {
        type SettlementTxSeqOut = hydrozoa.multisig.ledger.l1.txseq.SettlementTxSeq
        type FinalizationTxSeqOut = hydrozoa.multisig.ledger.l1.txseq.FinalizationTxSeq

        def isMinor(b: BlockResult): Boolean = b.brief match {
            case _: BlockType.Minor => true
            case _                  => false
        }

        // Only minor blocks carry post-dated refund txs (Major/Final drain refunds via
        // settlement / finalization).
        def minorRefunds(bs: List[BlockResult]): List[RefundTx] =
            bs.flatMap(b => if isMinor(b) then b.postDatedRefundTxs else Nil)

        // SEC over a (minor) block — self-contained: carries the minor header signing bytes so
        // the signer/verifier need no BlockResult lookup (see StandaloneEvacuationCommitment).
        def secOf(b: BlockResult): StandaloneEvacuationCommitment = {
            val h = b.brief.header
            StandaloneEvacuationCommitment(
              committedBlockNum = b.brief.blockNum,
              blockVersion = h.blockVersion,
              kzgCommitment = h.kzgCommitment,
              header = h.signingBytes
            )
        }

        val perPartition
            : L1LedgerM[NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment]]] =
            partitions.traverse { p =>
                p.kind match {
                    case StackPartition.Kind.Major =>
                        val major = p.blocks.head
                        val trailingMinors = p.blocks.tail
                        major.brief match {
                            case mb: BlockBrief.Major =>
                                L1LedgerM
                                    .mkSettlementTxSeq(
                                      nextKzg = mb.header.kzgCommitment,
                                      absorbedDeposits = major.absorbedDeposits,
                                      payoutObligations = major.payoutObligations.toVector,
                                      blockCreationEndTime = mb.header.endTime,
                                      competingFallbackValidityStart = major.competingFallbackTxTime
                                    )
                                    .map { (seq: SettlementTxSeqOut) =>
                                        PartitionEffects.Major(
                                          settlement = seq.settlementTx,
                                          fallback = seq.fallbackTx,
                                          rollouts = seq.rolloutTxs,
                                          refunds = minorRefunds(trailingMinors),
                                          sec = trailingMinors.lastOption.map(secOf)
                                        ): PartitionEffects[StandaloneEvacuationCommitment]
                                    }
                            case _ =>
                                L1LedgerM
                                    .pure[PartitionEffects[StandaloneEvacuationCommitment]](
                                      throw new IllegalStateException(
                                        "Major partition's opener is not a Major block"
                                      )
                                    )
                        }
                    case StackPartition.Kind.Final =>
                        val fin = p.blocks.head
                        L1LedgerM
                            .finalizeLedger(
                              payoutObligationsRemaining = fin.payoutObligations.toVector,
                              competingFallbackValidityStart = fin.competingFallbackTxTime
                            )
                            .map { (seq: FinalizationTxSeqOut) =>
                                PartitionEffects.Final(
                                  finalization = seq.finalizationTx,
                                  rollouts = seq.rolloutTxs
                                ): PartitionEffects[StandaloneEvacuationCommitment]
                            }
                    case StackPartition.Kind.Minor =>
                        L1LedgerM.pure[PartitionEffects[StandaloneEvacuationCommitment]](
                          PartitionEffects.Minor(
                            sec = secOf(p.blocks.last),
                            refunds = minorRefunds(p.blocks.toList)
                          )
                        )
                    case StackPartition.Kind.Initial =>
                        L1LedgerM.pure[PartitionEffects[StandaloneEvacuationCommitment]](
                          throw new IllegalStateException(
                            "deriveRegular received an Initial partition (stack 0 uses the " +
                                "separate Initial path)"
                          )
                        )
                }
            }

        perPartition.map(StackEffects.Unsigned.Regular.apply)
    }

    /** Build the initial-stack (stack 0) effect bundle. The init tx is exogenous (head config); the
      * fallback tx is derived locally from `(initTx + headParams)`.
      *
      * TODO(slow-consensus): implement. Inputs come from the head config / bootstrap data; this
      * function should be callable without an L1LedgerM action since stack 0 happens before any
      * treasury rotation. Likely signature change once StackComposer's `Bootstrap` is in.
      */
    def deriveInitial: StackEffects.Unsigned.Initial =
        ??? // PR2: implement initialization-stack effect construction
}
