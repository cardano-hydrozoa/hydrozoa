package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, FallbackTxStartTime}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockResult, BlockVersion}
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.joint.EvacuationMap.applyDiffs
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, InitializationTx, RefundTx}
import hydrozoa.multisig.ledger.l1.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.l1.utxo.{DepositUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects, StackPartition, StandaloneEvacuationCommitment}

/** Effect derivation for a closed stack. Two entry points:
  *
  *   - [[mkEffectsInitial]] — the stack-0 bundle: the init + fallback bodies, no partitions.
  *   - [[mkEffectsRegular]] — stacks 1+: from the [[StackPartition]]s, **one [[PartitionEffects]]
  *     per partition** (the partition is the effects spine).
  *
  * An L1 effect is NOT always a transaction:
  *
  *   - **Transactions** — settlement / fallback / rollout / refund / finalization. Built via the
  *     treasury helpers below (`mkSettlementTxSeq`, `finalizeLedger`).
  *   - **Standalone evacuation commitments (SEC)** — NOT transactions. The dormant record
  *     [[StandaloneEvacuationCommitment]] presented to the L1 dispute-resolution scripts in the
  *     rules-based regime (only after a fallback) — never submitted immediately, never a treasury
  *     mutation. Per partition:
  *     - **Minor** partition → SEC for the latest minor, **mandatory**.
  *     - **Major** partition → SEC for the last trailing minor, **iff** the partition has >= 1
  *       trailing minor (NOT redundant with the settlement: its L1 execution is not guaranteed, and
  *       the post-settlement minors advance KZG past the settlement snapshot).
  *     - **Final** partition → none (Final is alone, no minors).
  */
object StackEffectsBuilder {

    /** Bundle the (exogenous) initialization tx and the locally-derived fallback tx — both unsigned
      * bodies — into the stack-0 effect set. Stack 0 precedes any treasury rotation, so — unlike
      * [[mkEffectsRegular]] — there is no treasury to thread or return; the bundle is multisigned
      * later via the Initial two-phase hard-ack flow.
      */
    def mkEffectsInitial(
        initializationTx: InitializationTx,
        fallbackTx: FallbackTx
    ): StackEffects.Unsigned.Initial =
        StackEffects.Unsigned.Initial(initializationTx, fallbackTx)

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
      * Settlement / finalization run in partition order so the treasury rotates correctly; the
      * rotated treasury is returned. The round-2 *unlock* is selected structurally over the
      * partition list by the shared unlock-selection function (not chosen here).
      */
    def mkEffectsRegular(
        config: Config, // TODO: narrow?
        treasury: MultisigTreasuryUtxo,
        partitions: NonEmptyList[StackPartition],
        initialEvacuationMap: EvacuationMap,
    ): Either[Error, (StackEffects.Unsigned.Regular, MultisigTreasuryUtxo, EvacuationMap)] = {

        // Post-dated refund txs the partition's blocks carry — one per deposit REGISTERED in a
        // block, of ANY kind (minor or major).
        def partitionRefunds(bs: List[BlockResult]): List[RefundTx] =
            bs.flatMap(_.postDatedRefundTxs)

        // SEC over a (minor) block — self-contained: carries the serialized on-chain commitment
        // bytes so the signer/verifier need no BlockResult lookup (see StandaloneEvacuationCommitment).
        // The SEC's `header` is the on-chain commitment record (with KZG), built directly from
        // `SEC.Onchain` rather than via the fast-cycle `signingBytes` path, so the soft-ack signing
        // shape (no KZG) and the on-chain SEC shape (with KZG) stay independent.
        //
        // `kzg` is the KZG commitment of the evacuation map at the END of the block being
        // committed (computed slow-side by folding diffs over the running map).
        val headId = config.headTokenNames.treasuryTokenName.bytes
        def secOf(b: BlockResult, kzg: KzgCommitment): StandaloneEvacuationCommitment = {
            val h = b.brief.header
            StandaloneEvacuationCommitment(
              blockNum = b.brief.blockNum,
              blockVersion = h.blockVersion,
              kzgCommitment = kzg,
              header = StandaloneEvacuationCommitment.Onchain.Serialized(
                StandaloneEvacuationCommitment.Onchain(headId, h, kzg)
              )
            )
        }

        // Walk partitions in stack order, threading the cumulative evacuation map AND the treasury
        // explicitly (no ledger monad). KZG is computed lazily (via `EvacuationMap.kzgCommitment`)
        // only at the blocks that need it: each Major (for its settlement's `nextKzg`) and each
        // last-of-partition minor (for its SEC). Other minors in a run only get their diffs
        // applied; no KZG paid. The fold short-circuits on the first treasury-build `Left`.
        type Acc =
            (
                List[PartitionEffects[StandaloneEvacuationCommitment]],
                MultisigTreasuryUtxo,
                EvacuationMap
            )

        val seed: Either[Error, Acc] = Right((Nil, treasury, initialEvacuationMap))

        val folded: Either[Error, Acc] = partitions.toList.foldLeft(seed) { (accE, p) =>
            accE.flatMap { case (effectsAcc, tre, runningMap) =>
                p.kind match {
                    case StackPartition.Kind.Major =>
                        val major = p.blocks.head
                        val trailingMinors = p.blocks.tail
                        val mapAfterMajor = applyDiffs(runningMap, major.evacuationMapDiff)
                        // Apply trailing minors' diffs cumulatively; the LAST minor's
                        // post-map provides the SEC's KZG (if any trailing minor exists).
                        val mapAfterPartition =
                            trailingMinors.foldLeft(mapAfterMajor)((m, b) =>
                                applyDiffs(m, b.evacuationMapDiff)
                            )
                        major.brief match {
                            case mb: BlockBrief.Major =>
                                mkSettlementTxSeq(
                                  config = config,
                                  treasury = tre,
                                  nextKzg = mapAfterMajor.kzgCommitment,
                                  absorbedDeposits = major.absorbedDeposits,
                                  payoutObligations = major.payoutObligations.toVector,
                                  blockCreationEndTime = mb.header.endTime,
                                  competingFallbackValidityStart = major.competingFallbackTxTime
                                ).map { case (newTreasury, seq) =>
                                    val sec = trailingMinors.lastOption
                                        .map(b => secOf(b, mapAfterPartition.kzgCommitment))
                                    val pe = PartitionEffects.Major(
                                      settlement = seq.settlementTx,
                                      fallback = seq.fallbackTx,
                                      rollouts = seq.rolloutTxs,
                                      refunds = partitionRefunds(p.blocks.toList),
                                      sec = sec
                                    ): PartitionEffects[StandaloneEvacuationCommitment]
                                    (pe :: effectsAcc, newTreasury, mapAfterPartition)
                                }
                            case _ =>
                                throw new IllegalStateException(
                                  "Major partition's opener is not a Major block"
                                )
                        }
                    case StackPartition.Kind.Final =>
                        // The finalization tx pays out two things: the final block's OWN withdrawals
                        // (`fin.payoutObligations` — real L2 requests, never settled by a Major
                        // since the final block is not Major), and the residual L2 balances. The
                        // fast side does not maintain the cumulative evac map, so it emits no
                        // `delete-all` diff and leaves the residual drain to us: we recover it from
                        // the slow-side running map (`runningMap.outputs`). Withdrawals come first so
                        // they stay a recognizable prefix (they carry request provenance; the
                        // residual balances do not). The map is reset to empty for any subsequent
                        // partition / stack; the treasury is drained by finalization (chain ends).
                        val fin = p.blocks.head
                        val payoutObligationsRemaining =
                            fin.payoutObligations.toVector ++ runningMap.outputs.toVector
                        finalizeLedger(
                          config = config,
                          treasury = tre,
                          payoutObligationsRemaining = payoutObligationsRemaining,
                          competingFallbackValidityStart = fin.competingFallbackTxTime
                        ).map { seq =>
                            val pe = PartitionEffects.Final(
                              finalization = seq.finalizationTx,
                              rollouts = seq.rolloutTxs
                            ): PartitionEffects[StandaloneEvacuationCommitment]
                            (pe :: effectsAcc, tre, EvacuationMap.empty)
                        }
                    case StackPartition.Kind.Minor =>
                        val mapAfterRun = p.blocks.toList.foldLeft(runningMap)((m, b) =>
                            applyDiffs(m, b.evacuationMapDiff)
                        )
                        val pe = PartitionEffects.Minor(
                          sec = secOf(p.blocks.last, mapAfterRun.kzgCommitment),
                          refunds = partitionRefunds(p.blocks.toList)
                        ): PartitionEffects[StandaloneEvacuationCommitment]
                        Right((pe :: effectsAcc, tre, mapAfterRun))
                    case StackPartition.Kind.Initial =>
                        throw new IllegalStateException(
                          "mkEffectsRegular received an Initial partition " +
                              "(stack 0 uses mkEffectsInitial)"
                        )
                }
            }
        }

        folded.map { case (effectsReversed, finalTreasury, finalMap) =>
            val nel = NonEmptyList
                .fromList(effectsReversed.reverse)
                .getOrElse(
                  throw new IllegalStateException("Empty partition list — impossible")
                )
            (StackEffects.Unsigned.Regular(nel), finalTreasury, finalMap)
        }
    }

    /** Construct a settlement tx seq and return the treasury it produces. The collective value of
      * the payouts must '''not''' exceed the treasury value.
      */
    private def mkSettlementTxSeq(
        config: Config, // TODO: narrow?
        treasury: MultisigTreasuryUtxo,
        nextKzg: KzgCommitment,
        absorbedDeposits: List[DepositUtxo],
        payoutObligations: Vector[Payout.Obligation],
        blockCreationEndTime: BlockCreationEndTime,
        competingFallbackValidityStart: FallbackTxStartTime,
    ): Either[Error, (MultisigTreasuryUtxo, SettlementTxSeq)] = {
        val majorVersionProduced = BlockVersion.Major(treasury.datum.versionMajor.toInt + 1)
        SettlementTxSeq
            .Build(config)(
              kzgCommitment = nextKzg,
              majorVersionProduced = majorVersionProduced,
              treasuryToSpend = treasury,
              depositsToSpend = absorbedDeposits,
              payoutObligationsRemaining = payoutObligations,
              competingFallbackValidityStart = competingFallbackValidityStart,
              blockCreationEndTime = blockCreationEndTime
            )
            .result
            .left
            .map(Error.SettlementTxSeqBuilderError.apply)
            .map(seq => (seq.settlementTx.treasuryProduced, seq))
    }

    /** Construct a finalization tx seq for the lone Final block. The treasury is fully drained by
      * finalization, so it is NOT returned (the slow-side treasury chain ends here). The collective
      * value of the payouts must '''not''' exceed the treasury value.
      */
    // TODO (fund14): add Refund.Immediates to the return type
    private def finalizeLedger(
        config: Config, // TODO: narrow?
        treasury: MultisigTreasuryUtxo,
        payoutObligationsRemaining: Vector[Payout.Obligation],
        competingFallbackValidityStart: FallbackTxStartTime,
    ): Either[Error, FinalizationTxSeq] =
        FinalizationTxSeq
            .Build(config)(
              majorVersionProduced =
                  BlockVersion.Major(treasury.datum.versionMajor.toInt).increment,
              treasuryToSpend = treasury,
              payoutObligationsRemaining = payoutObligationsRemaining,
              competingFallbackValidityStart = competingFallbackValidityStart
            )
            .result
            .left
            .map(Error.FinalizationTxSeqBuilderError.apply)

    type Config = HeadConfig.Section

    /** Failure building the treasury-spending effects of a regular stack. Raised by the caller. */
    sealed trait Error extends Throwable

    object Error {
        final case class SettlementTxSeqBuilderError(wrapped: SettlementTxSeq.Build.Error)
            extends Error {
            override def toString: String = "Settlement tx-seq error:\n" + wrapped.toString
        }

        final case class FinalizationTxSeqBuilderError(wrapped: FinalizationTxSeq.Build.Error)
            extends Error {
            override def toString: String = "Finalization tx-seq error:\n" + wrapped.toString
        }
    }
}
