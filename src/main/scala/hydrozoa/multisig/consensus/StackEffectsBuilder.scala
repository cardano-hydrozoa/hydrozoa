package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import cats.syntax.all.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, FallbackTxStartTime}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber, BlockResult, BlockVersion}
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.EvacuationMap.applyDiffs
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationDiffGroup, EvacuationMap}
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, FinalizationTx, InitializationTx, RefundTx, RolloutTx, SettlementTx}
import hydrozoa.multisig.ledger.l1.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.l1.utxo.{DepositUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects, StackPartition, StandaloneEvacuationCommitment}
import scalus.cardano.ledger.{TransactionHash, Value}

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
        initialTreasury: MultisigTreasuryUtxo,
        partitions: NonEmptyList[StackPartition],
        initialEvacuationMap: EvacuationMap,
    ): Either[
      Error,
      (
          StackEffects.Unsigned.Regular,
          MultisigTreasuryUtxo,
          EvacuationMap,
          // The withdrawal-effect tracking this stack contributes: each `(requestId, l1TxId)` links
          // a withdrawing request to a settlement / rollout / finalization tx that pays one of its
          // L1-bound outputs. A local-only side value — never inside the signed effects.
          List[(RequestId, TransactionHash)]
      )
    ] = {

        // Post-dated refund txs the partition's blocks carry — one per deposit REGISTERED in a
        // block, of ANY kind (minor or major).
        def partitionRefunds(bs: List[BlockResult]): List[RefundTx] =
            bs.flatMap(_.postDatedRefundTxs)

        // Withdrawal-effect tracking for a partition: the withdrawals are the **prefix**
        // `withdrawalRequestIds` of the partition's payout-obligation vector — positions `[0, N)`,
        // one request id each — and any obligations at or beyond `N` are finalization residual
        // balances, not withdrawals. `slices` are the effect txs in **offset order** (settlement /
        // finalization first, then the rollouts), each with its `payoutCount`; the vector is
        // discharged as a forward run of contiguous slices, so a tx's offset is the running sum of
        // the preceding `payoutCount`s. Each distinct withdrawing request in a tx's in-prefix slice
        // contributes one `(requestId, l1TxId)` link; residual positions contribute none.
        def trackWithdrawals(
            withdrawalRequestIds: Vector[RequestId],
            slices: List[(Int, TransactionHash)]
        ): List[(RequestId, TransactionHash)] =
            val n = withdrawalRequestIds.length
            slices
                .foldLeft((0, List.empty[(RequestId, TransactionHash)])) {
                    case ((offset, acc), (count, l1TxId)) =>
                        val hi = math.min(offset + count, n)
                        val rows =
                            if hi > offset then
                                withdrawalRequestIds
                                    .slice(offset, hi)
                                    .distinct
                                    .map(r => (r, l1TxId))
                            else Nil
                        (offset + count, acc ++ rows)
                }
                ._2

        // The effect txs discharging the payout vector, in offset order: the treasury tx's direct
        // slice first (it takes `[0, payoutCount)` — the merged first rollout, when it fits), then
        // each rollout in chain order — as `(payoutCount, l1TxId)`.
        def settlementSlices(seq: SettlementTxSeq): List[(Int, TransactionHash)] =
            val direct = seq.settlementTx match
                case wp: SettlementTx.WithPayouts =>
                    List((wp.payoutCount, seq.settlementTx.tx.id))
                case _ => Nil
            direct ++ rolloutSlices(seq.rolloutTxs)

        def finalizationSlices(seq: FinalizationTxSeq): List[(Int, TransactionHash)] =
            val direct = seq.finalizationTx match
                case wp: FinalizationTx.WithPayouts =>
                    List((wp.payoutCount, seq.finalizationTx.tx.id))
                case _ => Nil
            direct ++ rolloutSlices(seq.rolloutTxs)

        def rolloutSlices(rollouts: List[RolloutTx]): List[(Int, TransactionHash)] =
            rollouts.map(rt => (rt.payoutCount, rt.tx.id))

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
        val seed = Acc(initialTreasury, initialEvacuationMap)

        val folded: Either[Error, Acc] = partitions.toList.foldM(seed) { (acc, p) =>
            import acc.*
            // Conservation gate first — cheap and pure, before any tx building: each block's
            // reported diffs must move the map by exactly what crossed the L1 boundary.
            checkPartitionConservation(evacuationMap, p).flatMap { _ =>
                p.kind match {
                    case StackPartition.Kind.Major =>
                        val major = p.blocks.head
                        val trailingMinors = p.blocks.tail
                        val mapAfterMajor = applyDiffs(evacuationMap, major.flatEvacuationDiffs)
                        // Apply trailing minors' diffs cumulatively; the LAST minor's
                        // post-map provides the SEC's KZG (if any trailing minor exists).
                        val mapAfterPartition =
                            trailingMinors.foldLeft(mapAfterMajor)((m, b) =>
                                applyDiffs(m, b.flatEvacuationDiffs)
                            )
                        major.brief match {
                            case mb: BlockBrief.Major =>
                                mkSettlementTxSeq(
                                  config = config,
                                  treasury = treasury,
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
                                    // The settlement drains the major block's withdrawals — every
                                    // obligation is a real request (the settlement input is
                                    // `major.payoutObligations`), so the whole vector is the prefix.
                                    val newWithdrawals =
                                        trackWithdrawals(
                                          major.payoutRequestIds.toVector,
                                          settlementSlices(seq)
                                        )
                                    acc.copy(
                                      treasury = newTreasury,
                                      evacuationMap = mapAfterPartition,
                                      effectsReversed = pe :: effectsReversed,
                                      withdrawalTracking = newWithdrawals ++ withdrawalTracking
                                    )
                                }
                            case _ =>
                                throw new IllegalStateException(
                                  "Major partition's opener is not a Major block"
                                )
                        }
                    case StackPartition.Kind.Final =>
                        // The finalization tx pays out two things: the final block's OWN withdrawals
                        // (`fin.payoutObligations` — real L2 requests), and the residual L2 balances.
                        // Like a
                        // Major, the final block carries its own `evacuationMapDiff` (the final
                        // window's L2 mutations); we fold it into the running map so `mapAfterFinal`
                        // is the true post-final residual.
                        // Withdrawals come first so they stay a
                        // recognizable prefix (they carry request provenance; the residual balances
                        // do not).
                        val fin = p.blocks.head
                        val mapAfterFinal = applyDiffs(evacuationMap, fin.flatEvacuationDiffs)
                        val payoutObligationsRemaining =
                            fin.payoutObligations.toVector ++ mapAfterFinal.outputs.toVector
                        finalizeLedger(
                          config = config,
                          treasury = treasury,
                          payoutObligationsRemaining = payoutObligationsRemaining,
                          competingFallbackValidityStart = fin.competingFallbackTxTime
                        ).map { seq =>
                            val pe = PartitionEffects.Final(
                              finalization = seq.finalizationTx,
                              rollouts = seq.rolloutTxs
                            ): PartitionEffects[StandaloneEvacuationCommitment]
                            // The final block's withdrawals are the prefix of the combined
                            // finalization input (`fin.payoutObligations ++ residual`), so its
                            // request ids `[0, N)` are exactly the withdrawal positions; the residual
                            // balances after them are not withdrawals.
                            val newWithdrawals =
                                trackWithdrawals(
                                  fin.payoutRequestIds.toVector,
                                  finalizationSlices(seq)
                                )
                            acc.copy(
                              evacuationMap = EvacuationMap.empty,
                              effectsReversed = pe :: effectsReversed,
                              withdrawalTracking = newWithdrawals ++ withdrawalTracking
                            )
                        }
                    case StackPartition.Kind.Minor =>
                        val mapAfterRun = p.blocks.toList.foldLeft(evacuationMap)((m, b) =>
                            applyDiffs(m, b.flatEvacuationDiffs)
                        )
                        val pe = PartitionEffects.Minor(
                          sec = secOf(p.blocks.last, mapAfterRun.kzgCommitment),
                          refunds = partitionRefunds(p.blocks.toList)
                        ): PartitionEffects[StandaloneEvacuationCommitment]
                        // Minor blocks carry no withdrawals (a withdrawal forces a Major
                        // block); a minor partition leaves the treasury untouched.
                        Right(
                          acc.copy(
                            evacuationMap = mapAfterRun,
                            effectsReversed = pe :: effectsReversed
                          )
                        )
                    case StackPartition.Kind.Initial =>
                        throw new IllegalStateException(
                          "mkEffectsRegular received an Initial partition " +
                              "(stack 0 uses mkEffectsInitial)"
                        )
                }
            }
        }

        folded.map { case Acc(finalTreasury, finalMap, effectsReversed, withdrawalTracking) =>
            val nel = NonEmptyList
                .fromList(effectsReversed.reverse)
                .getOrElse(
                  throw new IllegalStateException("Empty partition list — impossible")
                )
            (StackEffects.Unsigned.Regular(nel), finalTreasury, finalMap, withdrawalTracking)
        }
    }

    /** Accumulator threaded through [[mkEffectsRegular]]'s partition fold: the treasury and
      * cumulative evacuation map after the partitions consumed so far, the [[PartitionEffects]]
      * built so far (newest first), and the withdrawal-tracking pairs collected so far. The
      * defaults are the fold's seed: nothing built yet, starting from the stack's opening treasury
      * and map.
      */
    private final case class Acc(
        treasury: MultisigTreasuryUtxo,
        evacuationMap: EvacuationMap,
        effectsReversed: List[PartitionEffects[StandaloneEvacuationCommitment]] = Nil,
        withdrawalTracking: List[(RequestId, TransactionHash)] = Nil
    )

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

    /** Check L2 value conservation across a partition's blocks, one L2 command at a time: folding
      * an [[EvacuationDiffGroup]] must change the running map's total value by exactly what that
      * command moved across the L1 boundary — a transaction group by minus its payouts (zero for a
      * pure L2 transfer), the deposit-decisions group by the absorbed deposits' `l2Value` — in the
      * coin and in every asset. Anything else over- or under-credits an L2 account: a remote L2
      * controls the reports, so a mismatched delta (e.g. a rounding error minting a token unit, or
      * an account silently drained) is rejected before any effect tx is built or signed. Per
      * command, not per block: a block's aggregate delta can be zero while individual transactions
      * err in compensating directions. A final block-level identity (`Σ groups == absorbed −
      * payouts`) backstops the shape — it catches absorbed deposits whose decisions group never
      * arrived, which no per-group check would demand.
      */
    // TODO(#622-review): this folds `applyDiffsWithDelta` over every block's diffs to compute the
    // deltas, but the `mkEffectsRegular` match arms then re-fold `applyDiffs` over those diffs to
    // thread the running map — the map fold runs twice per partition. `applyDiffsWithDelta` already
    // yields the updated map; have this return it (or the per-block maps) so the arms reuse it
    // instead of re-folding, halving the per-partition fold on a large map.
    private def checkPartitionConservation(
        startMap: EvacuationMap,
        partition: StackPartition
    ): Either[Error, Unit] =
        partition.blocks.toList
            .foldM(startMap) { (runMap, block) =>
                val blockNum = block.brief.blockNum
                val absorbed = Value.combine(block.absorbedDeposits.map(_.l2Value))
                val paidOut = Value.combine(block.payoutObligations.map(_.utxo.value.value))
                // A transaction's payouts, summed by its producing request (`payoutObligations`
                // and `payoutRequestIds` are aligned).
                val paidOutByRequest: Map[RequestId, Value] =
                    block.payoutRequestIds
                        .zip(block.payoutObligations)
                        .groupMapReduce(_._1)(_._2.utxo.value.value)(_ + _)
                for {
                    checkedGroups <- block.evacuationMapDiff.toList
                        .foldM((runMap, Value.zero)) { case ((m, blockDelta), group) =>
                            val (next, actual) = EvacuationMap.applyDiffsWithDelta(m, group.diffs)
                            val (origin, expected) = group match {
                                case EvacuationDiffGroup.Transaction(requestId, _) =>
                                    (
                                      Some(requestId),
                                      -paidOutByRequest.getOrElse(requestId, Value.zero)
                                    )
                                case EvacuationDiffGroup.DepositDecisions(_) => (None, absorbed)
                            }
                            Either.cond(
                              (actual - expected).isZero,
                              (next, blockDelta + actual),
                              Error.EvacuationMapNotConserved(blockNum, origin, expected, actual)
                            )
                        }
                    (next, blockDelta) = checkedGroups
                    blockExpected = absorbed - paidOut
                    _ <- Either.cond(
                      (blockDelta - blockExpected).isZero,
                      (),
                      Error.EvacuationMapNotConserved(blockNum, None, blockExpected, blockDelta)
                    )
                } yield next
            }
            .void

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

        /** An L2 command's reported diff group breaks value conservation: folding it moved the
          * map's total value by `actualDelta`, but the command's L1 boundary crossings account for
          * `expectedDelta` (a transaction: minus its payouts; the deposit-decisions command: the
          * absorbed deposits' `l2Value`). The difference over- or under-credits an L2 account — a
          * bogus or malicious L2 report — so the stack is rejected.
          *
          * @param origin
          *   the producing request for a transaction group; `None` for the block's
          *   deposit-decisions command (or the block-level aggregate backstop).
          */
        final case class EvacuationMapNotConserved(
            blockNum: BlockNumber,
            origin: Option[RequestId],
            expectedDelta: Value,
            actualDelta: Value
        ) extends Error {
            override def toString: String =
                s"Evacuation map diffs of ${origin.fold(
                      "the deposit-decisions command / block " +
                          "aggregate"
                    )(r => s"request $r")} at block $blockNum break L2 value" +
                    s" conservation: the map changed by $actualDelta, but the command's L1" +
                    s" boundary crossings account for $expectedDelta"
        }
    }
}
