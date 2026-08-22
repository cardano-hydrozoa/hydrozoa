package hydrozoa.rulebased.evacuator

import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import hydrozoa.rulebased.ledger.l1.script.plutus.SetupLadder
import scalus.cardano.ledger.ProtocolParams

/** Chooses how many payouts to put in the next `Evacuate`.
  *
  * The existing bot builds a batch, and on rejection retries with half of it — so every oversized
  * attempt is a wasted build plus a wasted phase-2 evaluation, and a batch that fits at 14 can be
  * dropped to 8 by one halving. This picks the size up front instead.
  *
  * The cost model is affine in the batch size, because that is what the validator actually does:
  * one fixed setup (parse the datum, locate and authenticate the reference inputs, one final
  * pairing check) plus a per-entry cost dominated by the BLS G2 scalar multiplication and
  * uncompression. Payout shape barely enters it — `serialiseData` + `blake2b_224` over an entry is
  * under 4% of its cost — so a single fitted line predicts a batch closely enough to pick `k`, and
  * the phase-2 evaluation that follows is what actually certifies it.
  *
  * Constants fitted against landed `Evacuate` txs at k=8, 13 and 14, accurate to 0.13%.
  */
object BatchPlanner {

    /** Per-tx cost that does not depend on the batch size. */
    val fixedSteps: Long = 1_545_443_644L

    /** Marginal cost of one more payout in the batch. */
    val stepsPerPayout: Long = 585_696_440L

    /** The largest batch the deployed setup ladder can prove, read from the ladder itself rather
      * than restated here — a rung covering `k` evacuations needs `k + 1` G2 points, so the top
      * rung's reach follows from `rungCount`.
      */
    val ladderMax: Int = 1 << (SetupLadder.rungCount - 1)

    /** Largest batch whose predicted CPU cost still fits `maxTxSteps`, clamped to what is actually
      * available and to the ladder's reach.
      *
      * Steps are the binding constraint: at k=14 a landed tx sat at 97.5% of the CPU limit while
      * using 10.4% of the size budget and 27.7% of the memory budget. So this solves for steps and
      * lets the builder's own validation catch the rare shape that breaks the other two.
      *
      * @param safetyMargin
      *   fraction of the limit left unused, absorbing the model's error rather than betting the
      *   whole batch on it. The fit is good to 0.13%, so 2% is roughly fifteen times the observed
      *   error — cheap insurance, since one rejected build costs far more than one fewer payout.
      */
    def maxBatchSize(
        available: Int,
        params: ProtocolParams,
        safetyMargin: Double = 0.02
    ): Int = {
        val maxTxSteps = params.maxTxExecutionUnits.steps.toLong
        val budget = (maxTxSteps * (1.0 - safetyMargin)).toLong - fixedSteps
        // Narrow only after clamping: a generous limit divides out to more than `Int.MaxValue`
        // payouts, and converting that first wraps negative — which reads as "nothing fits" and
        // silently drops the batch to one.
        val fits = if budget <= 0 then 0L else budget / stepsPerPayout
        val capped = fits.min(available.toLong).min(ladderMax.toLong)
        math.max(1, capped.toInt)
    }

    /** Predicted CPU steps for a batch of `k`. */
    def predictedSteps(k: Int): Long = fixedSteps + stepsPerPayout * k

    /** How many of our txs the chain will accept per block — the ceiling no client-side cleverness
      * can raise, since the block ex-unit limit is shared by every transaction in it.
      *
      * Worth knowing rather than discovering: it is what makes a maximal `k` matter. Two txs a
      * block at k=14 drains twice as fast as two at k=8, and no amount of submitting harder changes
      * the divisor.
      */
    def txsPerBlock(k: Int, params: ProtocolParams): Long =
        params.maxBlockExecutionUnits.steps.toLong / predictedSteps(k)

    /** The next batch to evacuate: the first `k` entries in map order.
      *
      * Order is the map's own — ascending by evacuation key — which keeps the batch a contiguous
      * prefix. That matters for a chained run: each successive residual is again a prefix-free
      * suffix, so the sequence of batches partitions the map without any bookkeeping beyond "how
      * far have we got".
      */
    def nextBatch(outstanding: EvacuationMap, params: ProtocolParams): EvacuationMap = {
        val k = maxBatchSize(outstanding.size, params)
        EvacuationMap(outstanding.evacuationMap.take(k))
    }
}
