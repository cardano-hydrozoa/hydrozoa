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
  * uncompression, which are the same work whatever the entry holds.
  *
  * Payout shape is a second-order correction, not a free variable. An entry is hashed to a scalar
  * through `serialiseData` then `blake2b_224`, both charged on the entry's size — so a fatter entry
  * does cost more, just far less than the BLS work that dominates. Measured across the two shapes
  * to hand, the spread is about 4.4% per payout: plain enterprise outputs charge 585.7M steps each,
  * while outputs carrying an inline datum charge 611.5M.
  *
  * So the line is fitted to the cheap shape and [[shapeAllowance]] covers the spread, rather than
  * the model pretending to a precision it does not have. Getting this backwards is expensive in one
  * direction only: an underestimate is discovered when a built transaction is rejected, having
  * already paid for the build and its phase-2 evaluation.
  */
object BatchPlanner {

    /** Per-tx cost that does not depend on the batch size.
      *
      * Fitted against `Evacuate` transactions that landed on preview at k=8, 13 and 14, where it
      * reproduces the charged cost to within 0.13%.
      */
    val fixedSteps: Long = 1_545_443_644L

    /** Marginal cost of one more payout, for the cheapest entry shape (an enterprise output with no
      * datum) — the shape the running head's map is made of.
      */
    val stepsPerPayout: Long = 585_696_440L

    /** The entry size the fitted rate was measured at: an enterprise output holding only ada, whose
      * raw CBOR is 39 bytes. The demo stand's map is made of these.
      */
    val fittedEntryBytes: Int = 39

    /** Marginal cost of one more byte of payout, above [[fittedEntryBytes]].
      *
      * An entry is hashed through `serialiseData` then `blake2b_224`, and both are charged on size:
      * `serialiseData` on the Data memory usage (slope 213,312, and a byte contributes about an
      * eighth of a unit) and `blake2b_224` on the byte length (slope 8,310). That predicts ~34,974
      * steps per byte, which reproduces the one independent measurement to hand — a fixture whose
      * entries carry a ~775-byte inline datum charged 611.5M per payout against the 585.7M of a
      * 39-byte entry.
      */
    val stepsPerEntryByte: Long = 34_974L

    /** Head-room for the model's own error, used when the batch is sized without measuring the map.
      *
      * Covers the widest per-payout spread seen between entry shapes (4.4%) plus slack. Sizing
      * against a measured map does not need it — see [[maxBatchSizeFor]].
      */
    val shapeAllowance: Double = 0.06

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
      *   fraction of the limit left unused on top of [[shapeAllowance]], covering the fit's own
      *   residual error (0.13% against the transactions it was fitted to).
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
        val fits = if budget <= 0 then 0L else budget / dearestStepsPerPayout
        val capped = fits.min(available.toLong).min(ladderMax.toLong)
        math.max(1, capped.toInt)
    }

    /** Per-payout cost assumed when choosing a batch: the fitted rate, raised to cover the
      * costliest entry shape seen. Sizing the batch on the cheap shape would overshoot on any map
      * whose entries carry datums.
      */
    def dearestStepsPerPayout: Long = (stepsPerPayout * (1.0 + shapeAllowance)).toLong

    /** Predicted CPU steps for a batch of `k`, at the rate the batch was sized against — so this is
      * an upper bound across the entry shapes measured, not a point estimate for one of them.
      */
    def predictedSteps(k: Int): Long = fixedSteps + dearestStepsPerPayout * k

    /** Predicted CPU steps for a batch of `k` made purely of the cheapest entry shape. Reproduces
      * the cost of the transactions the line was fitted to.
      */
    def predictedStepsPlainShape(k: Int): Long = fixedSteps + stepsPerPayout * k

    /** Marginal cost of one payout whose raw CBOR is `entryBytes` long.
      *
      * Below the fitted size this returns the fitted rate rather than extrapolating downward: the
      * line was measured at 39 bytes and there is no evidence it stays linear beneath that, and
      * guessing low is the direction that costs a rejected build.
      */
    def stepsPerPayoutOfSize(entryBytes: Int): Long =
        stepsPerPayout + stepsPerEntryByte * math.max(0, entryBytes - fittedEntryBytes)

    /** The batch size for a map we can measure, rather than one we must guess at.
      *
      * The outstanding set is in hand whenever a batch is chosen, so its costliest entry is a fact,
      * not a risk to be padded against. Sizing on that measured worst case is both safer than
      * [[maxBatchSize]] — it cannot be surprised by an entry fatter than the allowance anticipated
      * — and less wasteful, since a map of small entries is no longer charged for datums it does
      * not carry.
      */
    def maxBatchSizeFor(
        outstanding: EvacuationMap,
        params: ProtocolParams,
        safetyMargin: Double = 0.02
    ): Int = {
        if outstanding.isEmpty then 1
        else {
            val dearestEntry = outstanding.evacuationMap.values.map(_.outputSize).max
            val perPayout = stepsPerPayoutOfSize(dearestEntry)
            val maxTxSteps = params.maxTxExecutionUnits.steps.toLong
            val budget = (maxTxSteps * (1.0 - safetyMargin)).toLong - fixedSteps
            val fits = if budget <= 0 then 0L else budget / perPayout
            val capped = fits.min(outstanding.size.toLong).min(ladderMax.toLong)
            math.max(1, capped.toInt)
        }
    }

    /** How many of our txs the chain will accept per block — the ceiling no client-side cleverness
      * can raise, since the block ex-unit limit is shared by every transaction in it.
      *
      * Worth knowing rather than discovering: it is what makes a maximal `k` matter. Two txs a
      * block at k=14 drains twice as fast as two at k=8, and no amount of submitting harder changes
      * the divisor.
      */
    def txsPerBlock(k: Int, params: ProtocolParams): Long =
        params.maxBlockExecutionUnits.steps.toLong / predictedSteps(k)

    /** As [[txsPerBlock]], but costing the batch at the rate this map's own entries earn. */
    def txsPerBlockOfSize(k: Int, outstanding: EvacuationMap, params: ProtocolParams): Long = {
        val dearest =
            if outstanding.isEmpty then fittedEntryBytes
            else outstanding.evacuationMap.values.map(_.outputSize).max
        val cost = fixedSteps + stepsPerPayoutOfSize(dearest) * k
        params.maxBlockExecutionUnits.steps.toLong / math.max(1L, cost)
    }

    /** The next batch to evacuate: the first `k` entries in map order.
      *
      * Order is the map's own — ascending by evacuation key — which keeps the batch a contiguous
      * prefix. That matters for a chained run: each successive residual is again a prefix-free
      * suffix, so the sequence of batches partitions the map without any bookkeeping beyond "how
      * far have we got".
      */
    def nextBatch(outstanding: EvacuationMap, params: ProtocolParams): EvacuationMap = {
        val k = maxBatchSizeFor(outstanding, params)
        EvacuationMap(outstanding.evacuationMap.take(k))
    }
}
