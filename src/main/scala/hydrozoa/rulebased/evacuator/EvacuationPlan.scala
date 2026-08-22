package hydrozoa.rulebased.evacuator

import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import scalus.cardano.ledger.ProtocolParams

/** The whole remaining evacuation, planned in one pass instead of one batch per poll.
  *
  * Every `Evacuate` spends the treasury and produces the next one, so the sequence of batches is
  * fully determined by the outstanding set and the ex-unit limit — no chain query tells us anything
  * we do not already know. That is what makes the plan computable offline, and what lets the bot
  * build and submit a long chain back-to-back rather than waiting a poll period between each.
  *
  * Planning ahead is also what closes the competitive window. While an unbroken chain spending the
  * current treasury sits in the network's mempools, there is no unclaimed treasury utxo for a
  * competitor to take; every pause between batches re-opens that window for a block or two.
  */
object EvacuationPlan {

    /** One step of the plan: the batch to evacuate, and what remains after it. */
    final case class Step(
        index: Int,
        batch: EvacuationMap,
        remainingAfter: EvacuationMap
    ) {
        def batchSize: Int = batch.size
    }

    /** Split the outstanding set into the batches that will evacuate it.
      *
      * Each step takes a maximal prefix, so the batches partition the map in key order and the
      * residual after every step is exactly the suffix — which is what the membership proof for the
      * next step is computed against.
      *
      * @param lookahead
      *   how many steps to plan. Bounding it bounds the work thrown away if we lose the treasury
      *   mid-chain; it is not a throughput limit, since building runs far ahead of what the network
      *   drains. `None` plans the entire evacuation.
      */
    def plan(
        outstanding: EvacuationMap,
        params: ProtocolParams,
        lookahead: Option[Int] = None
    ): LazyList[Step] = {
        def go(remaining: EvacuationMap, index: Int): LazyList[Step] =
            if remaining.isEmpty then LazyList.empty
            else {
                val batch = BatchPlanner.nextBatch(remaining, params)
                val after = EvacuationMap(remaining.evacuationMap.drop(batch.size))
                Step(index, batch, after) #:: go(after, index + 1)
            }

        val all = go(outstanding, 0)
        lookahead.fold(all)(all.take)
    }

    /** How many transactions the outstanding set needs, without materialising the batches. */
    def txCount(outstanding: EvacuationMap, params: ProtocolParams): Int = {
        val k = BatchPlanner.maxBatchSizeFor(outstanding, params)
        math.ceil(outstanding.size.toDouble / k).toInt
    }

    /** Lower bound on how long the evacuation can take, in blocks.
      *
      * Set by the chain, not by us: the block ex-unit limit admits only a couple of these
      * transactions per block, so this bounds any evacuator equally — ours and the incumbent's.
      * Worth computing up front so a run that hits it is recognised as finished rather than slow.
      */
    def minimumBlocks(outstanding: EvacuationMap, params: ProtocolParams): Long = {
        val k = BatchPlanner.maxBatchSizeFor(outstanding, params)
        val perBlock = math.max(1L, BatchPlanner.txsPerBlock(k, params))
        math.ceil(txCount(outstanding, params).toDouble / perBlock).toLong
    }
}
