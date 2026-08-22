package hydrozoa.rulebased.evacuator

import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.joint.EvacuationMap.evacuationMapDecoder
import io.circe.parser.decode
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import scalus.cardano.ledger.ProtocolParams

/** Plans the evacuation of a real outstanding set, taken from the head that entered the rule-based
  * regime on 2026-08-22.
  *
  * The fixture is what that head actually still owed: its full evacuation map, minus every key its
  * own bot had already paid out, as replayed from the `Evacuate` redeemers on chain. Using it
  * rather than a generated map is what makes the batch sizes here mean something — a synthetic map
  * of uniform entries would confirm the arithmetic while saying nothing about real payout shapes.
  */
class EvacuationPlanTest extends AnyFunSuite {

    private val env =
        MultiNodeConfig.generateWithCoil().pureApply(Gen.Parameters.default, Seed(0L))

    private val params: ProtocolParams = env.headConfig.cardanoProtocolParams

    private val outstanding: EvacuationMap = {
        given hydrozoa.config.head.network.CardanoNetwork.Section = env.headConfig
        val json = Source
            .fromInputStream(
              getClass.getResourceAsStream("/hydrozoa/evacuator/outstanding-demo-stand.json")
            )
            .mkString
        decode[EvacuationMap](json).fold(e => fail(s"fixture did not decode: $e"), identity)
    }

    test("the fixture is the outstanding set that head still owed") {
        assert(outstanding.size == 2330)
    }

    test("the plan's batches partition the outstanding set exactly") {
        val steps = EvacuationPlan.plan(outstanding, params).toList
        val planned = steps.flatMap(_.batch.evacuationMap.keys)

        // No key evacuated twice: a duplicate would be a membership proof against a set that no
        // longer contains it, so the second transaction fails on chain.
        val _ = assert(planned.distinct.size == planned.size, "a key appears in two batches")
        // No key left behind: the plan must drain the treasury, not almost drain it.
        assert(planned.toSet == outstanding.evacuationMap.keySet)
    }

    test("each step's remainder is what the next step plans against") {
        val steps = EvacuationPlan.plan(outstanding, params).toList
        steps.sliding(2).foreach {
            case List(a, b) =>
                val _ = assert(
                  a.remainingAfter.evacuationMap.keySet == b.batch.evacuationMap.keySet ++
                      b.remainingAfter.evacuationMap.keySet,
                  s"step ${a.index}'s remainder does not match step ${b.index}'s inputs"
                )
            case _ => ()
        }
        assert(steps.last.remainingAfter.isEmpty, "the last step leaves the treasury undrained")
    }

    test("every batch is within the ex-unit budget, and all but the last are full") {
        val steps = EvacuationPlan.plan(outstanding, params).toList
        val maxSteps = params.maxTxExecutionUnits.steps.toLong

        steps.foreach { s =>
            val _ = assert(
              BatchPlanner.predictedSteps(s.batchSize) <= maxSteps,
              s"step ${s.index} of ${s.batchSize} exceeds the per-tx limit"
            )
        }
        // A short batch anywhere but the tail means the planner is leaving room unused on a
        // transaction it has already paid the fixed cost for.
        val k = BatchPlanner.maxBatchSize(outstanding.size, params)
        assert(steps.init.forall(_.batchSize == k), "a non-final batch is under-filled")
    }

    test("planning the whole evacuation agrees with the transaction count") {
        val steps = EvacuationPlan.plan(outstanding, params).toList
        assert(steps.size == EvacuationPlan.txCount(outstanding, params))
    }

    test("the lookahead bounds the plan without changing what it plans") {
        val full = EvacuationPlan.plan(outstanding, params).toList
        val bounded = EvacuationPlan.plan(outstanding, params, lookahead = Some(5)).toList
        val _ = assert(bounded.size == 5)
        assert(bounded == full.take(5), "bounding the lookahead changed the plan")
    }

    test("the whole set drains in far fewer transactions than the incumbent's worst case") {
        // The running bot halves on rejection, so a batch that would fit at k lands at k/2 or less.
        // This is the margin the planner buys, and it is the reason the fast bot exists.
        val ours = EvacuationPlan.txCount(outstanding, params)
        val halved = math.ceil(outstanding.size.toDouble / 8).toInt
        assert(
          ours < halved,
          s"planned $ours txs vs $halved at the incumbent's observed batch of 8"
        )
    }
}
