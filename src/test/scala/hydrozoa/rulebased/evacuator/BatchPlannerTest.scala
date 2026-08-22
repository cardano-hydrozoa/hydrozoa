package hydrozoa.rulebased.evacuator

import hydrozoa.config.node.MultiNodeConfig
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnits, ProtocolParams}

/** Checks the batch planner against the ex-unit costs of `Evacuate` transactions that actually
  * landed on preview, and against real protocol parameters.
  *
  * The point of the planner is to pick the batch size without paying for a rejected build, so the
  * property that matters is two-sided: the batch it picks must fit, and one more payout must not. A
  * planner that only ever undershoots would pass a "does it fit" check while evacuating at half the
  * rate.
  */
class BatchPlannerTest extends AnyFunSuite {

    /** Real parameters from the house fixture, rather than hand-written numbers — so a protocol
      * change shows up here as a changed batch size instead of being masked by a stale literal.
      */
    private val params: ProtocolParams =
        MultiNodeConfig
            .generateWithCoil()
            .pureApply(Gen.Parameters.default, Seed(0L))
            .headConfig
            .cardanoProtocolParams

    private val maxTxSteps: Long = params.maxTxExecutionUnits.steps.toLong

    private def withTxSteps(steps: Long): ProtocolParams =
        params.copy(maxTxExecutionUnits =
            ExUnits(memory = params.maxTxExecutionUnits.memory, steps = steps)
        )

    /** The three landed transactions the cost model was fitted against: batch size, and the CPU
      * steps the chain actually charged for it.
      */
    private val landed: List[(Int, Long)] =
        List(8 -> 6_231_015_167L, 13 -> 9_159_697_364L, 14 -> 9_745_193_804L)

    test("the fitted model reproduces the ex-unit cost of transactions that landed") {
        landed.foreach { case (k, actualSteps) =>
            val predicted = BatchPlanner.predictedStepsPlainShape(k)
            val relativeError = math.abs(predicted - actualSteps).toDouble / actualSteps
            val _ = assert(
              relativeError < 0.005,
              s"k=$k predicted $predicted vs actual $actualSteps (${relativeError * 100}% off)"
            )
        }
    }

    test("the chosen batch fits, and one more payout would not") {
        val k = BatchPlanner.maxBatchSize(available = 1000, params = params)
        val _ = assert(BatchPlanner.predictedSteps(k) <= maxTxSteps, s"k=$k does not fit")
        val _ = assert(
          BatchPlanner.predictedSteps(k + 1) > maxTxSteps,
          s"k=$k leaves room for another payout — the planner is undershooting"
        )
    }

    test("a batch never exceeds what is available to evacuate") {
        val _ = assert(BatchPlanner.maxBatchSize(available = 3, params = params) == 3)
        assert(BatchPlanner.maxBatchSize(available = 1, params = params) == 1)
    }

    test("a batch never exceeds the deployed ladder's reach") {
        // A rung proves at most `2^(rungCount-1)` evacuations; asking for more would select a rung
        // that does not exist. Raise the ex-unit limit so only the ladder can bind.
        val roomy = withTxSteps(Long.MaxValue / 4)
        val _ = assert(
          BatchPlanner.maxBatchSize(available = 10_000, params = roomy) == BatchPlanner.ladderMax
        )
        assert(BatchPlanner.ladderMax == 64)
    }

    test("a limit too small for even one payout still yields a batch of one") {
        // Better to build a tx the node rejects than to return an empty batch and spin: the
        // rejection is visible, a silent no-op batch is not.
        val tiny = withTxSteps(BatchPlanner.fixedSteps)
        assert(BatchPlanner.maxBatchSize(available = 100, params = tiny) == 1)
    }
}
