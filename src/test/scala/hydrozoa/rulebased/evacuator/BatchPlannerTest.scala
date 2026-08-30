package hydrozoa.rulebased.evacuator

import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Coin, ExUnits, ProtocolParams}

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

    private val env2 =
        MultiNodeConfig.generateWithCoil().pureApply(Gen.Parameters.default, Seed(0L))
    private given hydrozoa.config.head.network.CardanoNetwork.Section = env2.headConfig
    private val payTo: scalus.cardano.address.ShelleyPaymentPart =
        scalus.cardano.address.ShelleyPaymentPart.Key(
          env2.nodePrivateConfigs.head._2.ownWallet.exportVerificationKey.addrKeyHash
        )
    private val network: scalus.cardano.address.Network = env2.headConfig.network

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

    test("a map of plain entries earns the full batch; fat entries are charged for") {
        // The two shapes measured. A map we hold is a fact, so sizing against its own entries
        // recovers the payout that a blanket worst-case allowance gives away — while a map of fat
        // entries is still charged honestly rather than overshooting into a rejected build.
        val plain = SyntheticMap(100, payTo, network, Coin.ada(2))
            .fold(v => fail(s"map did not build: $v"), identity)

        val plainK = BatchPlanner.maxBatchSizeFor(plain, params)
        val blindK = BatchPlanner.maxBatchSize(plain.size, params)

        val _ = assert(plainK == 14, s"a plain-entry map should fit 14 payouts, got $plainK")
        val _ = assert(
          blindK == 13,
          s"the unmeasured path should stay conservative at 13, got $blindK"
        )
        // And the measured batch must genuinely fit at its own entries' rate.
        val dearest = plain.evacuationMap.values.map(_.outputSize).max
        val cost = BatchPlanner.fixedSteps + BatchPlanner.stepsPerPayoutOfSize(dearest) * plainK
        assert(cost <= maxTxSteps, s"a batch of $plainK costs $cost against $maxTxSteps")
    }

    test("cost per payout grows with entry size, and never dips below the fitted rate") {
        val fitted = BatchPlanner.stepsPerPayoutOfSize(BatchPlanner.fittedEntryBytes)
        val _ = assert(fitted == BatchPlanner.stepsPerPayout)
        // Below the measured size the line is not extrapolated downward — guessing low is the
        // direction that costs a rejected build.
        val _ = assert(BatchPlanner.stepsPerPayoutOfSize(10) == BatchPlanner.stepsPerPayout)
        // The ~775-byte inline-datum fixture measured 611.5M per payout; the size model should
        // land near it rather than merely above it.
        val fat = BatchPlanner.stepsPerPayoutOfSize(775)
        assert(
          math.abs(fat - 611_451_363L).toDouble / 611_451_363L < 0.02,
          s"size model gives $fat for a 775-byte entry, measured 611,451,363"
        )
    }

    test("a limit too small for even one payout still yields a batch of one") {
        // Better to build a tx the node rejects than to return an empty batch and spin: the
        // rejection is visible, a silent no-op batch is not.
        val tiny = withTxSteps(BatchPlanner.fixedSteps)
        assert(BatchPlanner.maxBatchSize(available = 100, params = tiny) == 1)
    }
}
