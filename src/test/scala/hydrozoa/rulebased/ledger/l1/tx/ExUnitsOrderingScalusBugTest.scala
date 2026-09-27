package hydrozoa.rulebased.ledger.l1.tx

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits

/** Pins how scalus judges a tx that is over the per-tx CPU budget but under the memory one, which
  * [[EvacuationTx]]'s build loop guards with a component-wise ex-unit check.
  *
  * scalus's `Ordering[ExUnits]` (`scalus/cardano/ledger/Types.scala`) is lexicographic,
  * memory-first, so `actual > max` is governed by `memory` alone whenever the memories differ: it
  * calls such a tx within budget, although the real ledger rejects it (`ExUnitsTooBigUTxO`). The
  * ordering is deprecated, and `scalus.cardano.ledger.rules.ExUnitsTooBigValidator` judges "over
  * the per-tx budget?" with the component-wise `ExUnits.exceeds`, which this test pins.
  *
  * The name is from scalus 1.0.0, whose validator compared budgets with the ordering.
  */
class ExUnitsOrderingScalusBugTest extends AnyFunSuite {

    // A mainnet-style per-tx budget.
    private val max = ExUnits(memory = 16_500_000L, steps = 10_000_000_000L)

    // Under the memory cap, over the CPU-steps cap: genuinely over budget (observed on Yaci).
    private val overOnStepsOnly = ExUnits(memory = 6_966_471L, steps = 11_606_514_781L)

    test("steps alone exceed the max — this tx IS over budget") {
        assert(overOnStepsOnly.steps > max.steps)
    }

    test("`exceeds`, which scalus's ExUnitsTooBigValidator uses, reports it as over budget") {
        assert(overOnStepsOnly.exceeds(max))
    }

    test("the component-wise check EvacuationTx.Build uses instead is correct") {
        assert(overOnStepsOnly.memory > max.memory || overOnStepsOnly.steps > max.steps)
    }
}
