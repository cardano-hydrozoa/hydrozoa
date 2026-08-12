package hydrozoa.rulebased.ledger.l1.tx

import org.scalatest.funsuite.AnyFunSuite
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.ExUnits

/** Pins a KNOWN scalus bug that [[EvacuationTx]]'s build loop works around with a component-wise
  * ex-unit check.
  *
  * scalus's `Ordering[ExUnits]` (`scalus/cardano/ledger/Types.scala`) is lexicographic,
  * memory-FIRST:
  * {{{
  *   given Ordering[ExUnits] = (x, y) =>
  *       if x.memory != y.memory then x.memory.compareTo(y.memory) else x.steps.compareTo(y.steps)
  * }}}
  * So `actual > max` — the comparison `scalus.cardano.ledger.rules.ExUnitsTooBigValidator` uses to
  * decide "over the per-tx budget?" — is governed by `memory` alone whenever the memories differ. A
  * tx whose CPU `steps` exceed the max while its `memory` is under it compares as NOT over budget,
  * so the validator passes it — but the real ledger checks each component independently and rejects
  * it (`ExUnitsTooBigUTxO`). This surfaced on a Yaci devnet as an over-budget rule-based evacuation
  * tx that the build never halved.
  *
  * Reported upstream (see the PR description). When scalus fixes the ordering/validator the `BUG`
  * assertion below flips; delete this test and the [[EvacuationTx]] workaround at that point.
  */
class ExUnitsOrderingScalusBugTest extends AnyFunSuite {

    // A mainnet-style per-tx budget.
    private val max = ExUnits(memory = 16_500_000L, steps = 10_000_000_000L)

    // Under the memory cap, over the CPU-steps cap: genuinely over budget (observed on Yaci).
    private val overOnStepsOnly = ExUnits(memory = 6_966_471L, steps = 11_606_514_781L)

    test("steps alone exceed the max — this tx IS over budget") {
        assert(overOnStepsOnly.steps > max.steps)
    }

    test("BUG: `actual > max` reports it as NOT over budget (memory-first Ordering[ExUnits])") {
        // Correct answer is `true`; scalus returns `false`. Pinned as `!(_ > _)` so the suite stays
        // green until upstream fixes it, at which point this assertion flips.
        assert(!(overOnStepsOnly > max))
    }

    test("the component-wise check EvacuationTx.Build uses instead is correct") {
        assert(overOnStepsOnly.memory > max.memory || overOnStepsOnly.steps > max.steps)
    }
}
