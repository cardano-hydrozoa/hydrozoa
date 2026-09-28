package hydrozoa.rulebased.ledger.l1.tx

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.rules.{Context, ExUnitsTooBigValidator, State, UtxoEnv}
import scalus.cardano.ledger.{ExUnits, KeepRaw, Redeemer, RedeemerTag, Redeemers, Transaction, TransactionWitnessSet}
import scalus.uplc.builtin.Data

/** Pins that scalus rejects a tx over the per-tx CPU-steps budget but under the memory one, which
  * [[EvacuationTx]]'s build loop relies on to halve an over-budget evacuation.
  *
  * The loop halves the batch on `ExUnitsExceedMaxException`, raised by
  * `scalus.cardano.ledger.rules.ExUnitsTooBigValidator` (one of `EnrichedTx.Validators
  * .nonSigningValidators`). Up to scalus 1.0.0 that validator decided "over the per-tx budget?"
  * with the lexicographic, memory-first `Ordering[ExUnits]`, so it passed such a tx, which the real
  * ledger rejects (`ExUnitsTooBigUTxO`). This surfaced on a Yaci devnet as an over-budget
  * rule-based evacuation tx that the build never halved, and `EvacuationTx` carried its own
  * component-wise re-check. From 1.1.0 the validator uses the component-wise `ExUnits.exceeds`,
  * which is what this test pins.
  */
class ExUnitsStepsOnlyOverrunTest extends AnyFunSuite {

    // A realistic per-tx budget (16.5M memory, 10B steps).
    private val max = ExUnits(memory = 16_500_000L, steps = 10_000_000_000L)

    // Under the memory cap, over the CPU-steps cap: genuinely over budget (observed on Yaci).
    private val overOnStepsOnly = ExUnits(memory = 6_966_471L, steps = 11_606_514_781L)

    test("steps alone exceed the max — this tx IS over budget") {
        assert(overOnStepsOnly.memory < max.memory && overOnStepsOnly.steps > max.steps)
    }

    test("`exceeds`, which scalus's ExUnitsTooBigValidator uses, reports it as over budget") {
        assert(overOnStepsOnly.exceeds(max))
    }

    test("ExUnitsTooBigValidator rejects a tx that is over budget on steps alone") {
        val env = UtxoEnv.default
        val context = Context(env = env.copy(params = env.params.copy(maxTxExecutionUnits = max)))
        val tx = Transaction.empty.withWitness(
          TransactionWitnessSet(redeemers =
              Some(KeepRaw(Redeemers(Redeemer(RedeemerTag.Spend, 0, Data.unit, overOnStepsOnly))))
          )
        )
        assert(ExUnitsTooBigValidator.validate(context, State(), tx).isLeft)
    }
}
