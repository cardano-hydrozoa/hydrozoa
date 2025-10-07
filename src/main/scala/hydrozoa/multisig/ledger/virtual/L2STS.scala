package hydrozoa.multisig.ledger.virtual

/** This module defines the "L2 STS". It is almost a direct clone of scalus's upstream "STS" (state
  * transition system), which is in turn modeled after the `IntersectMBO/cardano-ledger` haskell
  * repository.
  *
  * For a more formal treatment, see
  * https://github.com/intersectmbo/cardano-ledger/releases/latest/download/small-step-semantics.pdf.
  * The mapping from this document to our types is:
  *   - "States" = given by the `State` associated type, in our case the
  *     `scalus.cardano.ledger.rules.State` type
  *   - "Transitions" = implementations of the `transit` method on instances of STSL2.Mutator.
  *   - "Signals" = given by the `Event` associated type, in our case `L2Event` (as defined in
  *     `Event.scala`)
  *   - "Rules" = roughly, a set of calls to implementations of the `validate` method on instances
  *     of STSL2.Validator (the antecedents), followed by a calls to `transit` functions (the
  *     consequent).
  *   - "Environment" = the `Context` associated type, in our case the
  *     `scalus.cardano.ledger.rules.Context` type
  *
  * The overall principle is simple: `validate` checks a `(context, state, event)` tuple for
  * validity, `transit` takes `(context, state, event)` to a new `state`.
  *
  * Where possible, we use the upstream types for representing our own STS. This means that our
  * State, Context, and Transition types are "too big" -- for instance, our State type contains
  * information about the UTxO Map, which the L2 Ledger indeed makes use of, but ALSO contains
  * information about Certificate state, which the L2 Ledger does not support. We do this because we
  * want to re-use L1 ledger rules directly where possible without having to convert possibly large
  * data structures (such as the entire utxo map) each time; this is a performance vs type-safety
  * trade-off that we felt was worth it.
  *
  * The validation rules for our STSL2 that are native to hydrozoa (i.e., that do not apply to L1)
  * can be found in `L2ConformanceValidator.scala`.
  */

import hydrozoa.*
import scalus.cardano.ledger.*

////////////////////////////////////////
// Layer 2 state transition system

sealed trait STSL2 {
    final type Context = scalus.cardano.ledger.rules.Context
    final type State = scalus.cardano.ledger.rules.State
    // An L2 Event is a Transaction (re-using the L1 transaction type) that can
    // also absorb deposits or release withdrawals.
    final type Event = L2Event
    final type Error = String | TransactionException
    final type Result = Either[Error, Value]
    type Value

    def apply(context: Context, state: State, event: Event): Result

    protected final def failure(error: Error): Result = Left(error)

}

object STSL2 {
    trait Validator extends STSL2 {
        override final type Value = Unit
        protected final val success: Result = Right(())

        override final def apply(context: Context, state: State, event: Event): Result =
            validate(context, state, event)

        def validate(context: Context, state: State, event: Event): Result
    }

    trait Mutator extends STSL2 {
        override final type Value = State

        override final def apply(context: Context, state: State, event: Event): Result =
            transit(context, state, event)

        def transit(context: Context, state: State, event: Event): Result

        protected final def success(state: State): Result = Right(state)
    }
}
