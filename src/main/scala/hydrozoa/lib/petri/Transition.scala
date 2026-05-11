package hydrozoa.lib.petri

import hydrozoa.lib.number.{NonNegativeInt, PositiveInt}
import scala.concurrent.duration.FiniteDuration

// TODO: YAPNE models "data petri nets" or "DPN". It allows simple typed variables (integer, doubles, and booleans)
// to be checked in transition pre-conditions ("guards") and updated as post-conditions.
// These are restricted to simple expressions, with beta support for arbitrary python.
// One reason I suspect that the language may be so restricted is to make SMT solving easier, but I don't know
// for certain.
type Expression = Unit

object Transition {
    case class TransitionPresentation[Label](
        label: Label,
        width: NonNegativeInt = NonNegativeInt.unsafeApply(20),
        height: NonNegativeInt = NonNegativeInt.unsafeApply(50),
        silent: Boolean = false,
        position: (Int, Int)
    )

    case class TransitionSimulation(
        isEnabled: Boolean,
        priority: PositiveInt,
        delay: FiniteDuration,
    )
}

case class Transition[Id](
    id: Id,
    precondition: Expression,
    postcondition: Expression
)
