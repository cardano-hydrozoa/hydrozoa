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

    /** @param label
      *   A label for the transition. It is _not_ the same as the transition ID, and thus can have
      *   duplicates.
      * @param width
      *   The width of the transition when graphically displayed.
      * @param height
      *   The height of the transition when graphically displayed.
      * @param silent
      *   Whether the transition appears in the event log when run in YAPNE
      * @param position
      *   The position of the transition on the YAPNE canvas
      * @param priority
      *   (Not totally sure of the semantics here. I think its something like "when auto-firing,
      *   transitions with higher priority are selected before those with lower priority;
      *   transitions with the same priority are selected at random (according to a seed).)
      * @param delay
      *   A graphical delay applied to the firing of the note (to make auto-firing intelligible).
      *   (I'm 90% sure that this is _not_ enabling timed petri nets)
      * @tparam label
      *   In YAPNE, the label is a javascript string. For typed nets, we can use another, but it
      *   should be injective to string.
      */
    case class TransitionPresentation[Label](
        label: Label,
        width: NonNegativeInt = NonNegativeInt.unsafeApply(20),
        height: NonNegativeInt = NonNegativeInt.unsafeApply(50),
        silent: Boolean = false,
        position: (Int, Int),
        priority: PositiveInt,
        delay: FiniteDuration,
    )
}

case class Transition[Id](
    id: Id,
    precondition: Expression,
    postcondition: Expression
)
