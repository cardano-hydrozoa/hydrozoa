package hydrozoa.lib.petri.net.components

import hydrozoa.lib.number.NonNegativeInt
import hydrozoa.lib.petri.net.components.Expression
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Transition {
    sealed trait Error extends Throwable

    object Error {
        // TODO: data expressions can either be syntactically invalid or have dangling
        // variable references. We're not using them now, so this is simple. It should propagate
        // to a ValidityFlag in the future.
        case object InvalidDataExpression extends Error
    }

    trait Id[TransitionId] {
        val id: TransitionId
    }

    // Stub: there are not currently any topological properties. In the future, there may be typed "ports",
    // connection limits, etc.
    trait Topology

    trait Semantics {
        val precondition: Expression
        val postcondition: Expression
    }

    trait Simulation {

        /** Controls whether the firing of this transition appears in the event logs
          */
        def silent: Boolean

        /** I _believe_ this controls the relative priority of transitions when selecting among all
          * enabled transitions for auto-firing. I'm not certain of the precise semantics.
          * @return
          */
        def priority: NonNegativeInt
    }

    /** @param label
      *   A label for the transition. It is _not_ the same as the transition ID, and thus can have
      *   duplicates.
      * @param width
      *   The width of the transition when graphically displayed.
      * @param height
      *   The height of the transition when graphically displayed.
      * @param position
      *   The position of the transition on the YAPNE canvas
      * @param delay
      *   A graphical delay applied to the firing of the note (to make auto-firing intelligible).
      *   (I'm 90% sure that this is _not_ enabling timed petri nets)
      */
    trait Presentation {
        val label: String
        val width: NonNegativeInt
        val height: NonNegativeInt
        val position: (Int, Int)
        val delay: FiniteDuration
    }
}

/** A canonical transition case class
  */
case class TransitionNoId(
    override val label: String,
    override val postcondition: Expression = (),
    override val precondition: Expression = (),
    override val delay: scala.concurrent.duration.FiniteDuration = 0.millis,
    override val height: hydrozoa.lib.number.NonNegativeInt = NonNegativeInt.unsafeApply(50),
    override val position: (Int, Int) = (0, 0),
    override val width: hydrozoa.lib.number.NonNegativeInt = NonNegativeInt.unsafeApply(20),
    override val priority: hydrozoa.lib.number.NonNegativeInt = NonNegativeInt.unsafeApply(0),
    override val silent: Boolean = false
) extends Transition.Topology,
      Transition.Semantics,
      Transition.Simulation,
      Transition.Presentation
