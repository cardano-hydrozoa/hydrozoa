package hydrozoa.lib.petri.net.components

import hydrozoa.lib.number.NonNegativeInt
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Transition {
    sealed trait Error extends Throwable

    object Error {
        // TODO: data expressions can either be syntactically invalid or have dangling
        // variable references. We're not using them now, so this is simple. It should propagate
        // to a ValidityFlag in the future.
        case object InvalidDataExpression extends Error {
            override def getMessage: String = "Invalid data expression in transition"
        }
    }

    trait Id[TransitionId] {
        val id: TransitionId
    }

    // Stub. Component-side acceptance predicates are deferred until the builder is designed.
    // See Place.Topology for rationale.
    trait Topology

    // We don't currently support data expressions. These should likely be net-wide anyways.
    trait Semantics
//    {
//        val precondition: Expression
//        val postcondition: Expression
//    }

    object Semantics

    // Instantiation data is split into per-property sub-traits. See package.scala for documentation
    // on the F-bounded polymorphism and mixin patterns used here.
    trait Syntax

    object Syntax {

        trait HasSilent[Self <: HasSilent[Self]] extends Syntax { self: Self =>

            /** Controls whether the firing of this transition appears in the event logs.
              */
            def silent: Boolean
            def withSilent(s: Boolean): Self
        }

        trait HasPriority[Self <: HasPriority[Self]] extends Syntax { self: Self =>

            /** I _believe_ this controls the relative priority of transitions when selecting among
              * all enabled transitions for auto-firing. I'm not certain of the precise semantics.
              */
            def priority: NonNegativeInt
            def withPriority(p: NonNegativeInt): Self
        }
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
    override val delay: FiniteDuration = 0.millis,
    override val height: NonNegativeInt = NonNegativeInt.unsafeApply(50),
    override val position: (Int, Int) = (0, 0),
    override val width: NonNegativeInt = NonNegativeInt.unsafeApply(20),
    override val priority: NonNegativeInt = NonNegativeInt.unsafeApply(0),
    override val silent: Boolean = false
) extends Transition.Topology,
      Transition.Semantics,
      Transition.Syntax.HasSilent[TransitionNoId],
      Transition.Syntax.HasPriority[TransitionNoId],
      Transition.Presentation {
    override def withSilent(s: Boolean): TransitionNoId = this.copy(silent = s)
    override def withPriority(p: NonNegativeInt): TransitionNoId = this.copy(priority = p)
}
