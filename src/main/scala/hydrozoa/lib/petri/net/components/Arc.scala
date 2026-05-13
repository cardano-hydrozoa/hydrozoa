package hydrozoa.lib.petri.net.components

import cats.Endo
import hydrozoa.lib.number.NonNegativeInt
import hydrozoa.lib.petri.net.components.Place.PlaceCapacity.{Bounded, Unlimited}
import scala.collection.immutable.Queue

/** This encodes the topological, configuration, simulation, and presentation data for arcs. Right
  * now, I'm going to keep it simple. But petri nets come in many, many flavors. Eventually, we will
  * need to parameterize this on different types of arcs, multiarcs, data/logic-arcs, and possibly
  * even encode firing logic for things like timed arcs.
  */
object Arc {
    // Stub error trait
    sealed trait Error extends Throwable

    trait Id[ArcId] {
        val id: ArcId
    }

    trait Topology[PlaceId, TransitionId] {
        def arcPlaceId: PlaceId
        def arcTransitionId: TransitionId
    }

    trait Semantics {
        val semantics: ArcSemantics
    }

    // There does not appear to be any simulation-specific data right now. In a more generic setup, there
    // might be logic or data needed here.
    trait Simulation

    trait Presentation {
        val label: String
        // The arc always starts from the source and ends at the target. These are additional points that go along
        // the way. I don't believe this is integrated into the YAPNE GUI yet.
        val points: Queue[(Int, Int)]
    }

    /** Canonical Arcs.
      */
    case class ArcNoId[PlaceId, TransitionId](
        override val arcPlaceId: PlaceId,
        override val arcTransitionId: TransitionId,
        override val semantics: ArcSemantics,
        override val label: String = "",
        override val points: Queue[(Int, Int)] = Queue.empty
    ) extends Topology[PlaceId, TransitionId],
          Semantics,
          Simulation,
          Presentation,
          ArcSemantics {

        override def necessaryEnablingPredicate(
            p: Place.Semantics & Place.Simulation
        ): Boolean = semantics.necessaryEnablingPredicate(p)

        override def firingEndomorphism: Place.Simulation => Place.Simulation =
            semantics.firingEndomorphism
    }

}

trait Weighted {
    val weight: NonNegativeInt
}

// YAPNE supports 4 "types" of arcs:
//  - Regular arcs (which add/remove tokens from places)
//  - Inhibitor arcs, which only enable if the connect place is empty
//  - Reset arcs, which remove all tokens from the connect place
//  - Read, which only allow for enabling the connected place has the requisite number of tokens.
//  I believe this is problematic (see https://github.com/chimenkamp/YAPNE-Yet-Another-Petri-Net-Editor/issues/13),
//  so we're going to refine it as follows

// TODO: Making this sealed for now, in the future we might want to allow for gluing nets with different sets of
//  arc types.

// TODO: right now these are not as typesafe as I would like . Ideally, we'd set it up so that
//  necessaryEnablingPredicates return an affine type or something, and restrict the firing endomorphism to only fire
//  when in a transition when all of the associated predicates are true.
//  For now, the methods are just encoding the intended semantics.
sealed trait ArcSemantics {

    /** The enabledness semantic for this type of arc. The CURRENT choice of enabledness semantic is
      * that all "enables" conditions must be true _pre-firing_.
      * @param p
      * @return
      */
    def necessaryEnablingPredicate(p: Place.Semantics & Place.Simulation): Boolean

    /** How this arc updates the place when an enabled transition fires. Note that this is currently
      * _unsafe_ -- it does not check enabledness.
      * @return
      */
    def firingEndomorphism: Place.Simulation => Place.Simulation
}

object ArcSemantics {
    case class PT(weight: NonNegativeInt) extends Weighted, ArcSemantics {
        // Observation: this doesn't depend on Place.Configuration
        override def necessaryEnablingPredicate(
            p: Place.Semantics & Place.Simulation
        ): Boolean = p.tokens >= weight

        override def firingEndomorphism: Endo[Place.Simulation] = p =>
            new Place.Simulation {
                override val tokens: NonNegativeInt = NonNegativeInt.unsafeApply(p.tokens - weight)
            }
    }

    case class TP(weight: NonNegativeInt) extends Weighted, ArcSemantics {
        // Observation: this DOES depend on both configuration and simulation
        override def necessaryEnablingPredicate(
            p: Place.Semantics & Place.Simulation
        ): Boolean =
            p.capacity match {
                case Unlimited  => true
                case Bounded(b) => p.tokens + weight <= b
            }

        override def firingEndomorphism: Place.Simulation => Place.Simulation = p =>
            new Place.Simulation {
                override val tokens: NonNegativeInt = NonNegativeInt.unsafeApply(p.tokens + weight)
            }
    }

    case object Inhibitor extends ArcSemantics {
        override def necessaryEnablingPredicate(
            p: Place.Simulation & Place.Semantics
        ): Boolean =
            p.tokens.convert == 0

        override def firingEndomorphism: Place.Simulation => Place.Simulation = identity
    }

    case object Reset extends ArcSemantics {
        override def necessaryEnablingPredicate(
            p: Place.Semantics & Place.Simulation
        ): Boolean = true

        override def firingEndomorphism: Place.Simulation => Place.Simulation = _ =>
            new Place.Simulation {
                override val tokens: NonNegativeInt = NonNegativeInt.unsafeApply(0)
            }
    }

    case class Read(weight: NonNegativeInt) extends Weighted, ArcSemantics {
        override def necessaryEnablingPredicate(
            p: Place.Semantics & Place.Simulation
        ): Boolean =
            p.tokens >= weight

        override def firingEndomorphism: Place.Simulation => Place.Simulation = identity
    }
}
