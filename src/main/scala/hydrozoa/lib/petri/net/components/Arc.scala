package hydrozoa.lib.petri.net.components

import hydrozoa.lib.petri.Positive
import scala.collection.immutable.Queue

/** Arcs per ISO 15909-1 (Concepts 1/8): the flow relation `F ⊆ (P×T) ∪ (T×P)` is a set of
  * [[Arc.Flow]] elements, and the annotation `W : F → AN` is the net's arc map keyed by them — an
  * arc's identity *is* its `F`-element, so `W` is a function by construction and duplicate arcs are
  * unrepresentable. An arc value carries only its annotation ([[Arc.Syntax]]); enabling and firing
  * are net-level rules implemented by the simulator.
  */
object Arc {

    /** An element of the flow relation `F ⊆ (P×T) ∪ (T×P)`: a directed edge between a place and a
      * transition. [[Flow.Pt]] is a precondition arc (place → transition), [[Flow.Tp]] a
      * postcondition arc (transition → place).
      */
    sealed trait Flow[PlaceId, TransitionId] {
        def place: PlaceId
        def transition: TransitionId
    }

    object Flow {

        /** `(p,t) ∈ F` — an input arc of `t`; `p` is a precondition of `t`. */
        final case class Pt[PlaceId, TransitionId](place: PlaceId, transition: TransitionId)
            extends Flow[PlaceId, TransitionId]

        /** `(t,p) ∈ F` — an output arc of `t`; `p` is a postcondition of `t`. */
        final case class Tp[PlaceId, TransitionId](transition: TransitionId, place: PlaceId)
            extends Flow[PlaceId, TransitionId]

        /** Orders all `Pt` elements before all `Tp` elements, then by components. Enables
          * `TreeMap[Flow[…], _]`-backed annotation maps with deterministic iteration.
          */
        given ordering[PlaceId, TransitionId](using
            placeOrdering: Ordering[PlaceId],
            transitionOrdering: Ordering[TransitionId]
        ): Ordering[Flow[PlaceId, TransitionId]] with {
            def compare(x: Flow[PlaceId, TransitionId], y: Flow[PlaceId, TransitionId]): Int =
                (x, y) match {
                    case (Pt(p1, t1), Pt(p2, t2)) =>
                        val c = placeOrdering.compare(p1, p2)
                        if c != 0 then c else transitionOrdering.compare(t1, t2)
                    case (Tp(t1, p1), Tp(t2, p2)) =>
                        val c = transitionOrdering.compare(t1, t2)
                        if c != 0 then c else placeOrdering.compare(p1, p2)
                    case (_: Pt[?, ?], _: Tp[?, ?]) => -1
                    case (_: Tp[?, ?], _: Pt[?, ?]) => 1
                }
        }
    }

    /** The arc annotation ("inscription"): ISO's `W(f)`, the value the enabling and firing rules
      * consume for a flow element `f`.
      */
    trait Syntax {
        type Annotation
        def annotation: Annotation
    }

    object Syntax {

        /** Refinement alias pinning the annotation type — used as the arc bound of simulators. */
        type Annotated[W] = Syntax { type Annotation = W }

        /** The P/T annotation (Concept 8, `W : F → ℕ⁺`): the arc weight. */
        trait Weighted extends Syntax {
            final type Annotation = Positive
            val weight: Positive
            final def annotation: Positive = weight
        }
    }

    trait Presentation {
        val label: String
        // The arc always starts from the source and ends at the target. These are additional points
        // that go along the way.
        val points: Queue[(Int, Int)]
    }
}
