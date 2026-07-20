package hydrozoa.lib.petri

import cats.implicits.*
import hydrozoa.lib.petri.net.*
import hydrozoa.lib.petri.net.components.*

/** A [[Simulator]] implementing the ISO 15909-1 enabling and firing rules (Concepts 10/12),
  * generically over a [[MarkingAlgebra]] — arcs contribute only their annotations `W`; the rules
  * live here.
  *
  * Extends [[Net.Topology.NoDanglingArcs]] as a soundness constraint: every flow element must
  * reference a valid place and transition ID. A dangling arc at fire time is a bug, not a runtime
  * condition. Because an arc's identity is its `F`-element, each place has at most one input and
  * one output arc per transition (`W` is a function on `F`), so the per-place update below is
  * exactly Concept 12.
  *
  * Firing checks, in order:
  *   1. Transition exists in the net
  *   2. [[Net.Semantics.netEnablingPredicate]] holds (the filtering-function seam, Fε)
  *   3. Enabling per input arc: `M(p) ≥ W(p,t)` ([[MarkingAlgebra.covers]], Concept 10)
  *   4. Place validity ([[Place.Semantics.validMarking]]) for every updated place
  *
  * The marking update is `M′(p) = M(p) − W(p,t) + W(t,p)` per connected place (Concept 12),
  * followed by [[aPost]] (Concept 7's `A-post` placeholder).
  *
  * Implementors must supply:
  *   - [[algebra]]: the [[MarkingAlgebra]] instantiating the rules at the net's token types
  *   - [[arcsForTransition]]: the annotated flow elements connected to a given transition
  *   - [[withUpdatedPlaces]]: how to produce a new `Self` with updated place markings
  */
trait SequentialSimulator[
    PlaceId,
    TransitionId,
    W,
    M,
    A <: Arc.Syntax.Annotated[W],
    P <: Place.Topology & Place.Syntax.Marked[P, M] & Place.Semantics[P],
    T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
    Self <: SequentialSimulator[PlaceId, TransitionId, W, M, A, P, T, Self]
] extends Simulator[PlaceId, TransitionId, A, P, T, Self],
      Net.Topology.NoDanglingArcs[PlaceId, TransitionId, P, T] {

    /** The token algebra the rules are instantiated at. */
    protected def algebra: MarkingAlgebra[W, M]

    /** All arcs connected to transition `t`, as (flow, annotation) pairs. Implementations may use
      * an internal index (e.g. a TreeMap) for efficiency.
      */
    protected def arcsForTransition(
        t: TransitionId
    ): List[(Arc.Flow[PlaceId, TransitionId], A)]

    /** Return a new `Self` with the given place markings merged in. Only the places in `updates`
      * need to change; all other places remain as-is.
      */
    protected def withUpdatedPlaces(updates: Iterable[(PlaceId, P)]): Self

    /** ISO Concept 7's `A-post` placeholder: an action applied to the net after the marking update.
      * Identity by default; ISO 15909-3 enrichments (e.g. reset arcs, 5.2.3.4) override. (`A-pre`
      * belongs to action selection — e.g. the elapse of time — and is not modeled here.)
      */
    protected def aPost(fired: Self): Self = fired

    override def fire(t: TransitionId): Either[Simulator.FiringError, Self] = {
        if !transitionIds.contains(t) then return Left(Simulator.FiringError.TransitionNotFound(t))

        if !netEnablingPredicate(t) then return Left(Simulator.FiringError.NetEnablingFailed(t))

        for
            resolved <- arcsForTransition(t).traverse { (flow, arc) =>
                getPlaceSemantics(flow.place)
                    .leftMap(_ => SequentialSimulator.DanglingArc(flow))
                    .map(place => (flow, arc, place))
            }
            updates <- firedPlaces(resolved)
        yield aPost(withUpdatedPlaces(updates))
    }

    // Concept 12, per connected place: M′(p) = M(p) − W(p,t) + W(t,p). Each place has at most one
    // arc per direction (W is a function on F), so the folds apply at most one annotation each.
    private def firedPlaces(
        resolved: List[(Arc.Flow[PlaceId, TransitionId], A, P)]
    ): Either[Simulator.FiringError, List[(PlaceId, P)]] = {
        val placeIdsInArcOrder = resolved.map((flow, _, _) => flow.place).distinct
        placeIdsInArcOrder.traverse { placeId =>
            val connected = resolved.filter((flow, _, _) => flow.place == placeId)
            val place = connected.head._3
            val (inputs, outputs) = connected.partition { (flow, _, _) =>
                flow match
                    case _: Arc.Flow.Pt[?, ?] => true
                    case _: Arc.Flow.Tp[?, ?] => false
            }
            for
                afterInputs <- inputs.foldLeftM(place.marking) { case (m, (flow, arc, _)) =>
                    Either.cond(
                      algebra.covers(m, arc.annotation),
                      algebra.minus(m, arc.annotation),
                      Simulator.FiringError.ArcNotEnabled(flow, m, arc.annotation)
                    )
                }
                afterOutputs = outputs.foldLeft(afterInputs) { case (m, (_, arc, _)) =>
                    algebra.plus(m, arc.annotation)
                }
                updated = place.mark(afterOutputs)
                _ <- updated.markingError
                    .map(Simulator.FiringError.PlaceValidityViolated(placeId, _))
                    .toLeft(())
            yield placeId -> updated
        }
    }
}

object SequentialSimulator {

    /** Flow element `flow` references a place which has no semantics entry. Specific to
      * [[SequentialSimulator]] because it arises from the [[Net.Topology.NoDanglingArcs]]
      * constraint this simulator enforces. Should not occur in a topologically valid net — validate
      * via [[Net.Topology.isValidTopology]] before simulating.
      */
    case class DanglingArc[PlaceId, TransitionId](flow: Arc.Flow[PlaceId, TransitionId])
        extends Simulator.FiringError {
        override def getMessage: String =
            s"Arc $flow references a place with no semantics entry; validate topology before simulating"
    }
}
