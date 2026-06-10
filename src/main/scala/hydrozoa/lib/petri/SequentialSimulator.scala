package hydrozoa.lib.petri

import cats.implicits.*
import hydrozoa.lib.petri.net.*
import hydrozoa.lib.petri.net.components.*

/** A [[Simulator]] mixin that fires transitions sequentially: one arc at a time, in the order
  * returned by [[arcsForTransition]].
  *
  * Extends [[Net.Topology.NoDanglingArcs]] and [[Net.Topology.SingleArc]] as soundness constraints:
  *   - [[Net.Topology.NoDanglingArcs]]: every arc must reference a valid place and transition ID. A
  *     dangling arc at fire time is a bug, not a runtime condition — [[fire]] will throw rather
  *     than silently skip it.
  *   - [[Net.Topology.SingleArc]]: at most one arc per (place, transition) pair. Firing
  *     endomorphisms do not commute in general, so multiple arcs on the same pair would make the
  *     firing order non-deterministic. This constraint makes firing order-independent.
  *
  * Firing enforces all enabling conditions:
  *   1. Transition exists in the net
  *   2. [[Net.Semantics.netEnablingPredicate]] holds (net-level conditions)
  *   3. [[Arc.Semantics.enabled]] holds for every arc connected to `t` (E2)
  *   4. [[Arc.Semantics.fire]] succeeds for every arc connected to `t`
  *   5. [[Place.Semantics.validMarking]] holds for every updated place (E1)
  *
  * All arc/place pairs are snapshotted before any firing endo is applied, so every endo is computed
  * against the pre-fire state.
  *
  * Implementors must supply:
  *   - [[arcsForTransition]]: the arcs connected to a given transition
  *   - [[withUpdatedPlaces]]: how to produce a new `Self` with updated place markings
  */
trait SequentialSimulator[
    ArcId: Ordering,
    PlaceId,
    TransitionId,
    A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
    P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
    T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
    Self <: SequentialSimulator[ArcId, PlaceId, TransitionId, A, P, T, Self]
] extends Simulator[ArcId, PlaceId, TransitionId, A, P, T, Self],
      Net.Topology.NoDanglingArcs[ArcId, PlaceId, TransitionId, A, P, T],
      Net.Topology.SingleArc[ArcId, PlaceId, TransitionId, A, P, T] {

    /** All arcs connected to transition `t`, as (arcId, arc) pairs. Implementations may use an
      * internal index (e.g. a TreeMap) for efficiency.
      */
    protected def arcsForTransition(t: TransitionId): List[(ArcId, A)]

    /** Return a new `Self` with the given place markings merged in. Only the places in `updates`
      * need to change; all other places remain as-is.
      */
    protected def withUpdatedPlaces(updates: Iterable[(PlaceId, P)]): Self

    override def fire(t: TransitionId): Either[Simulator.FiringError, Self] = {
        // Check transition exists
        if !transitionIds.contains(t) then return Left(Simulator.FiringError.TransitionNotFound(t))

        // Check net-level enabling conditions
        if !netEnablingPredicate(t) then return Left(Simulator.FiringError.NetEnablingFailed(t))

        // Snapshot all (arc, place) pairs for this transition before firing any endo.
        // NoDanglingArcs guarantees all arc place references are valid; a Left here means
        // the net was not validated before simulating.
        val arcPlacePairs: Either[Simulator.FiringError, List[(ArcId, A, PlaceId, P)]] =
            arcsForTransition(t).traverse { (arcId, arc) =>
                getPlaceSemantics(arc.arcPlaceId)
                    .leftMap(_ => SequentialSimulator.DanglingArc(arcId, arc.arcPlaceId))
                    .map(place => (arcId, arc, arc.arcPlaceId, place))
            }

        // For each arc: check arc-side enabling (E2), apply firing endo,
        // then check place-side validity (E1). All against the pre-fire snapshot.
        arcPlacePairs
            .flatMap(_.traverse { (arcId, arc, placeId, place) =>
                for
                    _ <- arc
                        .enablingError(place)
                        .map(Simulator.FiringError.ArcNotEnabled(arcId, placeId, _))
                        .toLeft(())
                    firedPlace <- arc
                        .fire(place)
                        .leftMap(Simulator.FiringError.ArcFiringFailed(arcId, _))
                    _ <- firedPlace.markingError
                        .map(Simulator.FiringError.PlaceValidityViolated(placeId, _))
                        .toLeft(())
                yield (placeId, firedPlace)
            })
            .map(withUpdatedPlaces)
    }
}

object SequentialSimulator {

    /** Arc `arcId` references place `placeId` which has no semantics entry. Specific to
      * [[SequentialSimulator]] because it arises from the [[Net.Topology.NoDanglingArcs]]
      * constraint this simulator enforces. Should not occur in a topologically valid net — validate
      * via [[Net.Topology.isValidTopology]] before simulating.
      */
    case class DanglingArc[ArcId, PlaceId](arcId: ArcId, placeId: PlaceId)
        extends Simulator.FiringError {
        override def getMessage: String =
            s"Arc $arcId references place $placeId which has no semantics entry; validate topology before simulating"
    }
}
