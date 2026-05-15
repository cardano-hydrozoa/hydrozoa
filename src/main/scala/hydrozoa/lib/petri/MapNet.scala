package hydrozoa.lib.petri

import hydrozoa.lib.petri.net.*
import hydrozoa.lib.petri.net.Net.Semantics.Error
import hydrozoa.lib.petri.net.components.*

import scala.collection.immutable.TreeMap

/** A full, canonical map-backed representation. Suitable for building and simulating typed nets.
  * TreeMap is used internally for deterministic iteration order (ArcId / PlaceId / TransitionId
  * ordering), which stabilises serialisation and makes firing order deterministic when multi-arcs
  * are present.
  *
  * MapNet is primarily suitable for _building_ nets rather than large-scale simulation. In
  * particular, determining whether a transition is enabled (via a test-fire) and actually firing
  * are both O(arcs connected to the transition), which is fast enough in practice but slower than a
  * matrix-backed representation for large nets.
  *
  * There are many type parameters here. It is a better choice than using path-dependent types
  * bundled in a trait because it makes inference easier. Downstream code should prefer wrapper
  * types or type aliases.
  *
  * @tparam NetId
  *   identity of this net
  * @tparam ArcId
  *   arc identity type (must have an [[Ordering]] for TreeMap)
  * @tparam PlaceId
  *   place identity type (must have an [[Ordering]] for TreeMap)
  * @tparam TransitionId
  *   transition identity type (must have an [[Ordering]] for TreeMap)
  * @tparam P
  *   concrete place type — must carry both syntax and semantics
  * @tparam A
  *   concrete arc type — must carry topology and semantics over [[P]]
  * @tparam T
  *   concrete transition type (currently a stub; no Transition.Semantics yet)
  */
case class MapNet[
    ArcId: Ordering,
    PlaceId: Ordering,
    TransitionId: Ordering,
    A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
    P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
    T <: Transition.Topology & Transition.Syntax & Transition.Semantics
](
    val name: String,
    val description: String,
    val placesMap: TreeMap[PlaceId, P],
    val transitionsMap: TreeMap[TransitionId, T],
    val arcsMap: TreeMap[ArcId, A]
) extends Net[ArcId, PlaceId, TransitionId, A, P, T],
      Net.Topology.NoDanglingArcs[ArcId, PlaceId, TransitionId, A, P, T],
      Net.Topology.SingleArc[ArcId, PlaceId, TransitionId, A, P, T] {

    // ---------------------------------------------------------------------------
    // net.Id
    // ---------------------------------------------------------------------------

    override val arcs: Set[ArcId] = arcsMap.keySet
    override val places: Set[PlaceId] = placesMap.keySet
    override val transitions: Set[TransitionId] = transitionsMap.keySet

    // ---------------------------------------------------------------------------
    // Net.Topology.Topology (via NoDanglingArcs / SingleArc mixin chain)
    // ---------------------------------------------------------------------------

    def getArcTopology(arcId: ArcId): Either[Net.Topology.MissingArcTopology[ArcId], A] =
        arcsMap.get(arcId).toRight(Net.Topology.MissingArcTopology(arcId))

    def getPlaceTopology(placeId: PlaceId): Either[Net.Topology.MissingPlaceTopology[PlaceId], P] =
        placesMap.get(placeId).toRight(Net.Topology.MissingPlaceTopology(placeId))

    def getTransitionTopology(
        transitionId: TransitionId
    ): Either[Net.Topology.MissingTransitionTopology[TransitionId], T] =
        transitionsMap
            .get(transitionId)
            .toRight(Net.Topology.MissingTransitionTopology(transitionId))

    // Net.Topology (outer) — bridge: delegate to the NoDanglingArcs/SingleArc mixin chain above.
    // Required because the inner Net.Topology.Topology and outer Net.Topology are separate trait
    // hierarchies; Scala does not auto-satisfy the outer abstract via the inner abstract-override chain.
    override def topologyErrors: List[Net.Topology.Error] = super.topologyErrors

    // ---------------------------------------------------------------------------
    // Net.Syntax.Syntax
    // ---------------------------------------------------------------------------

    def getArcSyntax(id: ArcId): Either[Net.Syntax.MissingArcSyntax[ArcId], A] =
        arcsMap.get(id).toRight(Net.Syntax.MissingArcSyntax(id))

    def getPlaceSyntax(id: PlaceId): Either[Net.Syntax.MissingPlaceSyntax[PlaceId], P] =
        placesMap.get(id).toRight(Net.Syntax.MissingPlaceSyntax(id))

    def getTransitionSyntax(
        id: TransitionId
    ): Either[Net.Syntax.MissingTransitionSyntax[TransitionId], T] =
        transitionsMap.get(id).toRight(Net.Syntax.MissingTransitionSyntax(id))

    // Net.Syntax (outer) — syntaxErrors default (List.empty) from Net.Syntax.Syntax satisfies the abstract.

    // ---------------------------------------------------------------------------
    // Net.Semantics (outer)
    // ---------------------------------------------------------------------------

    override def getArcSemantics(id: ArcId): Either[Error.MissingArcSemantics[ArcId], A] =
        arcsMap.get(id).toRight(Net.Semantics.Error.MissingArcSemantics(id))

    override def getPlaceSemantics(id: PlaceId): Either[Error.MissingPlaceSemantics[PlaceId], P] =
        placesMap.get(id).toRight(Net.Semantics.Error.MissingPlaceSemantics(id))

    override def getTransitionSemantics(
        id: TransitionId
    ): Either[Error.MissingTransitionSemantics[TransitionId], T] =
        transitionsMap.get(id).toRight(Net.Semantics.Error.MissingTransitionSemantics(id))
    def semanticsErrors: List[Net.Semantics.Error] = List.empty
}

//
//    // ---------------------------------------------------------------------------
//    // net.Id
//    // ---------------------------------------------------------------------------
//
//    override val arcs: Set[ArcId] = arcsMap.keySet
//    override val places: Set[PlaceId] = placesMap.keySet
//    override val transitions: Set[TransitionId] = transitionsMap.keySet
//
//
//
//    override def fireTransition(t: TransitionId): Either[Simulator.FiringError[TransitionId], Self] = {
//        // Arcs connected to this transition, in deterministic ArcId ordering.
//        // Under the single-arc-per-(place,transition) constraint this order is trivially
//        // irrelevant; with multi-arcs it gives a deterministic firing order.
//        val connectedArcs = arcsMap.filter((_, a) => a.arcTransitionId == t)
//
//        // Check all enabling predicates (AND of all levels; commutative by conjunction).
//        //   - arc-side: arc.enabled(p) for each (arc, place) pair
//        //   - place-side: p.validMarking for each connected place
//        //   - net-wide: netEnablingPredicates(t)
//        // TODO: transition-side (Transition.Semantics is a stub — skip for now)
//        val arcAndPlaceEnabled = connectedArcs.forall { (_, a) =>
//            placesMap.get(a.arcPlaceId).exists(p => a.enabled(p) && p.validMarking)
//        }
//        val netEnabled = netEnablingPredicates(t).forall(identity)
//
//        if !arcAndPlaceEnabled || !netEnabled
//        then Left(Simulator.FiringError.TransitionNotEnabled(t))
//        else
//            // Fire each arc sequentially, threading the updated place map through Either.
//            // `placesMap.get(arc.arcPlaceId)` is safe here: the enabling check above confirmed
//            // existence via `exists` for every connected arc.
//            connectedArcs
//                .foldLeft(Right(placesMap): Either[Simulator.FiringError[TransitionId], TreeMap[PlaceId, P]]) {
//                    case (left @ Left(_), _) => left
//                    case (Right(currentPlaces), (_, arc)) =>
//                        arc.fire(currentPlaces(arc.arcPlaceId)) match {
//                            case Left(e)     => Left(Simulator.FiringError.FireUnsafeFailure(t, e))
//                            case Right(newP) => Right(currentPlaces.updated(arc.arcPlaceId, newP))
//                        }
//                }
//                .map(newPlaces => this.copy(placesMap = newPlaces))
//    }
//}
//
//// TODO: MapNetOps / BuilderM (IndexedStateT monad transformer stack for building MapNets)
////       Resurface when the builder pattern is needed downstream.
