package hydrozoa.lib.petri

import cats.implicits.*
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
    placesMap: TreeMap[PlaceId, P],
    transitionsMap: TreeMap[TransitionId, T],
    arcsMap: TreeMap[ArcId, A]
) extends Simulator[ArcId, PlaceId, TransitionId, A, P, T, MapNet[
      ArcId,
      PlaceId,
      TransitionId,
      A,
      P,
      T
    ]],
      Net.Topology.NoDanglingArcs[ArcId, PlaceId, TransitionId, A, P, T],
      Net.Topology.SingleArc[ArcId, PlaceId, TransitionId, A, P, T] {

    // ---------------------------------------------------------------------------
    // net.Id
    // ---------------------------------------------------------------------------

    override val arcIds: Set[ArcId] = arcsMap.keySet
    override val placeIds: Set[PlaceId] = placesMap.keySet
    override val transitionIds: Set[TransitionId] = transitionsMap.keySet

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

    // ---------------------------------------------------------------------------
    // Simulator
    // ---------------------------------------------------------------------------

    override def fire(
        t: TransitionId
    ): Either[Simulator.FiringError, MapNet[ArcId, PlaceId, TransitionId, A, P, T]] = {
        // Check transition exists
        if !transitionIds.contains(t) then return Left(Simulator.FiringError.TransitionNotFound(t))

        // Check net-level enabling conditions
        if !netEnablingPredicate(t) then return Left(Simulator.FiringError.NetEnablingFailed(t))

        // Snapshot all (arc, place) pairs for this transition before firing any endo
        val arcPlacePairs: List[(ArcId, A, PlaceId, P)] = for
            (arcId, arc) <- arcsMap.toList
            if arc.arcTransitionId == t
            place <- placesMap.get(arc.arcPlaceId).toList
        yield (arcId, arc, arc.arcPlaceId, place)

        // For each arc: check arc-side enabling (E2), apply firing endo,
        // then check place-side validity (E1). All against the pre-fire snapshot.
        val firedPlaces: Either[Simulator.FiringError, List[(PlaceId, P)]] =
            arcPlacePairs.traverse { (arcId, arc, placeId, place) =>
                for
                    _ <- Either.cond(
                      arc.enabled(place),
                      (),
                      Simulator.FiringError.ArcNotEnabled(arcId, placeId)
                    )
                    firedPlace <- arc
                        .fire(place)
                        .leftMap(Simulator.FiringError.ArcFiringFailed(arcId, _))
                    _ <- Either.cond(
                      firedPlace.validMarking,
                      (),
                      Simulator.FiringError.PlaceValidityViolated(
                        placeId,
                        firedPlace.markingError(firedPlace.getMarking)
                      )
                    )
                yield (placeId, firedPlace)
            }

        firedPlaces.map(updates => this.copy(placesMap = placesMap ++ updates))
    }
}
