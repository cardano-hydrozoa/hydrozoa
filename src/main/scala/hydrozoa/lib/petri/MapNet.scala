package hydrozoa.lib.petri

import cats.Monad
import cats.data.IndexedStateT
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
) extends SequentialSimulator[ArcId, PlaceId, TransitionId, A, P, T, MapNet[
      ArcId,
      PlaceId,
      TransitionId,
      A,
      P,
      T
    ]] {

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
    // SequentialSimulator
    // ---------------------------------------------------------------------------

    override protected def arcsForTransition(t: TransitionId): List[(ArcId, A)] =
        arcsMap.toList.collect { case (id, arc) if arc.arcTransitionId == t => (id, arc) }

    override protected def withUpdatedPlaces(
        updates: Iterable[(PlaceId, P)]
    ): MapNet[ArcId, PlaceId, TransitionId, A, P, T] =
        this.copy(placesMap = placesMap ++ updates)
}

object MapNet {

    // =========================================================================
    // BuilderError
    // =========================================================================

    /** Errors that can arise when building a [[MapNet]] with [[BuilderM]]. */
    enum BuilderError[ArcId, PlaceId, TransitionId]:
        case PlaceIdConflict(placeId: PlaceId)
        case PlaceIdMissing(placeId: PlaceId)
        case TransitionIdConflict(transitionId: TransitionId)
        case TransitionIdMissing(transitionId: TransitionId)
        case ArcIdConflict(arcId: ArcId)
        case ArcIdMissing(arcId: ArcId)

    // =========================================================================
    // empty
    // =========================================================================

    /** An empty [[MapNet]] with no places, transitions, or arcs. Useful as the starting state for
      * [[BuilderM.runEmpty]].
      */
    def empty[
        ArcId: Ordering,
        PlaceId: Ordering,
        TransitionId: Ordering,
        A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
        P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
        T <: Transition.Topology & Transition.Syntax & Transition.Semantics
    ]: MapNet[ArcId, PlaceId, TransitionId, A, P, T] =
        MapNet(TreeMap.empty, TreeMap.empty, TreeMap.empty)

    // =========================================================================
    // BuilderM — opaque builder monad
    // =========================================================================

    /** Opaque builder monad: `IndexedStateT[Either[BuilderError, _], MapNet, MapNet, A]`.
      *
      * Programs are assembled from the operations in [[BuilderM]] (add / remove / update for
      * places, transitions, and arcs), composed via `map` / `flatMap` (or for-comprehensions with
      * `import cats.syntax.all.*`), and executed with [[BuilderM.run]] or [[BuilderM.runEmpty]].
      *
      * The monad short-circuits on the first [[BuilderError]]. State can only be modified through
      * the provided builder operations; use [[BuilderM.inspect]] for read-only access.
      *
      * ## Naming convention
      *
      * Operations without a trailing `_` are _strict_: they return a [[BuilderError]] if the
      * precondition is violated (e.g. ID already exists, or ID not found). Operations with a
      * trailing `_` are _force_ variants: they silently overwrite or no-op instead of failing.
      */
    opaque type BuilderM[
        ArcId,
        PlaceId,
        TransitionId,
        A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
        P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
        T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
        Result
    ] = IndexedStateT[
      [X] =>> Either[BuilderError[ArcId, PlaceId, TransitionId], X],
      MapNet[ArcId, PlaceId, TransitionId, A, P, T],
      MapNet[ArcId, PlaceId, TransitionId, A, P, T],
      Result
    ]

    object BuilderM {

        // ----- Constructors --------------------------------------------------

        def pure[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
            Result
        ](r: Result): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Result] =
            IndexedStateT.pure(r)

        /** Read-only access to the current net state. */
        def inspect[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
            Result
        ](
            f: MapNet[ArcId, PlaceId, TransitionId, A, P, T] => Result
        ): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Result] =
            IndexedStateT.inspect(f)

        // ----- Place operations ----------------------------------------------

        /** Add a place. Fails with [[BuilderError.PlaceIdConflict]] if `id` is already present. */
        def addPlace[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: PlaceId, place: P): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                if mn.placesMap.contains(id) then Left(BuilderError.PlaceIdConflict(id))
                else Right((mn.copy(placesMap = mn.placesMap.updated(id, place)), ()))
            )

        /** Add or overwrite a place, regardless of whether `id` already exists. */
        def addPlace_[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: PlaceId, place: P): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn => Right((mn.copy(placesMap = mn.placesMap.updated(id, place)), ())))

        /** Update an existing place. Fails with [[BuilderError.PlaceIdMissing]] if `id` is absent.
          */
        def updatePlace[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: PlaceId)(
            f: P => P
        ): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                mn.placesMap.get(id) match
                    case None    => Left(BuilderError.PlaceIdMissing(id))
                    case Some(p) => Right((mn.copy(placesMap = mn.placesMap.updated(id, f(p))), ()))
            )

        /** Update an existing place, or no-op if `id` is absent. */
        def updatePlace_[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: PlaceId)(
            f: P => P
        ): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                mn.placesMap.get(id) match
                    case None    => Right((mn, ()))
                    case Some(p) => Right((mn.copy(placesMap = mn.placesMap.updated(id, f(p))), ()))
            )

        /** Remove a place. Fails with [[BuilderError.PlaceIdMissing]] if `id` is absent. */
        def removePlace[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: PlaceId): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                if mn.placesMap.contains(id) then
                    Right((mn.copy(placesMap = mn.placesMap.removed(id)), ()))
                else Left(BuilderError.PlaceIdMissing(id))
            )

        /** Remove a place, or no-op if `id` is absent. */
        def removePlace_[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: PlaceId): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn => Right((mn.copy(placesMap = mn.placesMap.removed(id)), ())))

        // ----- Transition operations -----------------------------------------

        /** Add a transition. Fails with [[BuilderError.TransitionIdConflict]] if `id` is already
          * present.
          */
        def addTransition[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: TransitionId, transition: T): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                if mn.transitionsMap.contains(id) then Left(BuilderError.TransitionIdConflict(id))
                else
                    Right((mn.copy(transitionsMap = mn.transitionsMap.updated(id, transition)), ()))
            )

        /** Add or overwrite a transition, regardless of whether `id` already exists. */
        def addTransition_[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: TransitionId, transition: T): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                Right(
                  (mn.copy(transitionsMap = mn.transitionsMap.updated(id, transition)), ())
                )
            )

        /** Update an existing transition. Fails with [[BuilderError.TransitionIdMissing]] if `id`
          * is absent.
          */
        def updateTransition[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: TransitionId)(
            f: T => T
        ): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                mn.transitionsMap.get(id) match
                    case None =>
                        Left(BuilderError.TransitionIdMissing(id))
                    case Some(t) =>
                        Right(
                          (mn.copy(transitionsMap = mn.transitionsMap.updated(id, f(t))), ())
                        )
            )

        /** Update an existing transition, or no-op if `id` is absent. */
        def updateTransition_[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: TransitionId)(
            f: T => T
        ): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                mn.transitionsMap.get(id) match
                    case None =>
                        Right((mn, ()))
                    case Some(t) =>
                        Right(
                          (mn.copy(transitionsMap = mn.transitionsMap.updated(id, f(t))), ())
                        )
            )

        /** Remove a transition. Fails with [[BuilderError.TransitionIdMissing]] if `id` is absent.
          */
        def removeTransition[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: TransitionId): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                if mn.transitionsMap.contains(id) then
                    Right((mn.copy(transitionsMap = mn.transitionsMap.removed(id)), ()))
                else Left(BuilderError.TransitionIdMissing(id))
            )

        /** Remove a transition, or no-op if `id` is absent. */
        def removeTransition_[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: TransitionId): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                Right((mn.copy(transitionsMap = mn.transitionsMap.removed(id)), ()))
            )

        // ----- Arc operations ------------------------------------------------

        /** Add an arc. Fails with [[BuilderError.ArcIdConflict]] if `id` is already present. */
        def addArc[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: ArcId, arc: A): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                if mn.arcsMap.contains(id) then Left(BuilderError.ArcIdConflict(id))
                else Right((mn.copy(arcsMap = mn.arcsMap.updated(id, arc)), ()))
            )

        /** Add or overwrite an arc, regardless of whether `id` already exists. */
        def addArc_[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: ArcId, arc: A): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn => Right((mn.copy(arcsMap = mn.arcsMap.updated(id, arc)), ())))

        /** Update an existing arc. Fails with [[BuilderError.ArcIdMissing]] if `id` is absent. */
        def updateArc[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: ArcId)(
            f: A => A
        ): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                mn.arcsMap.get(id) match
                    case None    => Left(BuilderError.ArcIdMissing(id))
                    case Some(a) => Right((mn.copy(arcsMap = mn.arcsMap.updated(id, f(a))), ()))
            )

        /** Update an existing arc, or no-op if `id` is absent. */
        def updateArc_[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: ArcId)(
            f: A => A
        ): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                mn.arcsMap.get(id) match
                    case None    => Right((mn, ()))
                    case Some(a) => Right((mn.copy(arcsMap = mn.arcsMap.updated(id, f(a))), ()))
            )

        /** Remove an arc. Fails with [[BuilderError.ArcIdMissing]] if `id` is absent. */
        def removeArc[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: ArcId): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn =>
                if mn.arcsMap.contains(id) then
                    Right((mn.copy(arcsMap = mn.arcsMap.removed(id)), ()))
                else Left(BuilderError.ArcIdMissing(id))
            )

        /** Remove an arc, or no-op if `id` is absent. */
        def removeArc_[
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ](id: ArcId): BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Unit] =
            IndexedStateT(mn => Right((mn.copy(arcsMap = mn.arcsMap.removed(id)), ())))

        // ----- Eliminators ---------------------------------------------------

        extension [
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
            Result
        ](bm: BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Result])

            /** Run the program from `initial`, returning the final net and result value. */
            def run(
                initial: MapNet[ArcId, PlaceId, TransitionId, A, P, T]
            ): Either[
              BuilderError[ArcId, PlaceId, TransitionId],
              (
                  MapNet[ArcId, PlaceId, TransitionId, A, P, T],
                  Result
              )
            ] = (bm: IndexedStateT[
              [X] =>> Either[BuilderError[ArcId, PlaceId, TransitionId], X],
              MapNet[ArcId, PlaceId, TransitionId, A, P, T],
              MapNet[ArcId, PlaceId, TransitionId, A, P, T],
              Result
            ]).run(initial)

            /** Run the program from an empty net. */
            def runEmpty: Either[
              BuilderError[ArcId, PlaceId, TransitionId],
              (
                  MapNet[ArcId, PlaceId, TransitionId, A, P, T],
                  Result
              )
            ] = run(MapNet.empty[ArcId, PlaceId, TransitionId, A, P, T])

        // ----- Cats type class instance ---------------------------------------

        /** `Monad` instance for `BuilderM[ArcId, PlaceId, TransitionId, A, P, T, _]`.
          *
          * Import `cats.syntax.all.*` to get `map`, `flatMap`, `traverse`, etc. on `BuilderM`
          * values.
          */
        // Named call bypasses the circular implicit search that arises because BuilderM is
        // transparent as IndexedStateT inside this object. See the analogous note in Simulator.Sim.
        given [
            ArcId: Ordering,
            PlaceId: Ordering,
            TransitionId: Ordering,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ]: Monad[[Result] =>> BuilderM[ArcId, PlaceId, TransitionId, A, P, T, Result]] =
            cats.data.IndexedStateT.catsDataMonadForIndexedStateT[
              [X] =>> Either[BuilderError[ArcId, PlaceId, TransitionId], X],
              MapNet[ArcId, PlaceId, TransitionId, A, P, T]
            ]
    }
}
