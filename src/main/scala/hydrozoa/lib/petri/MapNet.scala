package hydrozoa.lib.petri

import cats.Monad
import cats.data.IndexedStateT
import cats.implicits.*
import hydrozoa.lib.petri.net.*
import hydrozoa.lib.petri.net.components.*
import scala.collection.immutable.TreeMap

/** A full, canonical map-backed representation. Suitable for building and simulating typed nets.
  * TreeMap is used internally for deterministic iteration order, which stabilises serialisation and
  * makes firing order deterministic.
  *
  * The arc map is keyed by [[Arc.Flow]] elements: its key set is the flow relation `F` and the map
  * itself is the annotation function `W : F → AN` (Concept 8) — a function by construction.
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
  * @tparam PlaceId
  *   place identity type (must have an [[Ordering]] for TreeMap)
  * @tparam TransitionId
  *   transition identity type (must have an [[Ordering]] for TreeMap)
  * @tparam W
  *   arc-annotation type (ISO's `W`)
  * @tparam M
  *   place-marking type
  * @tparam A
  *   concrete arc value type — carries the annotation
  * @tparam P
  *   concrete place type — carries syntax (marking type [[M]]) and semantics
  * @tparam T
  *   concrete transition type (currently a stub; no Transition.Semantics yet)
  */
case class MapNet[
    PlaceId: Ordering,
    TransitionId: Ordering,
    W,
    M,
    A <: Arc.Syntax.Annotated[W],
    P <: Place.Topology & Place.Syntax.Marked[P, M] & Place.Semantics[P],
    T <: Transition.Topology & Transition.Syntax & Transition.Semantics
](
    placesMap: TreeMap[PlaceId, P],
    transitionsMap: TreeMap[TransitionId, T],
    arcsMap: TreeMap[Arc.Flow[PlaceId, TransitionId], A]
)(using markingAlgebra: MarkingAlgebra[W, M])
    extends SequentialSimulator[PlaceId, TransitionId, W, M, A, P, T, MapNet[
      PlaceId,
      TransitionId,
      W,
      M,
      A,
      P,
      T
    ]] {

    // ---------------------------------------------------------------------------
    // net.Ids
    // ---------------------------------------------------------------------------

    override val placeIds: Set[PlaceId] = placesMap.keySet
    override val transitionIds: Set[TransitionId] = transitionsMap.keySet
    override val flowRelation: Set[Arc.Flow[PlaceId, TransitionId]] = arcsMap.keySet

    // ---------------------------------------------------------------------------
    // Net.Topology.Topology (via NoDanglingArcs mixin chain)
    // ---------------------------------------------------------------------------

    def getPlaceTopology(placeId: PlaceId): Either[Net.Topology.MissingPlaceTopology[PlaceId], P] =
        placesMap.get(placeId).toRight(Net.Topology.MissingPlaceTopology(placeId))

    def getTransitionTopology(
        transitionId: TransitionId
    ): Either[Net.Topology.MissingTransitionTopology[TransitionId], T] =
        transitionsMap
            .get(transitionId)
            .toRight(Net.Topology.MissingTransitionTopology(transitionId))

    // Net.Topology (outer) — bridge: delegate to the NoDanglingArcs mixin chain above.
    // Required because the inner Net.Topology.Topology and outer Net.Topology are separate trait
    // hierarchies; Scala does not auto-satisfy the outer abstract via the inner abstract-override chain.
    override def topologyErrors: List[Net.Topology.Error] = super.topologyErrors

    // ---------------------------------------------------------------------------
    // Net.Syntax.Syntax
    // ---------------------------------------------------------------------------

    def getArcSyntax(
        flow: Arc.Flow[PlaceId, TransitionId]
    ): Either[Net.Syntax.MissingArcSyntax[PlaceId, TransitionId], A] =
        arcsMap.get(flow).toRight(Net.Syntax.MissingArcSyntax(flow))

    def getPlaceSyntax(id: PlaceId): Either[Net.Syntax.MissingPlaceSyntax[PlaceId], P] =
        placesMap.get(id).toRight(Net.Syntax.MissingPlaceSyntax(id))

    def getTransitionSyntax(
        id: TransitionId
    ): Either[Net.Syntax.MissingTransitionSyntax[TransitionId], T] =
        transitionsMap.get(id).toRight(Net.Syntax.MissingTransitionSyntax(id))

    // ---------------------------------------------------------------------------
    // Net.Semantics (outer) — places and transitions only; arcs carry no semantics
    // ---------------------------------------------------------------------------

    override def getPlaceSemantics(
        id: PlaceId
    ): Either[Net.Semantics.Error.MissingPlaceSemantics[PlaceId], P] =
        placesMap.get(id).toRight(Net.Semantics.Error.MissingPlaceSemantics(id))

    override def getTransitionSemantics(
        id: TransitionId
    ): Either[Net.Semantics.Error.MissingTransitionSemantics[TransitionId], T] =
        transitionsMap.get(id).toRight(Net.Semantics.Error.MissingTransitionSemantics(id))

    // ---------------------------------------------------------------------------
    // SequentialSimulator
    // ---------------------------------------------------------------------------

    override protected val algebra: MarkingAlgebra[W, M] = markingAlgebra

    override protected def arcsForTransition(
        t: TransitionId
    ): List[(Arc.Flow[PlaceId, TransitionId], A)] =
        arcsMap.toList.filter((flow, _) => flow.transition == t)

    override protected def withUpdatedPlaces(
        updates: Iterable[(PlaceId, P)]
    ): MapNet[PlaceId, TransitionId, W, M, A, P, T] =
        this.copy(placesMap = placesMap ++ updates)
}

object MapNet {

    // =========================================================================
    // BuilderError — shared by BuilderM and BuilderMOps
    // =========================================================================

    /** Errors that can arise when building a [[MapNet]] with [[BuilderMOps]]. */
    enum BuilderError[PlaceId, TransitionId]:
        case PlaceIdConflict(placeId: PlaceId)
        case PlaceIdMissing(placeId: PlaceId)
        case TransitionIdConflict(transitionId: TransitionId)
        case TransitionIdMissing(transitionId: TransitionId)
        case ArcConflict(flow: Arc.Flow[PlaceId, TransitionId])
        case ArcMissing(flow: Arc.Flow[PlaceId, TransitionId])

    // =========================================================================
    // empty
    // =========================================================================

    /** An empty [[MapNet]] with no places, transitions, or arcs. Useful as the starting state for
      * [[BuilderM.runEmpty]].
      */
    def empty[
        PlaceId: Ordering,
        TransitionId: Ordering,
        W,
        M,
        A <: Arc.Syntax.Annotated[W],
        P <: Place.Topology & Place.Syntax.Marked[P, M] & Place.Semantics[P],
        T <: Transition.Topology & Transition.Syntax & Transition.Semantics
    ](using MarkingAlgebra[W, M]): MapNet[PlaceId, TransitionId, W, M, A, P, T] =
        MapNet(TreeMap.empty, TreeMap.empty, TreeMap.empty)

    // =========================================================================
    // BuilderM — class wrapping IndexedStateT
    // =========================================================================

    /** Builder monad for [[MapNet]]: wraps `IndexedStateT[Either[BuilderError, _], MapNet, MapNet,
      * Result]`.
      *
      * `map` and `flatMap` are defined directly on the class, so for-comprehensions work without
      * any import. Programs are built via [[BuilderMOps]], which fixes the net type parameters once
      * so that operations infer cleanly at every call site. Execute with [[run]] or [[runEmpty]].
      *
      * The monad short-circuits on the first [[BuilderError]].
      */
    final class BuilderM[
        PlaceId,
        TransitionId,
        W,
        M,
        A <: Arc.Syntax.Annotated[W],
        P <: Place.Topology & Place.Syntax.Marked[P, M] & Place.Semantics[P],
        T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
        Result
    ] private[MapNet] (
        private[MapNet] val inner: IndexedStateT[
          [X] =>> Either[BuilderError[PlaceId, TransitionId], X],
          MapNet[PlaceId, TransitionId, W, M, A, P, T],
          MapNet[PlaceId, TransitionId, W, M, A, P, T],
          Result
        ]
    ) {
        def map[B](
            f: Result => B
        ): BuilderM[PlaceId, TransitionId, W, M, A, P, T, B] =
            new BuilderM(inner.map(f))

        def flatMap[B](
            f: Result => BuilderM[PlaceId, TransitionId, W, M, A, P, T, B]
        ): BuilderM[PlaceId, TransitionId, W, M, A, P, T, B] =
            new BuilderM(inner.flatMap(r => f(r).inner))

        def run(
            initial: MapNet[PlaceId, TransitionId, W, M, A, P, T]
        ): Either[
          BuilderError[PlaceId, TransitionId],
          (
              MapNet[PlaceId, TransitionId, W, M, A, P, T],
              Result
          )
        ] = inner.run(initial)

        def runEmpty(using
            Ordering[PlaceId],
            Ordering[TransitionId],
            MarkingAlgebra[W, M]
        ): Either[
          BuilderError[PlaceId, TransitionId],
          (
              MapNet[PlaceId, TransitionId, W, M, A, P, T],
              Result
          )
        ] = run(MapNet.empty[PlaceId, TransitionId, W, M, A, P, T])
    }

    object BuilderM {

        /** `Monad` instance for `BuilderM[PlaceId, TransitionId, W, M, A, P, T, _]`. Provides
          * `traverse`, `sequence`, and other derived Cats combinators when in scope.
          */
        given [
            PlaceId,
            TransitionId,
            W,
            M,
            A <: Arc.Syntax.Annotated[W],
            P <: Place.Topology & Place.Syntax.Marked[P, M] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics
        ]: Monad[[R] =>> BuilderM[PlaceId, TransitionId, W, M, A, P, T, R]] with {

            private type BE = BuilderError[PlaceId, TransitionId]
            private type MN = MapNet[PlaceId, TransitionId, W, M, A, P, T]

            override def pure[R](r: R): BuilderM[PlaceId, TransitionId, W, M, A, P, T, R] =
                new BuilderM(IndexedStateT.pure(r))

            override def flatMap[R, S](
                fa: BuilderM[PlaceId, TransitionId, W, M, A, P, T, R]
            )(
                f: R => BuilderM[PlaceId, TransitionId, W, M, A, P, T, S]
            ): BuilderM[PlaceId, TransitionId, W, M, A, P, T, S] =
                fa.flatMap(f)

            // Delegates to IndexedStateT's tailRecM by explicit type application to avoid
            // the circular implicit search that would arise from summon[Monad[BuilderM[...]]].
            override def tailRecM[R, S](r: R)(
                f: R => BuilderM[PlaceId, TransitionId, W, M, A, P, T, Either[R, S]]
            ): BuilderM[PlaceId, TransitionId, W, M, A, P, T, S] =
                new BuilderM(
                  Monad[[Y] =>> IndexedStateT[[X] =>> Either[BE, X], MN, MN, Y]]
                      .tailRecM(r)(r0 => f(r0).inner)
                )
        }
    }

    // =========================================================================
    // BuilderMOps — fixed-type-param factory for BuilderM programs
    // =========================================================================

    /** Fixed-type-parameter factory for [[BuilderM]] programs. Instantiate once for your net type
      * and import (or use) the result to get clean for-comprehension syntax without repeated type
      * applications.
      *
      * {{{
      * val ops = MapNet.BuilderMOps[String, String, PositiveInt, Natural, MyArc, MyPlace, MyTransition]()
      * import ops.*
      *
      * val program = for
      *     _ <- addPlace("p1", myPlace)
      *     _ <- addTransition("t1", myTransition)
      *     _ <- addArc(Arc.Flow.Pt("p1", "t1"), myArc)
      * yield ()
      *
      * val net = program.runEmpty
      * }}}
      *
      * ## Naming convention
      *
      * Operations without a trailing `_` are _strict_: they return a [[BuilderError]] if the
      * precondition is violated (e.g. ID already exists, or ID not found). Operations with a
      * trailing `_` are _force_ variants: they silently overwrite or no-op instead of failing.
      */
    class BuilderMOps[
        PlaceId: Ordering,
        TransitionId: Ordering,
        W,
        M,
        A <: Arc.Syntax.Annotated[W],
        P <: Place.Topology & Place.Syntax.Marked[P, M] & Place.Semantics[P],
        T <: Transition.Topology & Transition.Syntax & Transition.Semantics
    ](using MarkingAlgebra[W, M]) {
        private type BM[R] = BuilderM[PlaceId, TransitionId, W, M, A, P, T, R]
        private type MN = MapNet[PlaceId, TransitionId, W, M, A, P, T]
        private type BE = BuilderError[PlaceId, TransitionId]

        private def lift[R](f: MN => Either[BE, (MN, R)]): BM[R] =
            new BuilderM(IndexedStateT(f))

        // ----- Read-only access ----------------------------------------------

        def inspect[R](f: MN => R): BM[R] =
            new BuilderM(IndexedStateT.inspect(f))

        // ----- Place operations ----------------------------------------------

        /** Add a place. Fails with [[BuilderError.PlaceIdConflict]] if `id` is already present. */
        def addPlace(id: PlaceId, place: P): BM[Unit] = lift(mn =>
            if mn.placesMap.contains(id) then Left(BuilderError.PlaceIdConflict(id))
            else Right((mn.copy(placesMap = mn.placesMap.updated(id, place)), ()))
        )

        /** Add or overwrite a place, regardless of whether `id` already exists. */
        def addPlace_(id: PlaceId, place: P): BM[Unit] =
            lift(mn => Right((mn.copy(placesMap = mn.placesMap.updated(id, place)), ())))

        /** Update an existing place. Fails with [[BuilderError.PlaceIdMissing]] if `id` is absent.
          */
        def updatePlace(id: PlaceId)(f: P => P): BM[Unit] = lift(mn =>
            mn.placesMap.get(id) match
                case None    => Left(BuilderError.PlaceIdMissing(id))
                case Some(p) => Right((mn.copy(placesMap = mn.placesMap.updated(id, f(p))), ()))
        )

        /** Update an existing place, or no-op if `id` is absent. */
        def updatePlace_(id: PlaceId)(f: P => P): BM[Unit] = lift(mn =>
            mn.placesMap.get(id) match
                case None    => Right((mn, ()))
                case Some(p) => Right((mn.copy(placesMap = mn.placesMap.updated(id, f(p))), ()))
        )

        /** Remove a place. Fails with [[BuilderError.PlaceIdMissing]] if `id` is absent. */
        def removePlace(id: PlaceId): BM[Unit] = lift(mn =>
            if mn.placesMap.contains(id) then
                Right((mn.copy(placesMap = mn.placesMap.removed(id)), ()))
            else Left(BuilderError.PlaceIdMissing(id))
        )

        /** Remove a place, or no-op if `id` is absent. */
        def removePlace_(id: PlaceId): BM[Unit] =
            lift(mn => Right((mn.copy(placesMap = mn.placesMap.removed(id)), ())))

        // ----- Transition operations -----------------------------------------

        /** Add a transition. Fails with [[BuilderError.TransitionIdConflict]] if `id` is already
          * present.
          */
        def addTransition(id: TransitionId, transition: T): BM[Unit] = lift(mn =>
            if mn.transitionsMap.contains(id) then Left(BuilderError.TransitionIdConflict(id))
            else Right((mn.copy(transitionsMap = mn.transitionsMap.updated(id, transition)), ()))
        )

        /** Add or overwrite a transition, regardless of whether `id` already exists. */
        def addTransition_(id: TransitionId, transition: T): BM[Unit] = lift(mn =>
            Right((mn.copy(transitionsMap = mn.transitionsMap.updated(id, transition)), ()))
        )

        /** Update an existing transition. Fails with [[BuilderError.TransitionIdMissing]] if `id`
          * is absent.
          */
        def updateTransition(id: TransitionId)(f: T => T): BM[Unit] = lift(mn =>
            mn.transitionsMap.get(id) match
                case None => Left(BuilderError.TransitionIdMissing(id))
                case Some(t) =>
                    Right((mn.copy(transitionsMap = mn.transitionsMap.updated(id, f(t))), ()))
        )

        /** Update an existing transition, or no-op if `id` is absent. */
        def updateTransition_(id: TransitionId)(f: T => T): BM[Unit] = lift(mn =>
            mn.transitionsMap.get(id) match
                case None => Right((mn, ()))
                case Some(t) =>
                    Right((mn.copy(transitionsMap = mn.transitionsMap.updated(id, f(t))), ()))
        )

        /** Remove a transition. Fails with [[BuilderError.TransitionIdMissing]] if `id` is absent.
          */
        def removeTransition(id: TransitionId): BM[Unit] = lift(mn =>
            if mn.transitionsMap.contains(id) then
                Right((mn.copy(transitionsMap = mn.transitionsMap.removed(id)), ()))
            else Left(BuilderError.TransitionIdMissing(id))
        )

        /** Remove a transition, or no-op if `id` is absent. */
        def removeTransition_(id: TransitionId): BM[Unit] =
            lift(mn => Right((mn.copy(transitionsMap = mn.transitionsMap.removed(id)), ())))

        // ----- Arc operations ------------------------------------------------

        /** Annotate flow element `flow` with `arc`. Fails with [[BuilderError.ArcConflict]] if the
          * element is already annotated (`W` is a function on `F`).
          */
        def addArc(flow: Arc.Flow[PlaceId, TransitionId], arc: A): BM[Unit] = lift(mn =>
            if mn.arcsMap.contains(flow) then Left(BuilderError.ArcConflict(flow))
            else Right((mn.copy(arcsMap = mn.arcsMap.updated(flow, arc)), ()))
        )

        /** Annotate or re-annotate flow element `flow`, regardless of whether it is present. */
        def addArc_(flow: Arc.Flow[PlaceId, TransitionId], arc: A): BM[Unit] =
            lift(mn => Right((mn.copy(arcsMap = mn.arcsMap.updated(flow, arc)), ())))

        /** Update an existing arc. Fails with [[BuilderError.ArcMissing]] if `flow` is absent. */
        def updateArc(flow: Arc.Flow[PlaceId, TransitionId])(f: A => A): BM[Unit] = lift(mn =>
            mn.arcsMap.get(flow) match
                case None    => Left(BuilderError.ArcMissing(flow))
                case Some(a) => Right((mn.copy(arcsMap = mn.arcsMap.updated(flow, f(a))), ()))
        )

        /** Update an existing arc, or no-op if `flow` is absent. */
        def updateArc_(flow: Arc.Flow[PlaceId, TransitionId])(f: A => A): BM[Unit] = lift(mn =>
            mn.arcsMap.get(flow) match
                case None    => Right((mn, ()))
                case Some(a) => Right((mn.copy(arcsMap = mn.arcsMap.updated(flow, f(a))), ()))
        )

        /** Remove an arc. Fails with [[BuilderError.ArcMissing]] if `flow` is absent. */
        def removeArc(flow: Arc.Flow[PlaceId, TransitionId]): BM[Unit] = lift(mn =>
            if mn.arcsMap.contains(flow) then
                Right((mn.copy(arcsMap = mn.arcsMap.removed(flow)), ()))
            else Left(BuilderError.ArcMissing(flow))
        )

        /** Remove an arc, or no-op if `flow` is absent. */
        def removeArc_(flow: Arc.Flow[PlaceId, TransitionId]): BM[Unit] =
            lift(mn => Right((mn.copy(arcsMap = mn.arcsMap.removed(flow)), ())))
    }
}
