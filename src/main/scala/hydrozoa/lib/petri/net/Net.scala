package hydrozoa.lib.petri.net

import cats.data.NonEmptyList
import hydrozoa.lib.petri.net.Net.Syntax.MissingArcSyntax
import hydrozoa.lib.petri.net.Net.Topology.{MissingArcTopology, MissingPlaceTopology, MissingTransitionTopology}
import hydrozoa.lib.petri.net.components.*

trait Net[
    ArcId,
    PlaceId,
    TransitionId,
    A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
    P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
    T <: Transition.Topology & Transition.Syntax & Transition.Semantics
] extends Net.Ids[
      ArcId,
      PlaceId,
      TransitionId
    ],
      Net.Topology[ArcId, PlaceId, TransitionId, A, P, T],
      Net.Syntax[ArcId, PlaceId, TransitionId, A, P, T],
      Net.Semantics[ArcId, PlaceId, TransitionId, A, P, T] {

    def getArc(
        arcId: ArcId
    ): Either[NonEmptyList[Net.Error], A] = ???

//    override final def getArcSyntax(id: ArcId): Either[MissingArcSyntax[ArcId], A] = ???

}

object Net {

    trait Error extends Throwable

    /** This the authoritative source of IDs in the net in the following sense:
      *   - If there is some data that MUST be associated every ID of a component (such as topology,
      *     configuration, etc), then it is invalid to NOT have a total mapping from these sets to
      *     the associated data
      *   - If an association between an ID and data exists, then the ID MUST appear in this set for
      *     the association to be considered valid.
      *
      * @tparam PlaceId
      * @tparam TransitionId
      * @tparam ArcId
      */
    trait Ids[ArcId, PlaceId, TransitionId] {
        val arcs: Set[ArcId]
        val places: Set[PlaceId]
        val transitions: Set[TransitionId]
        // TODO: Add data variables
    }

    // =========================================================================
    // Topology
    // =========================================================================

    /** Structural query and validation interface for a net. * * `topologyErrors` is abstract —
      * topology constraints are always explicit. Concrete nets * implement it either directly or by
      * composing the mixin traits from [[Topology]]: * - [[Topology.Topology]]: base accessor
      * trait; extend this and mix in below * - [[Topology.NoDanglingArcs]]: no arc references a
      * missing place/transition ID * - [[Topology.SingleArc]]: at most one arc per (place,
      * transition) pair * * Firing endomorphisms do not commute in general; the `SingleArc`
      * constraint makes firing * order-independent. Topologies that permit multi-arcs must impose
      * an explicit ordering or * prove/property-test commutativity for their arc combinations. See
      * * [[hydrozoa.lib.petri.net.components]] for the full discussion.
      */
    trait Topology[
        ArcId,
        PlaceId,
        TransitionId,
        A <: Arc.Topology[PlaceId, TransitionId],
        P <: Place.Topology,
        T <: Transition.Topology
    ] extends Net.Ids[ArcId, PlaceId, TransitionId] {
        type TopologyValidationError = Net.Topology.Error

        def getArcTopology(arcId: ArcId): Either[MissingArcTopology[ArcId], A]

        def getPlaceTopology(placeId: PlaceId): Either[MissingPlaceTopology[PlaceId], P]

        def getTransitionTopology(
            transitionId: TransitionId
        ): Either[MissingTransitionTopology[TransitionId], T]

        // Net-global topology errors
        def topologyErrors: List[TopologyValidationError] = List.empty

        final def isValidTopology: Boolean = topologyErrors.isEmpty
    }

    object Topology {
        trait Error extends Net.Error

        case class MissingArcTopology[ArcId](arcId: ArcId) extends Error

        case class MissingPlaceTopology[PlaceId](placeId: PlaceId) extends Error

        case class MissingTransitionTopology[TransitionId](transitionId: TransitionId) extends Error

        /** Mixin: validates that no arc references a place or transition ID absent from the net.
          *
          * Uses the abstract-override list-accumulation pattern. Must be linearised after
          * [[Topology]] so that `super.topologyErrors` is concrete.
          */
        trait NoDanglingArcs[
            ArcId,
            PlaceId,
            TransitionId,
            A <: Arc.Topology[PlaceId, TransitionId],
            P <: Place.Topology,
            T <: Transition.Topology
        ] extends Topology[ArcId, PlaceId, TransitionId, A, P, T] {
            abstract override def topologyErrors: List[this.TopologyValidationError] = {
                val danglingErrors = for {
                    aid <- arcs.toList
                    errors <- getArcTopology(aid) match {
                        case Left(missingArcTopology) => List(missingArcTopology)
                        case Right(arc) =>
                            val pid: PlaceId = arc.arcPlaceId
                            val tid: TransitionId = arc.arcTransitionId
                            val placeError = Option.when(!places.contains(pid))(
                              NoDanglingArcs.Error.DanglingArcPlace[ArcId, PlaceId, TransitionId](
                                aid,
                                pid
                              )
                            )
                            val transitionError = Option.when(!transitions.contains(tid))(
                              NoDanglingArcs.Error
                                  .DanglingArcTransition[ArcId, PlaceId, TransitionId](aid, tid)
                            )
                            placeError.toList ++ transitionError.toList
                    }
                } yield errors
                danglingErrors ++ super.topologyErrors
            }
        }

        object NoDanglingArcs {
            enum Error[ArcId, PlaceId, TransitionId] extends Net.Topology.Error:
                case DanglingArcPlace(arcId: ArcId, placeId: PlaceId)
                case DanglingArcTransition(arcId: ArcId, transitionId: TransitionId)
        }

        /** Mixin: validates at most one arc per (place, transition) pair.
          *
          * Uses the abstract-override list-accumulation pattern. Must be linearised after
          * [[Topology]] so that `super.topologyErrors` is concrete.
          *
          * `ArcId` requires an [[Ordering]] (already required by `MapNet`) so that the canonical
          * arc (smallest ArcId) is deterministically retained when duplicates are reported.
          */
        trait SingleArc[
            ArcId: Ordering,
            PlaceId,
            TransitionId,
            A <: Arc.Topology[PlaceId, TransitionId],
            P <: Place.Topology,
            T <: Transition.Topology
        ] extends Topology[ArcId, PlaceId, TransitionId, A, P, T] {

            abstract override def topologyErrors: List[this.TopologyValidationError] = {
                val eitherArcs
                    : List[Either[MissingArcTopology[ArcId], (ArcId, PlaceId, TransitionId)]] =
                    arcs.toList.map(arcId =>
                        getArcTopology(arcId).map(arc =>
                            (arcId, arc.arcPlaceId, arc.arcTransitionId)
                        )
                    )
                val partition = eitherArcs.partition(_.isLeft)
                val missingArcs = partition._1.collect { case Left(error) => error }
                val tuples: List[(ArcId, PlaceId, TransitionId)] =
                    partition._2.collect { case Right(tuple) => tuple }
                val grouped: Map[(PlaceId, TransitionId), List[ArcId]] =
                    tuples.groupBy(t => (t._2, t._3)).map((k, v) => (k, v.map(_._1)))
                val duplicateArcErrors = grouped
                    .filter((_, v) => v.size > 1)
                    .map((k, v) =>
                        SingleArc.Error.DuplicateArc(
                          arcIds = NonEmptyList.fromListUnsafe(v),
                          placeId = k._1,
                          transitionId = k._2
                        )
                    )
                    .toList
                duplicateArcErrors ++ missingArcs ++ super.topologyErrors
            }
        }

        object SingleArc {
            object Error {

                /** Reported for every (place, transition) pair with more than one arc. */
                case class DuplicateArc[ArcId, PlaceId, TransitionId](
                    arcIds: NonEmptyList[ArcId],
                    placeId: PlaceId,
                    transitionId: TransitionId
                ) extends Topology.Error
            }
        }
    }

    // =========================================================================
    // Syntax
    // =========================================================================

    /** Component accessor interface parameterized on each component type, so that callers can work
      * with the full concrete types rather than downcasting.
      *
      * Named `Syntax` by analogy with [[Place.Syntax]] and [[Arc.Syntax]]: it describes what the
      * net _carries_, not how it behaves.
      */
    trait Syntax[ArcId, PlaceId, TransitionId, A <: Arc.Syntax, P <: Place.Syntax[
      P
    ], T <: Transition.Syntax]
        extends Net.Ids[ArcId, PlaceId, TransitionId] {

        import Syntax.*

        def getArcSyntax(id: ArcId): Either[MissingArcSyntax[ArcId], A]

        def getPlaceSyntax(id: PlaceId): Either[MissingPlaceSyntax[PlaceId], P]

        def getTransitionSyntax(
            id: TransitionId
        ): Either[MissingTransitionSyntax[TransitionId], T]

        def syntaxErrors: List[Syntax.Error] = List.empty

        final def isValidSyntax: Boolean = syntaxErrors.isEmpty
    }

    object Syntax {
        trait Error extends Net.Error

        case class MissingArcSyntax[ArcId](arcId: ArcId) extends Error

        case class MissingPlaceSyntax[PlaceId](placeId: PlaceId) extends Error

        case class MissingTransitionSyntax[TransitionId](transitionId: TransitionId) extends Error
    }

    // =========================================================================
    // Semantics
    // =========================================================================

    /** Component semantics accessor interface. P is ordered first since A depends on it. P must
      * carry both syntax and semantics because [[Arc.Semantics]] requires `P <: Place.Syntax[P]`.
      */
    trait Semantics[ArcId, PlaceId, TransitionId, A <: Arc.Semantics[P], P <: Place.Syntax[
      P
    ] & Place.Semantics[P], T <: Transition.Semantics]
        extends Net.Ids[ArcId, PlaceId, TransitionId] {
        import Semantics.Error.*
        def getArcSemantics(id: ArcId): Either[MissingArcSemantics[ArcId], A]

        def getPlaceSemantics(id: PlaceId): Either[MissingPlaceSemantics[PlaceId], P]

        def getTransitionSemantics(
            id: TransitionId
        ): Either[MissingTransitionSemantics[TransitionId], T]

        // Override (with abstract override) to add net-wide enabling conditions for transition t.
        // Composed under AND with all other enabling predicate levels; commutative by conjunction.
        protected def netEnablingPredicates(t: TransitionId): List[Boolean] = List.empty

        final def isEnabled(t: TransitionId): Boolean = netEnablingPredicates(t).forall(identity)

        def semanticsErrors: List[Semantics.Error]

        final def validSemantics: Boolean = semanticsErrors.isEmpty
    }

    object Semantics {
        trait Error extends Net.Error
        object Error {
            case class MissingArcSemantics[ArcId](arcId: ArcId) extends Error

            case class MissingPlaceSemantics[PlaceId](placeId: PlaceId) extends Error

            case class MissingTransitionSemantics[TransitionId](transitionId: TransitionId)
                extends Error
        }
    }

    // =========================================================================
    // Presentation
    // =========================================================================

    object Presentation {
        trait Error extends Net.Error

        case class MissingArcPresentation[ArcId](arcId: ArcId) extends Error

        case class MissingPlacePresentation[PlaceId](placeId: PlaceId) extends Error

        case class MissingTransitionPresentation[TransitionId](transitionId: TransitionId)
            extends Error

        /** Component presentation accessor interface. This tells us the position, labels, and size
          * of the elements of the net.
          */
        trait Presentation[
            ArcId,
            PlaceId,
            TransitionId,
            A <: Arc.Presentation,
            P <: Place.Presentation,
            T <: Transition.Presentation
        ] {
            def getArcPresentation(id: ArcId): Either[MissingArcPresentation[ArcId], A]

            def getPlacePresentation(id: PlaceId): Either[MissingPlacePresentation[PlaceId], P]

            def getTransitionPresentation(
                id: TransitionId
            ): Either[MissingTransitionPresentation[TransitionId], T]

            def presentationErrors: List[Error] = List.empty

            final def isValidPresentation: Boolean = presentationErrors.isEmpty
        }

    }
}
