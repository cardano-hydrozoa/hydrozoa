package hydrozoa.lib.petri.net

import cats.data.NonEmptyList
import hydrozoa.lib.petri.net.Net.Topology.{MissingArcTopology, MissingPlaceTopology, MissingTransitionTopology}
import hydrozoa.lib.petri.net.components.*
import scala.annotation.unused

trait Net[
    ArcId,
    PlaceId,
    TransitionId,
    A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
    P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
    T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
    Self <: Net[ArcId, PlaceId, TransitionId, A, P, T, Self]
] extends Net.Ids[
      ArcId,
      PlaceId,
      TransitionId
    ],
      Net.Topology[ArcId, PlaceId, TransitionId, A, P, T],
      Net.Syntax[ArcId, PlaceId, TransitionId, A, P, T],
      Net.Semantics[ArcId, PlaceId, TransitionId, A, P, T]

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
        val arcIds: Set[ArcId]
        val placeIds: Set[PlaceId]
        val transitionIds: Set[TransitionId]
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

        case class MissingArcTopology[ArcId](arcId: ArcId) extends Error {
            override def getMessage: String = s"Missing arc topology for arc ID: $arcId"
        }

        case class MissingPlaceTopology[PlaceId](placeId: PlaceId) extends Error {
            override def getMessage: String = s"Missing place topology for place ID: $placeId"
        }

        case class MissingTransitionTopology[TransitionId](transitionId: TransitionId)
            extends Error {
            override def getMessage: String =
                s"Missing transition topology for transition ID: $transitionId"
        }

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
                    aid <- arcIds.toList
                    errors <- getArcTopology(aid) match {
                        case Left(missingArcTopology) => List(missingArcTopology)
                        case Right(arc) =>
                            val pid: PlaceId = arc.arcPlaceId
                            val tid: TransitionId = arc.arcTransitionId
                            val placeError = Option.when(!placeIds.contains(pid))(
                              NoDanglingArcs.Error.DanglingArcPlace[ArcId, PlaceId, TransitionId](
                                aid,
                                pid
                              )
                            )
                            val transitionError = Option.when(!transitionIds.contains(tid))(
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

                override def getMessage: String = this match
                    case DanglingArcPlace(arcId, placeId) =>
                        s"Arc $arcId references place $placeId which is not in the net"
                    case DanglingArcTransition(arcId, transitionId) =>
                        s"Arc $arcId references transition $transitionId which is not in the net"
        }

        /** Mixin: validates at most one arc per (place, transition) pair.
          *
          * Uses the abstract-override list-accumulation pattern. Must be linearised after
          * [[Topology]] so that `super.topologyErrors` is concrete.
          */
        trait SingleArc[
            ArcId,
            PlaceId,
            TransitionId,
            A <: Arc.Topology[PlaceId, TransitionId],
            P <: Place.Topology,
            T <: Transition.Topology
        ] extends Topology[ArcId, PlaceId, TransitionId, A, P, T] {

            abstract override def topologyErrors: List[this.TopologyValidationError] = {
                val eitherArcs
                    : List[Either[MissingArcTopology[ArcId], (ArcId, PlaceId, TransitionId)]] =
                    arcIds.toList.map(arcId =>
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
                    transitionId: TransitionId,
                ) extends Topology.Error {
                    override def getMessage: String =
                        s"Multiple arcs [${arcIds.toList.mkString(", ")}] connect place $placeId to transition $transitionId"
                }
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

        // Totality check: every ID registered in Net.Ids has a defined syntax entry.
        // For map-backed nets this is guaranteed by construction, so this will always be empty.
        final def syntaxErrors: List[Syntax.Error] =
            arcIds.toList.flatMap(getArcSyntax(_).swap.toOption) ++
                placeIds.toList.flatMap(getPlaceSyntax(_).swap.toOption) ++
                transitionIds.toList.flatMap(getTransitionSyntax(_).swap.toOption)

        final lazy val isValidSyntax: Boolean = syntaxErrors.isEmpty

        // The abstract-override mixin pattern (like Net.Topology's NoDanglingArcs / SingleArc)
        // could be used here for richer invariants beyond totality — for example:
        //   - every transition must have at least one connected arc
        //   - no two places may share the same label
        // To add constraints: override syntaxErrors with `super.syntaxErrors ++ myErrors`.
    }

    object Syntax {
        trait Error extends Net.Error

        case class MissingArcSyntax[ArcId](arcId: ArcId) extends Error {
            override def getMessage: String = s"No syntax entry for arc ID: $arcId"
        }

        case class MissingPlaceSyntax[PlaceId](placeId: PlaceId) extends Error {
            override def getMessage: String = s"No syntax entry for place ID: $placeId"
        }

        case class MissingTransitionSyntax[TransitionId](transitionId: TransitionId) extends Error {
            override def getMessage: String = s"No syntax entry for transition ID: $transitionId"
        }
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
        protected def netEnablingPredicates(@unused t: TransitionId): List[Boolean] = List.empty

        final def netEnablingPredicate(t: TransitionId): Boolean =
            netEnablingPredicates(t).forall(identity)

        // Totality check: every ID registered in Net.Ids has a defined semantics entry.
        // For map-backed nets this is guaranteed by construction, so this will always be empty.
        final def semanticsErrors: List[Semantics.Error] =
            arcIds.toList.flatMap(getArcSemantics(_).swap.toOption) ++
                placeIds.toList.flatMap(getPlaceSemantics(_).swap.toOption) ++
                transitionIds.toList.flatMap(getTransitionSemantics(_).swap.toOption)

        final lazy val isValidSemantics: Boolean = semanticsErrors.isEmpty

        // The abstract-override mixin pattern (like Net.Topology's NoDanglingArcs / SingleArc)
        // could be used here for richer invariants beyond totality — for example:
        //   - every transition must have at least one connected arc
        //   - every BoundedPlace must start below capacity
        // To add constraints: override semanticsErrors with `super.semanticsErrors ++ myErrors`.
    }

    object Semantics {
        trait Error extends Net.Error
        object Error {
            case class MissingArcSemantics[ArcId](arcId: ArcId) extends Error {
                override def getMessage: String = s"No semantics entry for arc ID: $arcId"
            }

            case class MissingPlaceSemantics[PlaceId](placeId: PlaceId) extends Error {
                override def getMessage: String = s"No semantics entry for place ID: $placeId"
            }

            case class MissingTransitionSemantics[TransitionId](transitionId: TransitionId)
                extends Error {
                override def getMessage: String =
                    s"No semantics entry for transition ID: $transitionId"
            }
        }

        /** Terminal-marking check, available on any net whose place type carries
          * [[Place.Syntax.HasFinalMarking]]. Not mixed into the base [[Net.Semantics]] trait — nets
          * that do not have a notion of final marking simply do not have these methods.
          *
          * A terminal state is valid when every place whose `finalMarking` is `Some(fm)` currently
          * holds exactly `fm` tokens. Places with `finalMarking = None` are unconstrained.
          */
        object FinalMarking {
            sealed trait Error extends Net.Error

            object Error {
                case class FinalMarkingNotReached[PlaceId, Marking](
                    placeId: PlaceId,
                    current: Marking,
                    expected: Marking,
                ) extends FinalMarking.Error {
                    override def getMessage: String =
                        s"Place $placeId has marking $current but final marking requires $expected"
                }
            }

            extension [
                ArcId,
                PlaceId,
                TransitionId,
                A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
                P <: Place.Topology & Place.Syntax.HasFinalMarking[P] & Place.Semantics[P],
                T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
                Self <: Net[ArcId, PlaceId, TransitionId, A, P, T, Self]
            ](net: Net[ArcId, PlaceId, TransitionId, A, P, T, Self])
                def terminalErrors: List[FinalMarking.Error] =
                    net.placeIds.toList.flatMap { pid =>
                        net.getPlaceSemantics(pid).toOption.toList.flatMap { place =>
                            place.finalMarking match
                                case None => Nil
                                case Some(fm) =>
                                    if place.marking == fm then Nil
                                    else List(Error.FinalMarkingNotReached(pid, place.marking, fm))
                        }
                    }

                def isValidTerminal: Boolean = terminalErrors.isEmpty
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
