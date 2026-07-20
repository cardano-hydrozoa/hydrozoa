package hydrozoa.lib.petri.net

import hydrozoa.lib.petri.net.Net.Topology.{MissingPlaceTopology, MissingTransitionTopology}
import hydrozoa.lib.petri.net.components.*
import scala.annotation.unused

trait Net[
    PlaceId,
    TransitionId,
    A <: Arc.Syntax,
    P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
    T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
    Self <: Net[PlaceId, TransitionId, A, P, T, Self]
] extends Net.Ids[PlaceId, TransitionId],
      Net.Topology[PlaceId, TransitionId, P, T],
      Net.Syntax[PlaceId, TransitionId, A, P, T],
      Net.Semantics[PlaceId, TransitionId, P, T]

object Net {

    trait Error extends Throwable

    /** The authoritative structure of the net (ISO Concept 1): the place set, the transition set,
      * and the flow relation `F`. Any data that must be associated with every element (annotations,
      * semantics, presentation) must be a total mapping over these sets, and an association whose
      * key is absent from them is invalid.
      */
    trait Ids[PlaceId, TransitionId] {
        val placeIds: Set[PlaceId]
        val transitionIds: Set[TransitionId]

        /** `F ⊆ (P×T) ∪ (T×P)` — the arcs of the net. */
        val flowRelation: Set[Arc.Flow[PlaceId, TransitionId]]
    }

    // =========================================================================
    // Topology
    // =========================================================================

    /** Structural query and validation interface for a net. `topologyErrors` is abstract — topology
      * constraints are always explicit. Concrete nets implement it either directly or by composing
      * the mixin traits from [[Topology]]:
      *   - [[Topology.NoDanglingArcs]]: no flow element references a missing place/transition ID
      *
      * There is no multiplicity constraint: an arc's identity is its `F`-element, so `W` is a
      * function on `F` (Concept 8) by construction.
      */
    trait Topology[
        PlaceId,
        TransitionId,
        P <: Place.Topology,
        T <: Transition.Topology
    ] extends Net.Ids[PlaceId, TransitionId] {
        type TopologyValidationError = Net.Topology.Error

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

        case class MissingPlaceTopology[PlaceId](placeId: PlaceId) extends Error {
            override def getMessage: String = s"Missing place topology for place ID: $placeId"
        }

        case class MissingTransitionTopology[TransitionId](transitionId: TransitionId)
            extends Error {
            override def getMessage: String =
                s"Missing transition topology for transition ID: $transitionId"
        }

        /** Mixin: validates that no flow element references a place or transition ID absent from
          * the net.
          *
          * Uses the abstract-override list-accumulation pattern. Must be linearised after
          * [[Topology]] so that `super.topologyErrors` is concrete.
          */
        trait NoDanglingArcs[
            PlaceId,
            TransitionId,
            P <: Place.Topology,
            T <: Transition.Topology
        ] extends Topology[PlaceId, TransitionId, P, T] {
            abstract override def topologyErrors: List[this.TopologyValidationError] = {
                val danglingErrors = flowRelation.toList.flatMap { flow =>
                    val placeError = Option.when(!placeIds.contains(flow.place))(
                      NoDanglingArcs.Error.DanglingArcPlace(flow)
                    )
                    val transitionError = Option.when(!transitionIds.contains(flow.transition))(
                      NoDanglingArcs.Error.DanglingArcTransition(flow)
                    )
                    placeError.toList ++ transitionError.toList
                }
                danglingErrors ++ super.topologyErrors
            }
        }

        object NoDanglingArcs {
            enum Error[PlaceId, TransitionId] extends Net.Topology.Error:
                case DanglingArcPlace(flow: Arc.Flow[PlaceId, TransitionId])
                case DanglingArcTransition(flow: Arc.Flow[PlaceId, TransitionId])

                override def getMessage: String = this match
                    case DanglingArcPlace(flow) =>
                        s"Arc $flow references place ${flow.place} which is not in the net"
                    case DanglingArcTransition(flow) =>
                        s"Arc $flow references transition ${flow.transition} which is not in the net"
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
    trait Syntax[PlaceId, TransitionId, A <: Arc.Syntax, P <: Place.Syntax[
      P
    ], T <: Transition.Syntax]
        extends Net.Ids[PlaceId, TransitionId] {

        import Syntax.*

        /** `W(f)`: the annotation of flow element `f`. Total over [[Ids.flowRelation]], making the
          * accessor exactly the annotation function `W : F → AN` (Concept 8).
          */
        def getArcSyntax(
            flow: Arc.Flow[PlaceId, TransitionId]
        ): Either[MissingArcSyntax[PlaceId, TransitionId], A]

        def getPlaceSyntax(id: PlaceId): Either[MissingPlaceSyntax[PlaceId], P]

        def getTransitionSyntax(
            id: TransitionId
        ): Either[MissingTransitionSyntax[TransitionId], T]

        // Totality check: every flow element and ID registered in Net.Ids has a defined syntax
        // entry. For map-backed nets this is guaranteed by construction, so this will always be
        // empty.
        final def syntaxErrors: List[Syntax.Error] =
            flowRelation.toList.flatMap(getArcSyntax(_).swap.toOption) ++
                placeIds.toList.flatMap(getPlaceSyntax(_).swap.toOption) ++
                transitionIds.toList.flatMap(getTransitionSyntax(_).swap.toOption)

        final lazy val isValidSyntax: Boolean = syntaxErrors.isEmpty

        // The abstract-override mixin pattern (like Net.Topology's NoDanglingArcs)
        // could be used here for richer invariants beyond totality — for example:
        //   - every transition must have at least one connected arc
        //   - no two places may share the same label
        // To add constraints: override syntaxErrors with `super.syntaxErrors ++ myErrors`.
    }

    object Syntax {
        trait Error extends Net.Error

        case class MissingArcSyntax[PlaceId, TransitionId](
            flow: Arc.Flow[PlaceId, TransitionId]
        ) extends Error {
            override def getMessage: String = s"No annotation entry for arc: $flow"
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

    /** Component semantics accessor interface for places and transitions. Arcs carry no semantics —
      * an arc is its annotation ([[Arc.Syntax]]); enabling and firing are net-level rules
      * implemented by the simulator.
      */
    trait Semantics[PlaceId, TransitionId, P <: Place.Syntax[
      P
    ] & Place.Semantics[P], T <: Transition.Semantics]
        extends Net.Ids[PlaceId, TransitionId] {
        import Semantics.Error.*

        def getPlaceSemantics(id: PlaceId): Either[MissingPlaceSemantics[PlaceId], P]

        def getTransitionSemantics(
            id: TransitionId
        ): Either[MissingTransitionSemantics[TransitionId], T]

        // The filtering-function seam (ISO 15909-3, 5.3.4): net-level enabling conjuncts Fε for
        // transition t, composed under AND with the base enabling rule. Override (with abstract
        // override) to add enrichment conditions; empty = no enrichments.
        protected def netEnablingPredicates(@unused t: TransitionId): List[Boolean] = List.empty

        final def netEnablingPredicate(t: TransitionId): Boolean =
            netEnablingPredicates(t).forall(identity)

        // Totality check: every ID registered in Net.Ids has a defined semantics entry.
        // For map-backed nets this is guaranteed by construction, so this will always be empty.
        final def semanticsErrors: List[Semantics.Error] =
            placeIds.toList.flatMap(getPlaceSemantics(_).swap.toOption) ++
                transitionIds.toList.flatMap(getTransitionSemantics(_).swap.toOption)

        final lazy val isValidSemantics: Boolean = semanticsErrors.isEmpty

        // The abstract-override mixin pattern (like Net.Topology's NoDanglingArcs)
        // could be used here for richer invariants beyond totality — for example:
        //   - every transition must have at least one connected arc
        //   - every BoundedPlace must start below capacity
        // To add constraints: override semanticsErrors with `super.semanticsErrors ++ myErrors`.
    }

    object Semantics {
        trait Error extends Net.Error
        object Error {
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
                PlaceId,
                TransitionId,
                A <: Arc.Syntax,
                P <: Place.Topology & Place.Syntax.HasFinalMarking[P] & Place.Semantics[P],
                T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
                Self <: Net[PlaceId, TransitionId, A, P, T, Self]
            ](net: Net[PlaceId, TransitionId, A, P, T, Self])
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

        case class MissingArcPresentation[PlaceId, TransitionId](
            flow: Arc.Flow[PlaceId, TransitionId]
        ) extends Error

        case class MissingPlacePresentation[PlaceId](placeId: PlaceId) extends Error

        case class MissingTransitionPresentation[TransitionId](transitionId: TransitionId)
            extends Error

        /** Component presentation accessor interface. This tells us the position, labels, and size
          * of the elements of the net.
          */
        trait Presentation[
            PlaceId,
            TransitionId,
            A <: Arc.Presentation,
            P <: Place.Presentation,
            T <: Transition.Presentation
        ] {
            def getArcPresentation(
                flow: Arc.Flow[PlaceId, TransitionId]
            ): Either[MissingArcPresentation[PlaceId, TransitionId], A]

            def getPlacePresentation(id: PlaceId): Either[MissingPlacePresentation[PlaceId], P]

            def getTransitionPresentation(
                id: TransitionId
            ): Either[MissingTransitionPresentation[TransitionId], T]

            def presentationErrors: List[Error] = List.empty

            final def isValidPresentation: Boolean = presentationErrors.isEmpty
        }

    }
}
