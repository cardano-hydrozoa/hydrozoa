package hydrozoa.lib.petri.hlpn

import cats.implicits.*
import hydrozoa.lib.petri.net.*
import hydrozoa.lib.petri.net.components.{Arc, Place, Transition}

/** The HLPN arc value (ISO Concept 21): its annotation is an inscription term `W(f) ∈ TERM(O ∪ V)`,
  * evaluated under a mode at firing. Direction lives in the [[Arc.Flow]] key, exactly as in the P/T
  * framework.
  */
final case class InscribedArc[C](inscription: Inscription[C]) extends Arc.Syntax {
    type Annotation = Inscription[C]
    def annotation: Inscription[C] = inscription
}

/** The HLPN transition component (ISO Concept 21): the variables it binds (`V`) and its guard (`Φ :
  * T → TERM_B`). Fills the framework's transition-semantics stub.
  */
final case class HlTransition[C](variables: List[Var[C]], guard: Guard)
    extends Transition.Topology,
      Transition.Syntax,
      Transition.Semantics

/** A high-level Petri net over a color type `C`, as an instance of the `petri.net` framework:
  * places are [[ColoredPlace]]s, transitions are [[HlTransition]]s, and the arc map is keyed by
  * [[Arc.Flow]] elements — its key set is the flow relation `F` and the map is the annotation
  * function `W : F → TERM(O ∪ V)` (Concept 21). Structural validation (ids, dangling arcs,
  * totality) is inherited from [[Net]] / [[Net.Topology.NoDanglingArcs]].
  *
  * Simulation is *mode-relative* (Concepts 23/24), which is why this net does not extend the
  * framework's mode-less [[Simulator]]: [[fire]] takes the mode `β`, evaluates each connected
  * inscription under it, checks the guard `Φ⟦β⟧`, and applies the same per-place
  * `covers`/`minus`/`plus` core at the multiset [[MarkingAlgebra]] instance. Finding enabled modes
  * is a search strategy layered on top (unification, or a scenario-specific driver) — the net
  * offers only [[isModeEnabled]] and [[fire]].
  *
  * All places share the single type parameter `C`. A homogeneous net uses a concrete `C`; a
  * heterogeneous net — mixing base and product colors — is assembled by the typed [[NetBuilder]],
  * which checks arc↔place color agreement at compile time and instantiates `C = Any`. Each place's
  * real color domain is recovered from its `colorDomain` and enforced by `markingError`; the
  * multiset operations are parametric in `C`, so the net runs unchanged at `Any`.
  */
final case class HlNet[PlaceId, TransitionId, C](
    placesMap: Map[PlaceId, ColoredPlace[C]],
    transitionsMap: Map[TransitionId, HlTransition[C]],
    arcsMap: Map[Arc.Flow[PlaceId, TransitionId], InscribedArc[C]]
) extends Net[
      PlaceId,
      TransitionId,
      InscribedArc[C],
      ColoredPlace[C],
      HlTransition[C],
      HlNet[PlaceId, TransitionId, C]
    ],
      Net.Topology.NoDanglingArcs[PlaceId, TransitionId, ColoredPlace[C], HlTransition[C]] {

    import HlNet.FiringError

    // ---------------------------------------------------------------------------
    // Net.Ids
    // ---------------------------------------------------------------------------

    override val placeIds: Set[PlaceId] = placesMap.keySet
    override val transitionIds: Set[TransitionId] = transitionsMap.keySet
    override val flowRelation: Set[Arc.Flow[PlaceId, TransitionId]] = arcsMap.keySet

    // ---------------------------------------------------------------------------
    // Net.Topology (via NoDanglingArcs mixin chain)
    // ---------------------------------------------------------------------------

    def getPlaceTopology(
        placeId: PlaceId
    ): Either[Net.Topology.MissingPlaceTopology[PlaceId], ColoredPlace[C]] =
        placesMap.get(placeId).toRight(Net.Topology.MissingPlaceTopology(placeId))

    def getTransitionTopology(
        transitionId: TransitionId
    ): Either[Net.Topology.MissingTransitionTopology[TransitionId], HlTransition[C]] =
        transitionsMap
            .get(transitionId)
            .toRight(Net.Topology.MissingTransitionTopology(transitionId))

    override def topologyErrors: List[Net.Topology.Error] = super.topologyErrors

    // ---------------------------------------------------------------------------
    // Net.Syntax
    // ---------------------------------------------------------------------------

    def getArcSyntax(
        flow: Arc.Flow[PlaceId, TransitionId]
    ): Either[Net.Syntax.MissingArcSyntax[PlaceId, TransitionId], InscribedArc[C]] =
        arcsMap.get(flow).toRight(Net.Syntax.MissingArcSyntax(flow))

    def getPlaceSyntax(
        id: PlaceId
    ): Either[Net.Syntax.MissingPlaceSyntax[PlaceId], ColoredPlace[C]] =
        placesMap.get(id).toRight(Net.Syntax.MissingPlaceSyntax(id))

    def getTransitionSyntax(
        id: TransitionId
    ): Either[Net.Syntax.MissingTransitionSyntax[TransitionId], HlTransition[C]] =
        transitionsMap.get(id).toRight(Net.Syntax.MissingTransitionSyntax(id))

    // ---------------------------------------------------------------------------
    // Net.Semantics
    // ---------------------------------------------------------------------------

    override def getPlaceSemantics(
        id: PlaceId
    ): Either[Net.Semantics.Error.MissingPlaceSemantics[PlaceId], ColoredPlace[C]] =
        placesMap.get(id).toRight(Net.Semantics.Error.MissingPlaceSemantics(id))

    override def getTransitionSemantics(
        id: TransitionId
    ): Either[Net.Semantics.Error.MissingTransitionSemantics[TransitionId], HlTransition[C]] =
        transitionsMap.get(id).toRight(Net.Semantics.Error.MissingTransitionSemantics(id))

    // ---------------------------------------------------------------------------
    // Mode-relative simulation (Concepts 23/24)
    // ---------------------------------------------------------------------------

    /** The current marking of a place. Assumes it exists — a validated net has no dangling arcs. */
    def marking(pid: PlaceId): MultiSet[C] = placesMap(pid).marking

    /** Whether `mode` enables `tid` (test-fire, capturing the guard, the enabling rule, the Fε
      * seam, and place validity in one pass).
      */
    def isModeEnabled(tid: TransitionId, mode: Binding): Boolean = fire(tid, mode).isRight

    /** Fire `tid` under `mode` (Concept 24): check `Φ⟦mode⟧`, evaluate each connected inscription
      * under `mode`, then per connected place `M′(p) = M(p) − W(p,t)⟦mode⟧ + W(t,p)⟦mode⟧`, failing
      * if an input annotation is not covered (Concept 23) or an updated place violates its marking
      * invariants.
      */
    def fire(
        tid: TransitionId,
        mode: Binding
    ): Either[FiringError, HlNet[PlaceId, TransitionId, C]] =
        transitionsMap.get(tid) match {
            case None => Left(FiringError.TransitionNotFound(tid))
            case Some(transition) =>
                if !netEnablingPredicate(tid) then Left(FiringError.NotEnabled(tid))
                else
                    for {
                        guardHolds <- Binding
                            .evalGuard(transition.guard, mode)
                            .leftMap(FiringError.EvalFailed(tid, _))
                        _ <- Either.cond(guardHolds, (), FiringError.NotEnabled(tid))
                        evaluated <- arcsMap.toList
                            .filter((flow, _) => flow.transition == tid)
                            .traverse { (flow, arc) =>
                                Binding
                                    .evalInscription(arc.inscription, mode)
                                    .bimap(FiringError.EvalFailed(tid, _), flow -> _)
                            }
                        updates <- firedPlaces(evaluated)
                    } yield copy(placesMap = placesMap ++ updates)
        }

    // Concept 24 at the multiset algebra, per connected place. Each place has at most one arc per
    // direction (W is a function on F), so the folds apply at most one annotation each.
    private def firedPlaces(
        evaluated: List[(Arc.Flow[PlaceId, TransitionId], MultiSet[C])]
    ): Either[FiringError, List[(PlaceId, ColoredPlace[C])]] = {
        val algebra = multisetAlgebra[C]
        val placeIdsInArcOrder = evaluated.map((flow, _) => flow.place).distinct
        placeIdsInArcOrder.traverse { placeId =>
            val connected = evaluated.filter((flow, _) => flow.place == placeId)
            val (inputs, outputs) = connected.partition { (flow, _) =>
                flow match {
                    case _: Arc.Flow.Pt[?, ?] => true
                    case _: Arc.Flow.Tp[?, ?] => false
                }
            }
            for {
                place <- placesMap
                    .get(placeId)
                    .toRight(FiringError.DanglingArc(connected.head._1))
                afterInputs <- inputs.foldLeftM(place.marking) { case (m, (flow, w)) =>
                    Either.cond(
                      algebra.covers(m, w),
                      algebra.minus(m, w),
                      FiringError.ArcNotEnabled(flow, m, w)
                    )
                }
                afterOutputs = outputs.foldLeft(afterInputs) { case (m, (_, w)) =>
                    algebra.plus(m, w)
                }
                updated = place.mark(afterOutputs)
                _ <- updated.markingError
                    .map(FiringError.PlaceInvalid(placeId, _))
                    .toLeft(())
            } yield placeId -> updated
        }
    }
}

object HlNet {

    /** Why firing failed. As in the framework, every case is an enabling-side failure or a
      * validation bug — firing an enabled transition cannot fail.
      */
    sealed trait FiringError

    object FiringError {

        /** The requested transition ID is not present in the net. */
        final case class TransitionNotFound[TransitionId](transition: TransitionId)
            extends FiringError

        /** The guard `Φ⟦mode⟧` or a net-level enabling predicate (Fε) is false. */
        final case class NotEnabled[TransitionId](transition: TransitionId) extends FiringError

        /** Input arc `flow` failed the enabling rule: the place's marking does not cover the
          * evaluated annotation (Concept 23).
          */
        final case class ArcNotEnabled[PlaceId, TransitionId, C](
            flow: Arc.Flow[PlaceId, TransitionId],
            marking: MultiSet[C],
            annotation: MultiSet[C],
        ) extends FiringError

        /** A guard or inscription failed to evaluate under the mode (e.g. an unbound variable — the
          * mode is incomplete).
          */
        final case class EvalFailed[TransitionId](transition: TransitionId, error: EvalError)
            extends FiringError

        /** Flow element `flow` references a place absent from the net — validate before simulating.
          */
        final case class DanglingArc[PlaceId, TransitionId](
            flow: Arc.Flow[PlaceId, TransitionId]
        ) extends FiringError

        /** Place `placeId` violates its own marking invariants after firing. */
        final case class PlaceInvalid[PlaceId](
            place: PlaceId,
            error: Place.Semantics.MarkingError
        ) extends FiringError
    }
}
