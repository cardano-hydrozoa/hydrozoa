package hydrozoa.lib.petri.net

import hydrozoa.lib.petri.net.components.*

/** Abstract simulation interface for a net. Extends [[Net]] with the ability to fire transitions
  * and query enabledness.
  *
  * [[Net.Semantics]] describes _what conditions must hold_ for a transition to fire
  * (configuration). [[Simulator]] describes _how_ firing is performed (evaluation). The two are
  * separate so that the same net structure can be paired with different simulators — pure vs.
  * effectful, sequential vs. parallel.
  *
  * ## Firing semantics
  *
  * [[fire]] enforces all enabling conditions:
  *   - Net-level: [[Net.Semantics.netEnablingPredicate]]
  *   - E2 (arc-side): [[Arc.Semantics.enabled]] for every arc connected to the transition
  *   - E1 (place-side): [[Place.Semantics.validMarking]] for every place after firing endos are
  *     applied
  *
  * [[isEnabled]] is derived as `fire(t).isRight` (test-fire approach), so it captures all three
  * levels of enabling in a single pass.
  *
  * ## Effectful simulation (future)
  *
  * This trait is the pure, synchronous variant. Effectful simulation (e.g., `IO`-based or
  * `StateT`-based) will be encoded in a separate trait parameterised on an effect type `F[_]` so
  * that the effect type does not leak into [[Net]]'s type signature.
  */
trait Simulator[
    ArcId,
    PlaceId,
    TransitionId,
    A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
    P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
    T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
    Self <: Simulator[ArcId, PlaceId, TransitionId, A, P, T, Self]
] extends Net[ArcId, PlaceId, TransitionId, A, P, T, Self] {

    /** Fire transition `t`. Checks, in order:
      *   1. Transition exists in the net
      *   2. [[Net.Semantics.netEnablingPredicate]] holds (net-level conditions)
      *   3. [[Arc.Semantics.enabled]] holds for every arc connected to `t` (E2)
      *   4. [[Arc.Semantics.fire]] succeeds for every arc connected to `t`
      *   5. [[Place.Semantics.validMarking]] holds for every updated place (E1)
      *
      * All arc/place pairs are snapshotted before any firing endo is applied, so every endo is
      * computed against the pre-fire state.
      */
    def fire(t: TransitionId): Either[Simulator.FiringError, Self]

    /** @throws Simulator.FiringError if firing fails */
    @throws[Simulator.FiringError]("if firing fails")
    final def fireUnsafe(t: TransitionId): Self = fire(t).fold(throw _, identity)

    // Enabling is defined as: fire would succeed (test-fire approach).
    // Arc-side enabling (E2) and place validity (E1) are both checked inside fire,
    // so this captures all enabling conditions in a single pass.
    final def isEnabled(t: TransitionId): Boolean = fire(t).isRight

    final def enabledTransitions: Set[TransitionId] = transitionIds.filter(isEnabled)
}

object Simulator {
    trait Error extends Net.Error

    /** Errors that can arise when firing a transition. Not sealed; extend for custom error cases.
      */
    trait FiringError extends Error

    object FiringError {

        /** The requested transition ID is not present in the net. */
        case class TransitionNotFound[TransitionId](transitionId: TransitionId) extends FiringError

        /** Arc `arcId` connected to place `placeId` has a false enabling predicate (E2). */
        case class ArcNotEnabled[ArcId, PlaceId](arcId: ArcId, placeId: PlaceId) extends FiringError

        /** Arc `arcId`'s firing endo returned a [[Arc.Semantics.FiringError]]. */
        case class ArcFiringFailed[ArcId](arcId: ArcId, cause: Arc.Semantics.FiringError)
            extends FiringError

        /** Place `placeId` has an invalid marking after firing (E1). */
        case class PlaceValidityViolated[PlaceId](
            placeId: PlaceId,
            cause: Place.Semantics.MarkingError
        ) extends FiringError

        /** [[Net.Semantics.netEnablingPredicate]] returned false for the transition. */
        case class NetEnablingFailed[TransitionId](transitionId: TransitionId) extends FiringError
    }
}
