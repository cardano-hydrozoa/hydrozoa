package hydrozoa.lib.petri.net

import cats.Monad
import cats.data.{NonEmptyList, StateT}
import cats.implicits.*
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
  * ## Monadic simulation
  *
  * [[Simulator.Sim]] is the opaque simulation monad: `StateT[Either[FiringError, _], Self, A]`. It
  * threads net state automatically and short-circuits on the first firing error. Use
  * [[Simulator.Sim.fire]], [[Simulator.Sim.inspect]], [[Simulator.Sim.isEnabled]], and
  * [[Simulator.Sim.enabledTransitions]] to build programs; [[Simulator.Sim.run]] to execute them.
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

        /** Arc `arcId` connected to place `placeId` failed one or more enabling checks (E2). */
        case class ArcNotEnabled[ArcId, PlaceId](
            arcId: ArcId,
            placeId: PlaceId,
            errors: NonEmptyList[Arc.Semantics.EnablingError]
        ) extends FiringError

        /** Arc `arcId`'s firing endo returned a [[Arc.Semantics.FiringError]]. */
        case class ArcFiringFailed[ArcId](arcId: ArcId, cause: Arc.Semantics.FiringError)
            extends FiringError

        /** Place `placeId` has one or more invalid marking constraints after firing (E1). */
        case class PlaceValidityViolated[PlaceId](
            placeId: PlaceId,
            cause: NonEmptyList[Place.Semantics.MarkingError]
        ) extends FiringError

        /** [[Net.Semantics.netEnablingPredicate]] returned false for the transition. */
        case class NetEnablingFailed[TransitionId](transitionId: TransitionId) extends FiringError
    }

    // =========================================================================
    // Sim — opaque simulation monad
    // =========================================================================

    /** Opaque simulation monad: `StateT[Either[FiringError, _], S, A]`.
      *
      * State `S` can only be modified via [[Sim.fire]]; all other operations are read-only. This
      * prevents callers from constructing arbitrary net states outside of legitimate firings.
      *
      * Programs are built from [[Sim.fire]], [[Sim.inspect]], [[Sim.isEnabled]], and
      * [[Sim.enabledTransitions]], composed via `map` / `flatMap` (or for-comprehensions with
      * `import cats.syntax.all.*`), and executed with [[Sim.run]] or [[Sim.runS]].
      */
    opaque type Sim[S, A] = StateT[[X] =>> Either[Simulator.FiringError, X], S, A]

    object Sim {

        // ----- Constructors --------------------------------------------------

        def pure[S, A](a: A): Sim[S, A] = StateT.pure(a)

        /** Lift a single transition firing into the monad. Threads the updated net state
          * automatically; short-circuits with a [[Simulator.FiringError]] on failure.
          */
        def fire[
            ArcId: Ordering,
            PlaceId,
            TransitionId,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
            S <: Simulator[ArcId, PlaceId, TransitionId, A, P, T, S]
        ](t: TransitionId): Sim[S, Unit] =
            StateT.modifyF(_.fire(t))

        /** Read-only access to the current net state. The only way to observe `S` from within a
          * [[Sim]] program.
          */
        def inspect[S, A](f: S => A): Sim[S, A] = StateT.inspect(f)

        def isEnabled[
            ArcId: Ordering,
            PlaceId,
            TransitionId,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
            S <: Simulator[ArcId, PlaceId, TransitionId, A, P, T, S]
        ](t: TransitionId): Sim[S, Boolean] =
            StateT.inspect(_.isEnabled(t))

        def enabledTransitions[
            ArcId: Ordering,
            PlaceId,
            TransitionId,
            A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
            S <: Simulator[ArcId, PlaceId, TransitionId, A, P, T, S]
        ]: Sim[S, Set[TransitionId]] =
            StateT.inspect(_.enabledTransitions)

        // ----- Eliminators ---------------------------------------------------

        extension [S, A](sim: Sim[S, A])

            /** Run the program from `initial`, returning the final net state and result value. */
            def run(initial: S): Either[Simulator.FiringError, (S, A)] =
                (sim: StateT[[X] =>> Either[Simulator.FiringError, X], S, A]).run(initial)

            /** Run the program, returning only the final net state. */
            def runS(initial: S): Either[Simulator.FiringError, S] =
                (sim: StateT[[X] =>> Either[Simulator.FiringError, X], S, A]).runS(initial)

            /** Run the program, returning only the result value. */
            def runA(initial: S): Either[Simulator.FiringError, A] =
                (sim: StateT[[X] =>> Either[Simulator.FiringError, X], S, A]).runA(initial)

        // ----- Cats type class instance ---------------------------------------

        /** `Monad` instance for `Sim[S, _]`. Import `cats.syntax.all.*` to get `map`, `flatMap`,
          * `traverse`, etc. on `Sim` values.
          */
        // Call the cats StateT instance by name to avoid circular implicit resolution:
        // inside object Sim, Sim[S, A] is transparent as StateT[...], so summon would
        // find this very given and loop. Named method call bypasses that search.
        given [S]: Monad[[A] =>> Sim[S, A]] =
            cats.data.IndexedStateT.catsDataMonadForIndexedStateT[
              [X] =>> Either[Simulator.FiringError, X],
              S
            ]
    }
}
