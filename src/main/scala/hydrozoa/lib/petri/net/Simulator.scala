package hydrozoa.lib.petri.net

import cats.Monad
import cats.data.StateT
import hydrozoa.lib.petri.net.components.*

/** Abstract simulation interface for a net. Extends [[Net]] with the ability to fire transitions
  * and query enabledness.
  *
  * [[Net.Semantics]] describes the net's *configuration* (place/transition semantics, the
  * filtering-function seam). [[Simulator]] implements the ISO 15909-1 *rules*: the enabling rule
  * (Concept 6/10) and the firing rule (Concept 7/12). The two are separate so that the same net
  * structure can be paired with different simulators — pure vs. effectful, sequential vs. parallel.
  *
  * [[isEnabled]] is derived as `fire(t).isRight` (test-fire approach), so it captures every
  * enabling condition — including place validity, which thereby acts as an enabling filter (the
  * capacity-enrichment behavior of ISO 15909-3, 5.2.5) — in a single pass.
  *
  * ## Monadic simulation
  *
  * [[Simulator.Sim]] is the opaque simulation monad: `StateT[Either[FiringError, _], Self, A]`. It
  * threads net state automatically and short-circuits on the first firing error. Use
  * [[Simulator.Sim.fire]], [[Simulator.Sim.inspect]], [[Simulator.Sim.isEnabled]], and
  * [[Simulator.Sim.enabledTransitions]] to build programs; [[Simulator.Sim.run]] to execute them.
  */
trait Simulator[
    PlaceId,
    TransitionId,
    A <: Arc.Syntax,
    P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
    T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
    Self <: Simulator[PlaceId, TransitionId, A, P, T, Self]
] extends Net[PlaceId, TransitionId, A, P, T, Self] {

    /** Fire transition `t` (ISO Concept 7/12). Checks, in order:
      *   1. Transition exists in the net
      *   2. [[Net.Semantics.netEnablingPredicate]] holds (the filtering-function seam, Fε)
      *   3. The enabling rule holds for every input arc (`M(p) ≥ W(p,t)`, Concept 10)
      *   4. Every updated place satisfies its own marking invariants
      *      ([[Place.Semantics.validMarking]])
      *
      * On success the marking update is `M′(p) = M(p) − W(p,t) + W(t,p)` per connected place
      * (Concept 12).
      */
    def fire(t: TransitionId): Either[Simulator.FiringError, Self]

    /** @throws Simulator.FiringError if firing fails */
    @throws[Simulator.FiringError]("if firing fails")
    final def fireUnsafe(t: TransitionId): Self = fire(t).fold(throw _, identity)

    // Enabling is defined as: fire would succeed (test-fire approach), capturing all enabling
    // conditions in a single pass.
    final def isEnabled(t: TransitionId): Boolean = fire(t).isRight

    final def enabledTransitions: Set[TransitionId] = transitionIds.filter(isEnabled)
}

object Simulator {
    trait Error extends Net.Error

    /** Errors that can arise when firing a transition. Not sealed; extend for custom error cases.
      *
      * In ISO 15909-1 firing an *enabled* transition cannot fail (Concept 7 has no failure path),
      * so every case here is an enabling-side failure or a validation bug — there are no
      * post-enabling arithmetic errors.
      */
    trait FiringError extends Error

    object FiringError {

        /** The requested transition ID is not present in the net. */
        case class TransitionNotFound[TransitionId](transitionId: TransitionId)
            extends FiringError {
            override def getMessage: String = s"Transition not found: $transitionId"
        }

        /** Input arc `flow` failed the enabling rule: the place's marking does not cover the arc's
          * annotation (`M(p) ≥ W(p,t)` is false, Concept 10).
          */
        case class ArcNotEnabled[PlaceId, TransitionId, W, M](
            flow: Arc.Flow[PlaceId, TransitionId],
            marking: M,
            annotation: W,
        ) extends FiringError {
            override def getMessage: String =
                s"Input arc $flow is not enabled: marking $marking does not cover annotation $annotation"
        }

        /** Place `placeId` violates its own marking invariants after firing. */
        case class PlaceValidityViolated[PlaceId](
            placeId: PlaceId,
            cause: Place.Semantics.MarkingError,
        ) extends FiringError {
            override def getMessage: String =
                s"Place $placeId has invalid marking after firing: ${cause.getMessage}"
        }

        /** [[Net.Semantics.netEnablingPredicate]] returned false for the transition. */
        case class NetEnablingFailed[TransitionId](transitionId: TransitionId) extends FiringError {
            override def getMessage: String =
                s"Net-level enabling predicate failed for transition: $transitionId"
        }
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
            PlaceId,
            TransitionId,
            A <: Arc.Syntax,
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
            S <: Simulator[PlaceId, TransitionId, A, P, T, S]
        ](t: TransitionId): Sim[S, Unit] =
            StateT.modifyF(_.fire(t))

        /** Read-only access to the current net state. The only way to observe `S` from within a
          * [[Sim]] program.
          */
        def inspect[S, A](f: S => A): Sim[S, A] = StateT.inspect(f)

        def isEnabled[
            PlaceId,
            TransitionId,
            A <: Arc.Syntax,
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
            S <: Simulator[PlaceId, TransitionId, A, P, T, S]
        ](t: TransitionId): Sim[S, Boolean] =
            StateT.inspect(_.isEnabled(t))

        def enabledTransitions[
            PlaceId,
            TransitionId,
            A <: Arc.Syntax,
            P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
            T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
            S <: Simulator[PlaceId, TransitionId, A, P, T, S]
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
