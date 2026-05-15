package hydrozoa.lib.petri.net

import cats.data.NonEmptyList
import hydrozoa.lib.petri.net.components.*

/** An opaque refinement of a [[Simulator]] instance that has passed all validity checks.
  *
  * ## Invariants (assuming law-abiding implementations)
  *
  *   1. **Construction**: [[ValidatedSimulator.validate]] is the only way to produce a
  *      `ValidatedSimulator[S]`; it checks [[Net.Topology.topologyErrors]],
  *      [[Net.Syntax.syntaxErrors]], and [[Net.Semantics.semanticsErrors]] before wrapping.
  *   2. **Preservation under firing**: [[ValidatedSimulator.fire]] can only return a new
  *      `ValidatedSimulator[S]` or fail with a [[Simulator.FiringError]]. The new wrapped value is
  *      guaranteed valid because firing only updates place markings (via `withUpdatedPlaces`) — it
  *      never changes ID sets, arc structure, or transition structure. Therefore:
  *      - Topology validity is preserved: no dangling arcs or duplicate arcs are introduced.
  *      - Syntax validity is preserved: ID sets are unchanged.
  *      - Semantics validity is preserved: all IDs still have entries; updated places remain in the
  *        semantics map.
  *
  * ## Usage
  *
  * {{{
  * val net: MapNet[...] = ...
  * val validated: Either[NonEmptyList[ValidatedSimulator.ValidationError], ValidatedSimulator[MapNet[...]]] =
  *     ValidatedSimulator.validate(net)
  *
  * val result: Either[Simulator.FiringError, ValidatedSimulator[MapNet[...]]] =
  *     validated.flatMap(_.fire(someTransition))
  * }}}
  */
opaque type ValidatedSimulator[S] = S

object ValidatedSimulator {

    // =========================================================================
    // Validation errors
    // =========================================================================

    /** Errors produced by [[validate]] when the net fails one or more validity checks. All violated
      * constraints are collected before returning, so callers see the full picture in a single call
      * rather than failing at the first violation.
      */
    sealed trait ValidationError extends Net.Error

    object ValidationError {

        /** One or more topology constraints were violated (e.g. dangling arcs, duplicate arcs). See
          * [[Net.Topology.topologyErrors]] for full error definitions.
          */
        case class TopologyInvalid(errors: NonEmptyList[Net.Topology.Error])
            extends ValidationError {
            override def getMessage: String =
                s"Net topology is invalid: ${errors.toList.map(_.getMessage).mkString("; ")}"
        }

        /** One or more arc / place / transition IDs have no defined syntax entry. */
        case class SyntaxInvalid(errors: NonEmptyList[Net.Syntax.Error]) extends ValidationError {
            override def getMessage: String =
                s"Net syntax is invalid: ${errors.toList.map(_.getMessage).mkString("; ")}"
        }

        /** One or more arc / place / transition IDs have no defined semantics entry. */
        case class SemanticsInvalid(errors: NonEmptyList[Net.Semantics.Error])
            extends ValidationError {
            override def getMessage: String =
                s"Net semantics are invalid: ${errors.toList.map(_.getMessage).mkString("; ")}"
        }
    }

    // =========================================================================
    // Smart constructor
    // =========================================================================

    /** Validate `s` and wrap it if all checks pass. Returns [[scala.util.Left]] with every violated
      * constraint if any check fails; [[scala.util.Right]] with the wrapped simulator otherwise.
      */
    def validate[
        ArcId: Ordering,
        PlaceId,
        TransitionId,
        A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
        P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
        T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
        S <: Simulator[ArcId, PlaceId, TransitionId, A, P, T, S]
    ](s: S): Either[NonEmptyList[ValidationError], ValidatedSimulator[S]] = {
        val errors: List[ValidationError] =
            NonEmptyList
                .fromList(s.topologyErrors)
                .map(ValidationError.TopologyInvalid.apply)
                .toList ++
                NonEmptyList
                    .fromList(s.syntaxErrors)
                    .map(ValidationError.SyntaxInvalid.apply)
                    .toList ++
                NonEmptyList
                    .fromList(s.semanticsErrors)
                    .map(ValidationError.SemanticsInvalid.apply)
                    .toList
        NonEmptyList.fromList(errors).toLeft(s)
    }

    // =========================================================================
    // Operations
    // =========================================================================

    extension [
        ArcId: Ordering,
        PlaceId,
        TransitionId,
        A <: Arc.Topology[PlaceId, TransitionId] & Arc.Syntax & Arc.Semantics[P],
        P <: Place.Topology & Place.Syntax[P] & Place.Semantics[P],
        T <: Transition.Topology & Transition.Syntax & Transition.Semantics,
        S <: Simulator[ArcId, PlaceId, TransitionId, A, P, T, S]
    ](vs: ValidatedSimulator[S])

        /** Fire transition `t`. On success, the returned [[ValidatedSimulator]] is guaranteed valid
          * by the firing-preservation invariant (see type-level Scaladoc): firing only updates
          * place markings and cannot introduce topology, syntax, or semantics violations.
          */
        def fire(t: TransitionId): Either[Simulator.FiringError, ValidatedSimulator[S]] =
            (vs: S).fire(t)

        def isEnabled(t: TransitionId): Boolean = (vs: S).isEnabled(t)

        def enabledTransitions: Set[TransitionId] = (vs: S).enabledTransitions

        /** Read-only access to the underlying net for inspection purposes. */
        def get: S = vs
}
