package hydrozoa.lib.petri.hlpn

import cats.implicits.*

/** A resolved HLPN transition: the variables it binds (ISO `V`), its guard `Φ`, and its arcs (each
  * an [[ArcSemanticsH]] connected to a place). Carries the *semantic* operations given a mode and a
  * marking — whether the mode enables it, and the marking it produces — but **not** any search for
  * enabled modes; finding modes is a simulator concern (see [[ModeSearch]]). Direction-neutral: an
  * arc constrains enabling only through its own `pre` (`pre ≤ M(p)`), so output arcs never block.
  */
final case class TransitionH[PlaceId, C](
    variables: List[Var[C]],
    guard: Guard,
    arcs: List[(PlaceId, ArcSemanticsH[C])]
):

    /** Whether `mode` enables this transition at `marking` (ISO Concept 23): the guard holds and
      * every arc is enabled (`W(p,t)⟦mode⟧ ≤ M(p)`). An evaluation failure counts as not enabled.
      */
    def enabledUnder(mode: Binding, marking: PlaceId => MultiSet[C]): Boolean =
        Binding.evalGuard(guard, mode) == Right(true) &&
            arcs.forall { case (pid, arc) => arc.enabled(mode, marking(pid)) == Right(true) }

    /** The new marking of every place this transition touches, firing under `mode` (`M − pre +
      * post` per arc, ISO Concept 24). Assumes `mode` enables it; a negative multiplicity surfaces
      * at the place's E1 check downstream.
      */
    def firedMarkings(
        mode: Binding,
        marking: PlaceId => MultiSet[C]
    ): Either[EvalError, Map[PlaceId, MultiSet[C]]] =
        arcs.traverse { case (pid, arc) => arc.fire(mode, marking(pid)).map(pid -> _) }.map(_.toMap)
