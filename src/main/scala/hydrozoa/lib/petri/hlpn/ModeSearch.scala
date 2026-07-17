package hydrozoa.lib.petri.hlpn

import cats.implicits.*

/** The enabling engine (ISO 15909-1 Concept 23): given a [[TransitionH]] and a marking, enumerate
  * the *modes* — bindings under which the transition is enabled — and fire it under a chosen mode.
  */
object ModeSearch:

    /** Every enabled mode of `t` at `marking`: bindings β where `Φ⟦β⟧` holds and every arc is
      * enabled (`W(p,t)⟦β⟧ ≤ M(p) ∧ sideCondition`).
      *
      * Enumerates the cartesian product of the variables' finite carriers, so it is exponential in
      * the variable count — correct for the symmetric-net fragment; a unification-based prune is
      * the future optimization. A binding whose terms are undefined for it (e.g. `Succ` off a
      * linear-class end) is simply not a mode; structural ill-sortedness is the sort-checker's
      * concern (§5), not this.
      */
    def enabledModes[PlaceId, C](
        t: TransitionH[PlaceId, C],
        marking: PlaceId => MultiSet[C]
    ): LazyList[Binding] =
        candidates(t.variables).filter(isModeEnabled(t, _, marking))

    /** Whether `t` has any enabled mode at `marking`. */
    def isEnabled[PlaceId, C](
        t: TransitionH[PlaceId, C],
        marking: PlaceId => MultiSet[C]
    ): Boolean = enabledModes(t, marking).nonEmpty

    /** Fire `t` under `mode`, returning the new marking of every place it touches (`M − pre + post`
      * per arc). Assumes `mode` is enabled; a resulting negative multiplicity would surface at the
      * place's E1 validity check downstream.
      */
    def fire[PlaceId, C](
        t: TransitionH[PlaceId, C],
        mode: Binding,
        marking: PlaceId => MultiSet[C]
    ): Either[EvalError, Map[PlaceId, MultiSet[C]]] =
        t.arcs
            .traverse { case (pid, arc) => arc.fire(mode, marking(pid)).map(pid -> _) }
            .map(_.toMap)

    /** All candidate bindings: the cartesian product of each variable's carrier. */
    private def candidates[C](variables: List[Var[C]]): LazyList[Binding] =
        variables.foldLeft(LazyList(Binding.empty)) { (bindings, v) =>
            for
                b <- bindings
                value <- LazyList.from(v.sort.elements)
            yield Binding.bind(b, v, value)
        }

    /** Whether the specific binding `b` enables `t` at `marking`: the guard holds and every arc is
      * enabled. Used to verify a mode before firing it.
      */
    def isModeEnabled[PlaceId, C](
        t: TransitionH[PlaceId, C],
        b: Binding,
        marking: PlaceId => MultiSet[C]
    ): Boolean =
        Binding.evalGuard(t.guard, b) == Right(true) &&
            t.arcs.forall { case (pid, arc) => arc.enabled(b, marking(pid)) == Right(true) }
