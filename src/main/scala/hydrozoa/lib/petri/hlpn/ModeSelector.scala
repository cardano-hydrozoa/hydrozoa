package hydrozoa.lib.petri.hlpn

/** A strategy for proposing candidate modes of a transition — the pluggable search side of HLPN
  * simulation (enumeration, unification, or a domain-specific method).
  *
  * Contract: candidates need not all be enabled — [[HlSimulator]] validates each against the net's
  * own rules, so a wrong candidate is filtered, never fired. A selector that *omits* an enabled
  * mode, however, makes the simulator incomplete for it: completeness is the selector's burden,
  * soundness the net's.
  */
trait ModeSelector[PlaceId, TransitionId, C] {

    /** Candidate modes for `tid` at the net's current marking, in preference order. */
    def candidates(
        net: HlNet[PlaceId, TransitionId, C],
        tid: TransitionId
    ): LazyList[Binding]
}

object ModeSelector {

    /** The enumerating selector: the cartesian product of the transition's variable carriers.
      * Complete for the symmetric-net fragment (finite classes) but exponential in the variable
      * count and materializes each class — suits small nets; a unification-based selector that
      * matches inscriptions against the tokens present in the marking is the scalable alternative.
      */
    def enumerating[PlaceId, TransitionId, C]: ModeSelector[PlaceId, TransitionId, C] =
        (net, tid) =>
            net.transitionsMap.get(tid) match {
                case None             => LazyList.empty
                case Some(transition) => candidateBindings(transition.variables)
            }

    /** All candidate bindings: the cartesian product of each variable's carrier. */
    private def candidateBindings[C](variables: List[Var[C]]): LazyList[Binding] =
        variables.foldLeft(LazyList(Binding.empty)) { (bindings, v) =>
            for
                b <- bindings
                value <- LazyList.from(enumerate(v.sort))
            yield Binding.bind(b, v, value)
        }

    /** Enumerate every color of a finite sort. Enumeration is specific to this strategy, so it
      * lives here rather than on `Sort` (which offers only membership).
      */
    private def enumerate[C](sort: Sort[C]): List[C] =
        sort match {
            case Sort.Dot                     => List(())
            case Sort.Class(_, carrier, _, _) => carrier.toSortedSet.toList
            case Sort.Prod(left, right) =>
                for
                    x <- enumerate(left)
                    y <- enumerate(right)
                yield (x, y)
        }
}
