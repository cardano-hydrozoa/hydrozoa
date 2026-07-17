package hydrozoa.lib.petri.hlpn

/** An *enumerating* strategy for finding the enabled modes of a transition: enumerate the cartesian
  * product of the variables' carriers and keep the bindings that enable it
  * ([[TransitionH.enabledUnder]]).
  *
  * This is one simulation strategy, deliberately **not** part of the net — a net exposes only the
  * enabling predicate and firing ([[HlNet.isModeEnabled]] / [[HlNet.fire]]); how you search for
  * modes (enumeration here, unification, or a scenario-specific method) is a separate concern.
  * Enumeration is exponential in the variable count and materializes each variable's class, so it
  * suits small finite nets; a unification-based strategy that matches inscriptions against the
  * tokens present in `M(p)` is the scalable alternative.
  */
object ModeSearch:

    /** Every enabled mode of `t` at `marking`. */
    def enabledModes[PlaceId, C](
        t: TransitionH[PlaceId, C],
        marking: PlaceId => MultiSet[C]
    ): LazyList[Binding] =
        candidates(t.variables).filter(t.enabledUnder(_, marking))

    /** Whether `t` has any enabled mode at `marking`. */
    def isEnabled[PlaceId, C](
        t: TransitionH[PlaceId, C],
        marking: PlaceId => MultiSet[C]
    ): Boolean = enabledModes(t, marking).nonEmpty

    /** Every enabled mode of transition `tid` in `net` (empty if `tid` is unknown). */
    def enabledModes[PlaceId, TransitionId, ArcId, C](
        net: HlNet[PlaceId, TransitionId, ArcId, C],
        tid: TransitionId
    ): LazyList[Binding] =
        net.transitionH(tid).fold(LazyList.empty)(enabledModes(_, net.marking))

    /** All candidate bindings: the cartesian product of each variable's carrier. */
    private def candidates[C](variables: List[Var[C]]): LazyList[Binding] =
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
        sort match
            case Sort.Dot                     => List(())
            case Sort.Class(_, carrier, _, _) => carrier.toSortedSet.toList
            case Sort.Prod(left, right) =>
                for x <- enumerate(left); y <- enumerate(right) yield (x, y)
