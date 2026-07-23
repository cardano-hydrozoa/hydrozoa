package hydrozoa.lib.petri.hlpn

import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.petri.net.components.Arc
import scala.collection.immutable.SortedMap
import spire.algebra.Order
import spire.math.SafeLong

/** A strategy for proposing candidate modes of a transition — the pluggable search side of HLPN
  * simulation (enumeration, unification, or a domain-specific method).
  *
  * Contract: candidates need not all be enabled — [[HlSimulator]] validates each against the net's
  * own rules, so a wrong candidate is filtered, never fired. A selector that *omits* an enabled
  * mode, however, makes the simulator incomplete for it: completeness is the selector's burden,
  * soundness the net's.
  *
  * This is a *firing* search and may be incomplete: [[HlSimulator.fire]] only needs it to propose
  * *some* enabled candidate. The framework offers no "enabled set" query — that would be a
  * selector-relative result masquerading as truth (an arbitrary selector may propose nothing for a
  * transition). A caller that wants the enabled modes composes
  * `candidates(net, tid).filter(net.isModeEnabled(tid, _))` itself, and its completeness is *this
  * selector's* burden.
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
      * count and materializes each class — suits small nets. See [[unifying]] for the scalable
      * alternative.
      */
    def enumerating[PlaceId, TransitionId, C]: ModeSelector[PlaceId, TransitionId, C] =
        (net, tid) =>
            net.transitionsMap
                .get(tid)
                .fold(LazyList.empty)(t =>
                    complete(Binding.empty, t.variables).flatMap(bindCollections(net, tid, _))
                )

    /** The unifying selector: bind each variable by *matching* the transition's input-arc
      * inscriptions against the tokens actually present in `M(p)`, rather than enumerating
      * carriers.
      *
      * For every input arc `p → t`, each weighted-color leaf of its inscription is unified against
      * each token in `M(p)` (a partial binding, or nothing). Those partials are joined across all
      * input leaves — a shared variable must resolve consistently — so the bound variables are
      * pinned by real tokens, bounded by the *marking* size, not the class size. Variables the
      * transition declares but no input arc mentions ("free" — e.g. the vote value in `castVote`,
      * or `versionNew` in a ratchet) cannot be matched, so those alone are enumerated.
      *
      * Complete: every enabled mode `β` has `W(p,t)⟦β⟧ ≤ M(p)`, so each input leaf's color under
      * `β` is a present token that the unification recovers; the free part is enumerated. It may
      * over-propose (multiplicity and the guard are not checked here) — [[HlSimulator]] filters.
      */
    def unifying[PlaceId, TransitionId, C]: ModeSelector[PlaceId, TransitionId, C] =
        (net, tid) =>
            net.transitionsMap.get(tid) match
                case None             => LazyList.empty
                case Some(transition) =>
                    // Each input-arc leaf: a color term paired with the tokens present in its place.
                    val demands: List[(ColorTerm[?], List[Any])] =
                        net.arcsMap.toList
                            .collect {
                                case (Arc.Flow.Pt(place, t), arc) if t == tid =>
                                    (arc.inscription, net.marking(place))
                            }
                            .flatMap { (inscription, marking) =>
                                val tokens = marking.multiplicityMap.keys.toList
                                leaves(inscription).map(term => (term, tokens))
                            }

                    // Join: partial bindings consistent with every input demand.
                    val partials = demands.foldLeft(LazyList(Binding.empty)) {
                        case (bindings, (term, tokens)) =>
                            for
                                partial <- bindings
                                token <- LazyList.from(tokens)
                                unified <- unify(term, token, partial).to(LazyList)
                            yield unified
                    }

                    // Variables the input arcs never bind still need enumerating.
                    val boundVars = demands.flatMap((term, _) => termVars(term)).toSet
                    val freeVars = transition.variables.filterNot(boundVars.contains)

                    partials
                        .flatMap(complete(_, freeVars))
                        .flatMap(bindCollections(net, tid, _))

    /** Extend `partial` by binding each variable in `vars` to every color of its carrier — the
      * cartesian completion shared by [[enumerating]] (all variables) and [[unifying]] (only the
      * free ones).
      */
    private def complete(partial: Binding, vars: List[Var[?]]): LazyList[Binding] =
        vars.foldLeft(LazyList(partial)) { (bindings, v) =>
            for
                b <- bindings
                value <- LazyList.from(enumerate(v.sort))
            yield Binding.bind(b, v.asInstanceOf[Var[Any]], value)
        }

    /** Unify a color term against a concrete token, extending `partial` — `None` on a clash. The
      * inverse of [[Binding.evalColor]]: `Const` by equality, `Ref` binds or checks, `Tuple`
      * component-wise, `Succ` by inverting the successor (its predecessor).
      */
    private def unify(term: ColorTerm[?], value: Any, partial: Binding): Option[Binding] =
        term match
            case ColorTerm.Const(c, _) => Option.when(c == value)(partial)
            case ColorTerm.Ref(v) =>
                val variable = v.asInstanceOf[Var[Any]]
                Binding.lookup(partial, variable) match
                    case Some(bound) => Option.when(bound == value)(partial)
                    case None        => Some(Binding.bind(partial, variable, value))
            case ColorTerm.Tuple(l, r) =>
                value match
                    case (a, b) => unify(l, a, partial).flatMap(unify(r, b, _))
                    case _      => None
            case ColorTerm.Succ(inner) =>
                Binding.predecessor(inner.sort, value).flatMap(unify(inner, _, partial))
            case ColorTerm.Wildcard(_) => Some(partial)

    /** The color of every weighted leaf of an inscription (both branches of a `Union`). `Collect`
      * (its variable is bound by [[bindCollections]]) and `Inhibit` (a precondition, checked by the
      * net) contribute no scalar leaves.
      */
    private def leaves(inscription: Inscription[?]): List[ColorTerm[?]] =
        inscription match
            case Inscription.Weighted(_, color) => List(color)
            case Inscription.Union(l, r)        => leaves(l) ++ leaves(r)
            case Inscription.Collect(_, _)      => Nil
            case Inscription.Inhibit(_)         => Nil
            case Inscription.Read(inner)        => leaves(inner)

    /** The variables a color term references. */
    private def termVars(term: ColorTerm[?]): Set[Var[?]] =
        term match
            case ColorTerm.Ref(v)      => Set(v)
            case ColorTerm.Const(_, _) => Set.empty
            case ColorTerm.Tuple(l, r) => termVars(l) ++ termVars(r)
            case ColorTerm.Succ(inner) => termVars(inner)
            case ColorTerm.Wildcard(_) => Set.empty

    /** Bind every collection variable of `tid`: for each input `Collect(cv, pattern)` arc, gather
      * the sub-multiset of the place's tokens matching `pattern` under `binding` (free `Wildcard`
      * positions range over the collected dimension), capped at `cv.bound`. Deterministic given the
      * scalar binding, so it extends each candidate in place. Yields `None` — dropping the
      * candidate — if any collection is empty, since a batch firing over nothing is a spurious
      * no-op.
      */
    private def bindCollections[PlaceId, TransitionId, C](
        net: HlNet[PlaceId, TransitionId, C],
        tid: TransitionId,
        binding: Binding
    ): Option[Binding] =
        net.arcsMap.toList.foldLeft(Option(binding)) {
            case (Some(b), (Arc.Flow.Pt(place, t), arc)) if t == tid =>
                arc.inscription match
                    case Inscription.Collect(cv, pattern) =>
                        bindOneCollection(net, place, cv, pattern, b)
                    case _ => Some(b)
            case (acc, _) => acc
        }

    /** Bind one collection variable to the capped sub-multiset of `place`'s tokens matching
      * `pattern` under `binding`; `None` if nothing matches (an empty batch does not fire).
      */
    private def bindOneCollection[PlaceId, TransitionId, C](
        net: HlNet[PlaceId, TransitionId, C],
        place: PlaceId,
        cv: CollectVar[C],
        pattern: ColorTerm[C],
        binding: Binding
    ): Option[Binding] =
        given Order[C] = net.placesMap(place).colorDomain.order
        val matched = net
            .marking(place)
            .multiplicityMap
            .toList
            .filter((token, _) => unify(pattern, token, binding).isDefined)
        val batch = Multiset(capTo(matched, cv.bound).to(SortedMap))
        Option.when(batch.multiplicityMap.nonEmpty)(Binding.bindCollected(binding, cv, batch))

    /** The longest prefix of `entries` whose multiplicities sum to at most `bound` (splitting the
      * final entry if it would overrun).
      */
    private def capTo[C](entries: List[(C, SafeLong)], bound: Int): List[(C, SafeLong)] =
        @annotation.tailrec
        def go(
            rem: List[(C, SafeLong)],
            budget: SafeLong,
            acc: List[(C, SafeLong)]
        ): List[(C, SafeLong)] =
            rem match
                case (color, n) :: tail if budget > SafeLong(0) =>
                    val take = if n <= budget then n else budget
                    go(tail, budget - take, (color -> take) :: acc)
                case _ => acc.reverse
        go(entries, SafeLong(bound.max(0)), Nil)

    /** Enumerate every color of a finite sort. Specific to the carrier-materializing strategies, so
      * it lives here rather than on `Sort` (which offers only membership).
      */
    private def enumerate[C](sort: Sort[C]): List[C] =
        sort match
            case Sort.Dot                     => List(())
            case Sort.Class(_, carrier, _, _) => carrier.toSortedSet.toList
            case Sort.Prod(left, right) =>
                for
                    x <- enumerate(left)
                    y <- enumerate(right)
                yield (x, y)
}
