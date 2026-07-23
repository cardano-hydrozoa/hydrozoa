package hydrozoa.lib.petri.hlpn

/** Static well-sortedness for an [[HlNet]] (§5): rejects terms that are constructible but not
  * meaningful, before any simulation. This is the compilation step that turns "a bag of terms with
  * variables" into "a coherent net that can be simulated". Evaluation deliberately does not police
  * these — e.g. `Binding.evalGuard` will happily compare an unordered class by carrier index — so
  * they must be caught here.
  *
  * Checked: variables referenced but not declared by the transition; `Succ` / `Lt` over a class
  * with no successor order; guard operands (`Eq` / `Lt`) whose sorts differ — the type parameter
  * cannot catch this, since distinct color classes may share a Scala type (e.g. `Key` and the
  * version minor are both `BigInt`); subclass names absent from a sort's partition; every leaf of
  * an arc inscription whose sort is not the connected place's color domain (`W(p,t)` must range
  * over `Bag(C(p))`); and arcs referencing a place or transition absent from the net.
  */
object SortCheck:

    /** Every sort error in `net`, empty if it is well-sorted. */
    def errors[PlaceId, TransitionId, C](
        net: HlNet[PlaceId, TransitionId, C]
    ): List[SortError] =
        val guardErrors = net.transitionsMap.toList.flatMap { (_, decl) =>
            walkGuard(decl.guard, decl.variables)
        }
        val arcErrors = net.arcsMap.toList.flatMap { (flow, arc) =>
            val placeErrors =
                domainErrors(flow.toString, flow.place, arc.inscription, net.placesMap)
            // A dangling transition would make every arc variable look undeclared; report it once
            // and skip the term walk. (Topology validation also flags it, at the net level.)
            net.transitionsMap.get(flow.transition) match
                case None => SortError.MissingTransition(flow.transition.toString) :: placeErrors
                case Some(decl) => walkInscription(arc.inscription, decl.variables) ++ placeErrors
        }
        guardErrors ++ arcErrors

    /** Each distinct leaf sort of `inscription` that is not the connected place's color domain. */
    private def domainErrors[PlaceId, C](
        arcId: String,
        place: PlaceId,
        inscription: Inscription[C],
        places: Map[PlaceId, ColoredPlace[C]]
    ): List[SortError] =
        places.get(place) match
            case None => List(SortError.MissingPlace(place.toString))
            case Some(p) =>
                leafSorts(inscription).distinct
                    .filter(_ != p.colorDomain)
                    .map(s => SortError.ArcDomainMismatch(arcId, name(s), name(p.colorDomain)))

    /** The sort of every weighted color in an inscription — both branches of a `Union`, not just
      * the root (whose sort is only the left branch's).
      */
    private def leafSorts(inscription: Inscription[?]): List[Sort[?]] =
        inscription match
            case Inscription.Weighted(_, color) => List(color.sort)
            case Inscription.Union(l, r)        => leafSorts(l) ++ leafSorts(r)
            case Inscription.Collect(cv, _)     => List(cv.sort)
            case Inscription.Inhibit(pattern)   => List(pattern.sort)
            case Inscription.Read(inner)        => leafSorts(inner)

    private def walkGuard(guard: Guard, declared: List[Var[?]]): List[SortError] =
        guard match
            case Guard.True      => Nil
            case Guard.Not(g)    => walkGuard(g, declared)
            case Guard.And(l, r) => walkGuard(l, declared) ++ walkGuard(r, declared)
            case Guard.Or(l, r)  => walkGuard(l, declared) ++ walkGuard(r, declared)
            case Guard.Eq(l, r) =>
                operandMismatch("Eq", l.sort, r.sort).toList ++
                    walkColor(l, declared) ++ walkColor(r, declared)
            case Guard.Lt(l, r) =>
                val unordered =
                    Option.unless(isOrdered(l.sort))(SortError.LtOnUnordered(name(l.sort)))
                operandMismatch("Lt", l.sort, r.sort).toList ++ unordered.toList ++
                    walkColor(l, declared) ++ walkColor(r, declared)
            case Guard.InSubclass(c, sub) =>
                subclassError(c.sort, sub).toList ++ walkColor(c, declared)

    /** `Eq` / `Lt` require both operands to have the same sort; the type parameter alone does not
      * guarantee it (two color classes may share a Scala type).
      */
    private def operandMismatch(op: String, left: Sort[?], right: Sort[?]): Option[SortError] =
        Option.when(left != right)(SortError.OperandSortMismatch(op, name(left), name(right)))

    private def walkInscription(
        inscription: Inscription[?],
        declared: List[Var[?]]
    ): List[SortError] =
        inscription match
            case Inscription.Weighted(_, color) => walkColor(color, declared)
            case Inscription.Union(l, r) =>
                walkInscription(l, declared) ++ walkInscription(r, declared)
            case Inscription.Collect(_, pattern) => walkColor(pattern, declared)
            case Inscription.Inhibit(pattern)    => walkColor(pattern, declared)
            case Inscription.Read(inner)         => walkInscription(inner, declared)

    private def walkColor(term: ColorTerm[?], declared: List[Var[?]]): List[SortError] =
        term match
            case ColorTerm.Const(_, _) => Nil
            case ColorTerm.Ref(v) =>
                Option.unless(declared.contains(v))(SortError.UndeclaredVariable(v.name)).toList
            case ColorTerm.Tuple(l, r) => walkColor(l, declared) ++ walkColor(r, declared)
            case ColorTerm.Succ(inner) =>
                val order =
                    Option.unless(isOrdered(inner.sort))(
                      SortError.SuccOnUnordered(name(inner.sort))
                    )
                order.toList ++ walkColor(inner, declared)
            case ColorTerm.Wildcard(_) => Nil

    private def subclassError(sort: Sort[?], subclass: String): Option[SortError] =
        sort match
            case Sort.Class(_, _, _, subclasses) if subclasses.contains(subclass) => None
            case _ => Some(SortError.UnknownSubclass(name(sort), subclass))

    private def isOrdered(sort: Sort[?]): Boolean =
        sort match
            case Sort.Class(_, _, discipline, _) => discipline != Sort.Discipline.Unordered
            case _                               => false

    private def name(sort: Sort[?]): String =
        sort match
            case Sort.Dot               => "Dot"
            case Sort.Class(n, _, _, _) => n
            case Sort.Prod(left, right) => s"(${name(left)}, ${name(right)})"

/** A static well-sortedness violation in a net's terms or a sort definition. */
enum SortError:
    /** A term references a variable the transition does not declare. */
    case UndeclaredVariable(name: String)

    /** A `Sort.Class` was given a `subclasses` map that is not a genuine partition of its carrier
      * (an empty block, a color outside the carrier, an overlap, or a gap).
      */
    case MalformedPartition(sort: String, reason: String)

    /** `Succ` over a class with no successor order (`Unordered`, or a non-class sort). */
    case SuccOnUnordered(sort: String)

    /** `Lt` over a class with no order. */
    case LtOnUnordered(sort: String)

    /** `Eq` / `Lt` compares operands of different sorts — possible when distinct color classes
      * share a Scala type (e.g. `Key` and the version minor are both `BigInt`).
      */
    case OperandSortMismatch(guard: String, leftSort: String, rightSort: String)

    /** A subclass name absent from the sort's partition. */
    case UnknownSubclass(sort: String, subclass: String)

    /** An arc inscription leaf's sort is not the connected place's color domain. */
    case ArcDomainMismatch(arc: String, inscriptionSort: String, placeDomain: String)

    /** An arc references a place absent from the net. */
    case MissingPlace(place: String)

    /** An arc references a transition absent from the net. */
    case MissingTransition(transition: String)
