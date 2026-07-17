package hydrozoa.lib.petri.hlpn

/** Static well-sortedness for an [[HlNet]] (§5): rejects terms that are constructible but not
  * meaningful, before any simulation. This is the compilation step that turns "a bag of terms with
  * variables" into "a coherent net that can be simulated". Evaluation deliberately does not police
  * these — e.g. `Binding.evalGuard` will happily compare an unordered class by carrier index — so
  * they must be caught here.
  *
  * Checked: variables referenced but not declared by the transition; `Succ` / `Lt` over a class
  * with no successor order; subclass names absent from a sort's partition; and an arc inscription
  * whose sort is not the connected place's color domain (`W(p,t)` must range over `Bag(C(p))`).
  */
object SortCheck:

    /** Every sort error in `net`, empty if it is well-sorted. */
    def errors[PlaceId, TransitionId, ArcId, C](
        net: HlNet[PlaceId, TransitionId, ArcId, C]
    ): List[SortError] =
        val guardErrors = net.transitions.toList.flatMap { (_, decl) =>
            walkGuard(decl.guard, decl.variables)
        }
        val arcErrors = net.arcs.toList.flatMap { (arcId, arc) =>
            val declared = net.transitions.get(arc.transition).map(_.variables).getOrElse(Nil)
            inscriptionOf(arc.semantics) match
                case None => Nil
                case Some(inscription) =>
                    walkInscription(inscription, declared) ++
                        domainErrors(arcId.toString, arc.place, inscription, net.places)
        }
        guardErrors ++ arcErrors

    private def domainErrors[PlaceId, C](
        arcId: String,
        place: PlaceId,
        inscription: Inscription[C],
        places: Map[PlaceId, ColoredPlace[C]]
    ): List[SortError] =
        places.get(place) match
            case None => List(SortError.MissingPlace(place.toString))
            case Some(p) if inscription.sort != p.colorDomain =>
                List(
                  SortError.ArcDomainMismatch(arcId, name(inscription.sort), name(p.colorDomain))
                )
            case Some(_) => Nil

    private def walkGuard(guard: Guard, declared: List[Var[?]]): List[SortError] =
        guard match
            case Guard.True      => Nil
            case Guard.Eq(l, r)  => walkColor(l, declared) ++ walkColor(r, declared)
            case Guard.Not(g)    => walkGuard(g, declared)
            case Guard.And(l, r) => walkGuard(l, declared) ++ walkGuard(r, declared)
            case Guard.Or(l, r)  => walkGuard(l, declared) ++ walkGuard(r, declared)
            case Guard.Lt(l, r) =>
                val order = Option.unless(isOrdered(l.sort))(SortError.LtOnUnordered(name(l.sort)))
                order.toList ++ walkColor(l, declared) ++ walkColor(r, declared)
            case Guard.InSubclass(c, sub) =>
                subclassError(c.sort, sub).toList ++ walkColor(c, declared)

    private def walkInscription(
        inscription: Inscription[?],
        declared: List[Var[?]]
    ): List[SortError] =
        inscription match
            case Inscription.Weighted(_, color)     => walkColor(color, declared)
            case Inscription.All(_)                 => Nil
            case Inscription.SubclassAll(sort, sub) => subclassError(sort, sub).toList
            case Inscription.Union(l, r) =>
                walkInscription(l, declared) ++ walkInscription(r, declared)

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

    private def inscriptionOf[C](semantics: ArcSemanticsH[C]): Option[Inscription[C]] =
        semantics match
            case ArcSemanticsH.Consume(i) => Some(i)
            case ArcSemanticsH.Produce(i) => Some(i)
            case ArcSemanticsH.Read(i)    => Some(i)
            case _                        => None

/** A static well-sortedness violation in a net's terms. */
enum SortError:
    /** A term references a variable the transition does not declare. */
    case UndeclaredVariable(name: String)

    /** `Succ` over a class with no successor order (`Unordered`, or a non-class sort). */
    case SuccOnUnordered(sort: String)

    /** `Lt` over a class with no order. */
    case LtOnUnordered(sort: String)

    /** A subclass name absent from the sort's partition. */
    case UnknownSubclass(sort: String, subclass: String)

    /** An arc inscription's sort is not the connected place's color domain. */
    case ArcDomainMismatch(arc: String, inscriptionSort: String, placeDomain: String)

    /** An arc references a place absent from the net. */
    case MissingPlace(place: String)
