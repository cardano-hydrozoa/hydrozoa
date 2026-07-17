package hydrozoa.lib.petri.hlpn

import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import scala.collection.immutable.SortedMap
import spire.algebra.Order
import spire.math.SafeLong

/** A candidate transition mode: an assignment of a transition's variables to color values of their
  * sorts. A binding is *complete* for a term when every variable the term references is assigned;
  * evaluation of an incomplete binding fails with [[EvalError.UnboundVariable]]. Mode search (the
  * enabling engine) builds partial bindings internally and only exposes complete ones.
  */
opaque type Binding = Map[Var[?], Any]

object Binding:

    val empty: Binding = Map.empty

    /** Extend a binding, assigning `value` to `v`. */
    def bind[C](b: Binding, v: Var[C], value: C): Binding = b.updated(v, value)

    /** Look up the value assigned to `v`, if any. The cast is sound: [[bind]] stores a `C` under a
      * `Var[C]`.
      */
    def lookup[C](b: Binding, v: Var[C]): Option[C] = b.get(v).map(_.asInstanceOf[C])

    /** Evaluate a color function to a single color. `Left` if a referenced variable is unbound
      * (incomplete binding) or a class operation is undefined for its sort (e.g. `Succ` off an
      * unordered class, or off the last element of a `Linear` class).
      */
    def evalColor[C](term: ColorTerm[C], b: Binding): Either[EvalError, C] =
        term match
            case ColorTerm.Ref(v) => lookup(b, v).toRight(EvalError.UnboundVariable(v.name))
            case ColorTerm.Const(value, _) => Right(value)
            case ColorTerm.Tuple(l, r) =>
                for a <- evalColor(l, b); c <- evalColor(r, b) yield (a, c)
            case ColorTerm.Succ(inner) =>
                evalColor(inner, b).flatMap(successor(inner.sort, _))

    /** Evaluate an inscription to a concrete multiset over C. The key order is taken from the
      * term's sort.
      */
    def evalInscription[C](
        term: Inscription[C],
        b: Binding
    ): Either[EvalError, Multiset[C, SafeLong]] =
        given Order[C] = term.sort.order
        def bag(entries: List[(C, SafeLong)]): Multiset[C, SafeLong] =
            Multiset(entries.to(SortedMap))
        term match
            case Inscription.Weighted(coeff, color) =>
                evalColor(color, b).map(c => bag(List(c -> SafeLong(coeff.toInt))))
            case Inscription.All(over) =>
                Right(bag(over.elements.map(_ -> SafeLong.one)))
            case Inscription.SubclassAll(over, sub) =>
                over match
                    case Sort.Class(name, _, _, subclasses) =>
                        subclasses
                            .get(sub)
                            .toRight(EvalError.UnknownSubclass(name, sub))
                            .map(elems => bag(elems.toList.map(_ -> SafeLong.one)))
                    case other => Left(EvalError.UnknownSubclass(other.toString, sub))
            case Inscription.Union(l, r) =>
                for lb <- evalInscription(l, b); rb <- evalInscription(r, b)
                yield lb.combineWith(rb)((x, y) => x + y)

    /** Evaluate a guard to a boolean. */
    def evalGuard(guard: Guard, b: Binding): Either[EvalError, Boolean] =
        guard match
            case Guard.True => Right(true)
            case Guard.Eq(l, r) =>
                for a <- evalColor(l, b); c <- evalColor(r, b) yield l.sort.order.eqv(a, c)
            case Guard.Lt(l, r) =>
                for a <- evalColor(l, b); c <- evalColor(r, b) yield l.sort.order.lt(a, c)
            case Guard.InSubclass(color, sub) =>
                evalColor(color, b).flatMap(c =>
                    color.sort match
                        case Sort.Class(name, _, _, subclasses) =>
                            subclasses
                                .get(sub)
                                .toRight(EvalError.UnknownSubclass(name, sub))
                                .map(_.contains(c))
                        case other => Left(EvalError.UnknownSubclass(other.toString, sub))
                )
            case Guard.Not(g)    => evalGuard(g, b).map(!_)
            case Guard.And(l, r) => for a <- evalGuard(l, b); c <- evalGuard(r, b) yield a && c
            case Guard.Or(l, r)  => for a <- evalGuard(l, b); c <- evalGuard(r, b) yield a || c

    /** The successor of `value` in `sort`, per the class discipline: `Circular` wraps, `Linear` has
      * none past the last element, `Unordered` has none.
      */
    private def successor[C](sort: Sort[C], value: C): Either[EvalError, C] =
        sort match
            case Sort.Class(name, carrier, discipline, _) =>
                val elems = carrier.toSortedSet.toList
                val i = elems.indexOf(value)
                discipline match
                    case Sort.Discipline.Circular => Right(elems((i + 1) % elems.size))
                    case Sort.Discipline.Linear =>
                        if i + 1 < elems.size then Right(elems(i + 1))
                        else Left(EvalError.SuccessorUndefined(name))
                    case Sort.Discipline.Unordered => Left(EvalError.SuccessorUndefined(name))
            case _ => Left(EvalError.SuccessorUndefined(sort.toString))

/** Why evaluation of a term under a binding failed. */
enum EvalError:
    /** A referenced variable has no assignment — the binding is incomplete for this term. */
    case UnboundVariable(name: String)

    /** `Succ` was applied where the sort's discipline defines no successor. */
    case SuccessorUndefined(sort: String)

    /** A guard or inscription referenced a subclass name absent from the sort's partition. */
    case UnknownSubclass(sort: String, subclass: String)
