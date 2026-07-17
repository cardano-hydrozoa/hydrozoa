package hydrozoa.lib.petri.hlpn

import hydrozoa.lib.number.PositiveInt

/** A typed variable of a transition (ISO 15909-1 `V`, the S-indexed variable set): a name with a
  * color sort. Bound to a value by a [[Binding]] to form a transition mode.
  */
final case class Var[C](name: String, sort: Sort[C])

/** A color function (ISO Concept 16, "basic color function"): evaluates under a [[Binding]] to a
  * single color of sort C. Leaves are variables (the projections of the transition's color domain);
  * `Succ` steps an ordered/circular class; `Tuple` builds a product color.
  *
  * Typed by construction, but not every syntactically constructible term is well-sorted — e.g.
  * `Succ` over an unordered class. Well-sortedness is checked at net validation, not here.
  */
sealed trait ColorTerm[C]:
    def sort: Sort[C]

object ColorTerm:

    /** A bound-variable occurrence (a projection `X_C^j` of the transition's color domain). */
    final case class Ref[C](v: Var[C]) extends ColorTerm[C]:
        def sort: Sort[C] = v.sort

    /** A literal color. */
    final case class Const[C](value: C, sort: Sort[C]) extends ColorTerm[C]

    /** The successor `X++` in an ordered/circular class (Concept 16). */
    final case class Succ[C](inner: ColorTerm[C]) extends ColorTerm[C]:
        def sort: Sort[C] = inner.sort

    /** A product color `⟨a, b⟩`. */
    final case class Tuple[A, B](left: ColorTerm[A], right: ColorTerm[B]) extends ColorTerm[(A, B)]:
        def sort: Sort[(A, B)] = Sort.Prod(left.sort, right.sort)

/** An arc inscription (ISO Concept 17, "class color function"): evaluates under a [[Binding]] to a
  * multiset over C. This is `W(p,t)` / `W(t,p)`. Built as a positive linear combination of weighted
  * colors; kept positive so an inscription never selects a negative number of tokens. (The `all`
  * broadcast of Concept 16 is deliberately omitted — it is symmetric-net sugar that would force a
  * full enumeration of a class.)
  */
sealed trait Inscription[C]:
    def sort: Sort[C]

object Inscription:

    /** `n·⟨color⟩` — `n` copies of one color. `⟨x⟩` is `Weighted(1, x)`. */
    final case class Weighted[C](coeff: PositiveInt, color: ColorTerm[C]) extends Inscription[C]:
        def sort: Sort[C] = color.sort

    /** `⊕` — multiset union, i.e. the sum of a linear combination. */
    final case class Union[C](left: Inscription[C], right: Inscription[C]) extends Inscription[C]:
        def sort: Sort[C] = left.sort

/** A transition guard (ISO Φ, Concept 15): evaluates under a [[Binding]] to a boolean.
  * Symmetric-net guards compare colors (`=`, `<` on ordered classes) and test static-subclass
  * membership, closed under the boolean connectives.
  */
sealed trait Guard

object Guard:
    case object True extends Guard
    final case class Eq[C](left: ColorTerm[C], right: ColorTerm[C]) extends Guard
    final case class Lt[C](left: ColorTerm[C], right: ColorTerm[C]) extends Guard
    final case class InSubclass[C](color: ColorTerm[C], subclass: String) extends Guard
    final case class Not(g: Guard) extends Guard
    final case class And(left: Guard, right: Guard) extends Guard
    final case class Or(left: Guard, right: Guard) extends Guard
