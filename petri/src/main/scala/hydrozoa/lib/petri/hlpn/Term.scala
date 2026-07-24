package hydrozoa.lib.petri.hlpn

import hydrozoa.lib.petri.Positive

// TODO (symmetry): this term language is the *symmetric-net* color-function fragment (projection /
// `Succ` / tuple; `=`/`<`/subclass guards) — not general HLPN §8 operators — but the SN symmetry
// property is not enforced. `Const` names a specific color (symmetry-breaking on an unordered class
// unless it is a static subclass, i.e. a fixed point of the group), and `Lt` is equivariant only on
// linear classes. Future work, in order:
//   1. A firing-equivariance property: `σ · fire(t, β, M) == fire(t, σ·β, σ·M)` for permutations σ
//      in a net's admissible group (full S_n on a constant-free unordered class, rotations on
//      circular, trivial on linear, ∩ the subgroup fixing every `Const` / static subclass). Turns
//      "is this net symmetric?" into a ScalaCheck test and grounds later symmetry reduction.
//   2. A symmetry *classifier* (a `SortCheck`-style pass computing a net's admissible group / what
//      breaks it) — a classifier, not a restrictive `Symmetric` sublanguage, since real nets (the
//      RBR one) legitimately use constants and we want to know their symmetry, not forbid it.

/** A typed variable of a transition (ISO 15909-1 `V`, the S-indexed variable set): a name with a
  * color sort. Bound to a value by a [[Binding]] to form a transition mode.
  */
final case class Var[C](name: String, sort: Sort[C])

/** A collection (batch) variable (ISO §8). Unlike [[Var]] it binds to a whole sub-multiset of a
  * place's tokens, not a single color — the device a firing uses to consume/produce a variable-size
  * batch (see [[Inscription.Collect]]). `bound` caps the batch size.
  */
final case class CollectVar[C](name: String, sort: Sort[C], bound: Int)

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

    /** A match-only wildcard: in a [[Inscription.Collect]] pattern it matches any color of its sort
      * (the "collected" dimension of a batch) and binds nothing. It is never evaluated — a
      * `Collect` annotation reads its bound multiset, not its pattern — so `evalColor` rejects it.
      */
    final case class Wildcard[C](sort: Sort[C]) extends ColorTerm[C]

/** An arc inscription (ISO Concept 17, "class color function"): evaluates under a [[Binding]] to a
  * multiset over C. This is `W(p,t)` / `W(t,p)`. Built as a positive linear combination of weighted
  * colors (`Positive` coefficients — a weighted color moves ≥ 1 token, per ISO 15909-1's positive
  * arc weights). A [[Collect]] arc additionally consumes/produces a variable-size *batch* of
  * present tokens (ISO §8). (The `all` broadcast of Concept 16 — synchronizing over a whole *class*
  * — remains omitted; `Collect` ranges over the tokens actually in a place, not the class carrier,
  * so it forces no class enumeration.)
  */
sealed trait Inscription[C]:
    def sort: Sort[C]

object Inscription:

    /** `n·⟨color⟩` — `n` copies of one color. `⟨x⟩` is `Weighted(Positive.unsafe(1), x)`. */
    final case class Weighted[C](coeff: Positive, color: ColorTerm[C]) extends Inscription[C]:
        def sort: Sort[C] = color.sort

    /** `⊕` — multiset union, i.e. the sum of a linear combination. */
    final case class Union[C](left: Inscription[C], right: Inscription[C]) extends Inscription[C]:
        def sort: Sort[C] = left.sort

    /** A collection (batch) arc (ISO §8): consume/produce a variable-size sub-multiset in one
      * firing. On an input arc the [[ModeSelector]] binds `collected` to the sub-multiset of the
      * place's tokens matching `pattern` under the mode — free [[ColorTerm.Wildcard]] positions are
      * the collected dimension — capped at `collected.bound`; on an output arc it re-emits that
      * bound multiset. Either way the annotation evaluates to `collected`'s bound multiset, so
      * enabling/firing treat it exactly like any other `W(f)⟦β⟧`.
      */
    final case class Collect[C](collected: CollectVar[C], pattern: ColorTerm[C])
        extends Inscription[C]:
        def sort: Sort[C] = collected.sort

    /** An inhibitor arc (ISO 15909-3): on an input arc `p → t`, `t` is enabled only if `p` holds
      * **no** token matching `pattern` under the mode (free [[ColorTerm.Wildcard]] positions match
      * anything) — the dual of [[Collect]]. It is a pure precondition: it consumes nothing, so it
      * evaluates to the empty multiset, and the absence check lives in `HlNet.fire`.
      */
    final case class Inhibit[C](pattern: ColorTerm[C]) extends Inscription[C]:
        def sort: Sort[C] = pattern.sort

    /** A read (test) arc (ISO 15909-3): on an input arc `p → t`, `t` is enabled only if `M(p)`
      * covers `inner`, but firing consumes nothing — exactly as a `Pt`+`Tp` self-loop of `inner`
      * would. `HlNet.fire` realizes it as that self-loop, so it is a semantic no-op versus the
      * pair; it exists to say "read, not consume" in a single arc (and to render as a single edge).
      */
    final case class Read[C](inner: Inscription[C]) extends Inscription[C]:
        def sort: Sort[C] = inner.sort

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
