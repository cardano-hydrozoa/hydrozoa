# From P/T nets to High-Level Petri Nets (ISO/IEC 15909-1)

Status: design / re-evaluation. No code committed on this branch yet.

This document answers three questions raised for the petri-net library:

1. Is the current Scala framework suitable to host **ISO 15909-1 HLPNs**, or does it need
   restructuring? (§2)
2. What machinery must be added for **multiset places, arc inscriptions, and transition modes**,
   and what does the "expression DSL + compilation pipeline" look like? (§3–§5)
3. How hard is an **ISO 15909-2 (PNML) codec**? (§6)

The short version: the four-layer metamodel (topology / syntax / semantics / presentation) is
the right shape and survives almost untouched. HLPN is not a rewrite — it is *unbaking the one
place where P/T assumptions are hard-coded*: the arc-firing signature and the enabling rule. The
combinatorial blow-ups we hit in the RBR model (`AwaitingVote`/`Voted` × `Vote`/`Abstain`/
`Ratchet`; the artificial cap on evacuees-per-transition) are exactly the P/T unfolding of a
colored net. Fold them back and both disappear.

---

## 1. What ISO 15909-1 actually requires

The standard builds a tower: **P/T net** (§6) ⊂ **Symmetric net** (§7) ⊂ **HLPN** (§8). Each
level reuses the level below and only changes what annotations arcs/places/transitions carry and
how enabling/firing evaluate them.

**HLPN (Concept 21).** A high-level Petri net is a tuple `⟨N, W, D, C, V, Σ, Φ⟩`:

| Symbol | Meaning | Maps to (current framework) |
|--------|---------|------------------------------|
| `N = ⟨P, T, F⟩` | places, transitions, flow relation | `Net.Ids` + `Net.Topology` (exists) |
| `D` | color classes (the available token *sorts*) | **new** — net-wide `Syntax` |
| `C : P∪T → domain` | each place/transition typed by a color domain | **new** — place/transition `Syntax` |
| `Σ = (S, O)` | signature: sorts `S` + operators `O` | **new** — the DSL's operator table |
| `V` | `S`-indexed variables | **new** — per-transition variable decls |
| `W = TERM(O∪V)` | arc inscriptions: terms over ops + vars | replaces `Arc` weight (`PositiveInt`) |
| `Φ : T → TERM_B(O∪V)` | per-transition boolean guard | fills the `Transition.Semantics` stub |

**Marking (Concept 22).** `M(p) ∈ Bag(C(p))` — a **multiset over the place's color domain**, not
a token count. P/T nets are the special case `C(p) = {•}` (the singleton "dot" sort), where
`Bag({•}) ≅ ℕ`.

**Enabling (Concept 23).** `t` is enabled in `M` iff there exists a **binding** `β` (an
assignment of `t`'s variables to values) such that

```
(∀ p ∈ •t.  W(p,t)⟦β⟧ ≤ M(p))   ∧   Φ(t)⟦β⟧ = true
```

**Firing (Concept 24).** Firing `t` under a chosen binding `β`:

```
∀ p.  M'(p) = M(p) − W(p,t)⟦β⟧ + W(t,p)⟦β⟧          (multiset ± multiset)
```

The binding `β` is precisely the task's **"transition mode."** A transition can have *many*
enabled modes at one marking; enabling is `∃ mode`, firing selects one. This is the single
biggest departure from P/T, where "the" firing of an enabled transition is deterministic.

**Symmetric net (§7)** is the decidable middle ground: finite color classes, and arc functions
restricted to linear combinations of *projections*, *successors*, and the *broadcast* `all`
(Concept 17). It matters to us because it is the largest fragment where enabling/mode-search is
guaranteed finite and analysis is tractable — a natural first implementation target and the
natural PNML conformance level (see §6).

---

## 2. Re-evaluation of the current framework

### 2.1 What is already right (keep verbatim)

- **The T.S.S.P. ontological split.** ISO separates net structure (`N`), annotation (`W`, `Φ`),
  data typing (`D`, `C`, `Σ`, `V`), and — in Part 2 — presentation. That is our four layers
  exactly. The layering is *validated* by the standard, not just aesthetically nice.
- **`Place.Syntax#PlaceMarking` is already an abstract type.** `WithTokens` fixes it to
  `NonNegativeInt` — but that is a *specialization*, not a baked-in assumption. A colored place is
  just `Place.Syntax[P] { type PlaceMarking = MultiSet[C] }`. The metamodel anticipated this.
- **`Net.Topology` / `Net.Syntax` / `Net.Semantics` accessors** are color-agnostic ID→component
  maps. Unchanged.
- **`ValidatedSimulator`** (opaque, smart-constructed after `topologyErrors ++ syntaxErrors ++
  semanticsErrors`) is the right home for HLPN *well-sortedness* validation (§5). Its
  firing-preservation invariant still holds: firing changes only markings.
- **`MapNet.BuilderMOps`** (the `IndexedStateT` builder) is agnostic to what places/arcs carry.
  Unchanged.
- **Enabling composes under AND across levels** (place / arc / transition / net). Still true; HLPN
  adds the guard `Φ(t)` as one more conjunct, and wraps the whole thing in an ∃-over-bindings.

### 2.2 Where P/T is hard-coded (the actual work)

Three coupled sites, all in the *semantics/simulation* layer, none in topology or syntax:

1. **`Arc.Semantics[P]` is unary.** `enablingError(p): Option` and `fire(p): Either[E, P]` take
   only the place. They cannot express `W(p,t)⟦β⟧` because they have no `β`. **This signature must
   become mode-relative** (see §3.3). This is the load-bearing change.

2. **`SequentialSimulator.fire(t)` assumes one deterministic firing.** It snapshots
   `(arc, place)` pairs and applies each `arc.fire(place)` once. HLPN firing is *`fire(t, mode)`*,
   and there is a prior *mode-search* step. `fire` splits into `enabledModes(t): Iterator[Mode]`
   + `fire(t, mode)`.

3. **`isEnabled(t) := fire(t).isRight`** (test-fire). Under HLPN this is `enabledModes(t).nonEmpty`
   — an existential search, not a single test. The auto-firing selector (`enabledTransitions`)
   likewise now ranges over `(transition, mode)` pairs.

Everything else — `Net.*`, `ValidatedSimulator`, the builder, `Normalization` — is untouched.
That is the core finding: **HLPN is a semantics-layer extension, not a metamodel rewrite.**

### 2.3 On "undirected arcs, direction imposed by semantics"

Current `Arc.Topology[PlaceId, TransitionId]` records the *pair* `(place, transition)` with no
direction; direction lives in the semantics mixin (`PT` vs `TP`). **Recommendation: keep it, with
eyes open.**

- ISO's `F ⊆ (P×T) ∪ (T×P)` *is* directed, and `W` is defined separately on `(p,t)` and `(t,p)`.
  So a purist HLPN encoding would put direction in topology.
- But our generalization pays for itself: `Read`, `Inhibitor`, and `Reset` arcs are *not*
  cleanly "input" or "output" — they are test/side-condition arcs. Keeping topology
  direction-neutral and letting each arc's *term contribution* declare its role (consume from
  input marking / produce to output / test-only) models all five uniformly, and matches how the
  standard treats guards-vs-flow as separate concerns.
- Concrete rule for HLPN: an arc semantics contributes a **pre-multiset** `W⁻(p,t)⟦β⟧` (subtracted,
  and bounded by enabling) and a **post-multiset** `W⁺(t,p)⟦β⟧` (added). `PT` sets only `W⁻`;
  `TP` sets only `W⁺`; `Read` sets `W⁻ = W⁺` (test, no net change); `Inhibitor` sets a `= ∅`
  side-condition; `Reset` sets `W⁻ = M(p)`. Direction is then *derived* from which of `W⁻`/`W⁺`
  is non-empty. This is the undirected-topology idea, made precise for colored tokens.

The one caveat the current code already documents — **firing endomorphisms don't commute**, hence
`SingleArc` — becomes *more* important under HLPN, because multiset add/subtract still don't
commute with reset/inhibit. Keep `SingleArc` as the default topology constraint.

---

## 3. The machinery to add

### 3.1 Multiset places

We do not need to write the multiset type. A suitable one already exists in git history:
`Multiset[K, V, M <: AdditiveMonoid[V], O <: Order[K]]` (authored by George, recovered from
`ac15ef20d^`, formerly at `lib.cardano.value.multiasset.Multiset`). It is a canonical
`SortedMap[K, V]` where the *multiplicity* `V` is any spire additive monoid — so it is a near-1:1
match for ISO's `Bag` (Concept 2: a marking is a multiset; multiplicity is a generalized indicator
function). Recover and re-home it rather than reimplement:

- **Enabling and firing come for free.** `V = NonNegativeInt` is the natural-bag base case; signed
  bags (`SafeLong`) come free from the same code, which we need because firing computes
  `M(p) − W⁻(p,t)` — subtract in the abelian group, then check non-negativity. Its
  `AdditiveCommutativeGroup` / `CModule` gives `+`, `−`, `negate`, and scalar `n·x` (the
  `Weighted` inscription of §3.2) directly. Its `PartialOrder` *is* the sub-multiset relation, i.e.
  the enabling test `W(p,t)⟦β⟧ ≤ M(p)` (§3.4), with `NaN` for incomparable.
- **Performant.** `combineWith` is an O(n+m) tail-recursive merge-join over canonical sorted maps
  (zero entries dropped), not `map ++ map`.
- **No new dependency** — spire 0.18.0 is already a main dependency (`build.sbt:84`), and the
  algebra is law-checked (partialOrder / cModule / vectorSpace) transitively via `MultiAsset`.

Re-home was mechanical: moved to `hydrozoa.lib.collection`, made public, dropped the stray
`org.scalactic.Equality` reference, and simplified the signature to `Multiset[K, V]` with the
multiplicity monoid / key order as context-bound givens (`using AdditiveMonoid[V], Order[K]`)
rather than redundant `M`/`O` type parameters. (The dead `VectorSpace` Field-scalar instance was
dropped: it had no consumer after `MultiAsset.Fractional` left, and under the given-based
signature its only-forwarded evidence tripped `-Wunused`. `CModule` — the `n·x` scaling we do
need — stays.)

The petri layer then fixes the multiplicity carrier via a one-line facade:

```scala
/** Bag over color values C. P/T is MultiSet[Dot] ≅ ℕ. Multiplicity is a *safe signed* integer
  * (SafeLong: overflow-free, promotes to BigInt), not a natural — firing computes M(p) − W⁻ in
  * one abelian group; "multiplicity ≥ 0" is a place invariant checked at E1, not a carrier type. */
type MultiSet[C] = Multiset[C, SafeLong]
```

A colored place is then a straightforward instance of the *existing* `Place.Syntax` — it only
fixes `PlaceMarking` to a multiset. `Self` is fixed to `ColoredPlace[C]` (not F-bounded on the
leaf), mirroring the `RBRPlace` pattern: the base's `mark` returns the base and leaves override
covariantly, which is what a uniform `MapNet` `P` needs.

```scala
abstract class ColoredPlace[C]
    extends Place.Topology, Place.Syntax[ColoredPlace[C]], Place.Semantics[ColoredPlace[C]]:
    final type PlaceMarking = MultiSet[C]
    def colorDomain: Sort[C]                 // C(p): the declared color domain
    override def markingError: Option[MarkingError] = /* out-of-domain color OR negative count */
```

`markingError` is where the two E1 invariants land: every color present must belong to
`colorDomain` (`OutOfDomain`), and every multiplicity must be ≥ 0 (`NegativeMultiplicity`) — the
latter being how "a marking is a non-negative multiset" is enforced given the signed carrier.
`Bounded`, `HasFinalMarking`, and the rest of `Place.Semantics` apply unchanged. This confirms the
increment's goal: the Place metamodel absorbs multiset markings with *zero* changes to
`Place`/`Net`.

### 3.2 The expression DSL: sorts, signature, terms, variables

This is `Σ = (S, O)`, `V`, and `TERM(O∪V)`. It is a small, closed, typed expression language.
Two ISO distinctions drive the design and answer the task's "map builder with variables" vs
"coherent simulable net" question directly:

- A **`Term`** is a raw syntax tree: operators applied to sub-terms and variables. Constructing
  one always succeeds — this is the "map builder with some variables."
- A **well-sorted `Term`** type-checks against `Σ`: every operator's argument sorts line up and
  the term has a definite result sort. This is a *compilation step* (§5). Ill-sorted terms are
  rejected here, before any simulation.
- A **closed** term under a binding `β` (all free variables assigned) **evaluates** to a concrete
  multiset/boolean. Whether `β` is complete is exactly "are the variable assignments complete."

Implemented in `hydrozoa.lib.petri.hlpn` (`Sort`, `Term`, `Binding`). The term language is split
into ISO's three syntactic categories rather than one `Term[C]`, because that is exactly how
evaluation dispatches — to a color, a multiset, or a boolean — and it keeps each category's GADT
tight:

```scala
/** A color domain: the type of color a token carries, plus the finite structure the SN operators
  * need (a canonical Order doubling as MultiSet key order + ordered-class `<`; a subclass partition). */
sealed trait Sort[C]:  def order: Order[C]
object Sort:
    case object Dot extends Sort[Unit]                             // the P/T singleton • ; Bag(Dot) ≅ ℕ
    final case class Class[C](name: String, carrier: NonEmptyList[C],
        discipline: Discipline, subclasses: Map[String, Set[C]]) extends Sort[C]   // Concept 13/15
    final case class Prod[A, B](left: Sort[A], right: Sort[B]) extends Sort[(A, B)] // Concept 14
    enum Discipline: case Unordered, Linear, Circular             // governs Succ (Concept 16)

final case class Var[C](name: String, sort: Sort[C])              // ISO V

/** ColorTerm — basic color function (Concept 16): evaluates to one color. Variables ARE the
  * projections; `Succ` steps an ordered class; `Tuple` builds a product color. */
sealed trait ColorTerm[C]:  def sort: Sort[C]
object ColorTerm: /* Ref(Var) · Const(value, sort) · Succ(inner) · Tuple(left, right) */

/** Inscription — class color function W(p,t)/W(t,p) (Concept 17): evaluates to a multiset.
  * A positive linear combination — kept positive so an inscription never selects negative tokens. */
sealed trait Inscription[C]:  def sort: Sort[C]
object Inscription: /* Weighted(n, color) · All(sort) · SubclassAll(sort, name) · Union(l, r) */

/** Guard — Φ (Concept 15): evaluates to a boolean. */
sealed trait Guard
object Guard: /* True · Eq · Lt · InSubclass(color, name) · Not · And · Or */
```

`Binding` (opaque `Map[Var[?], Any]`) carries a candidate mode; `evalColor` / `evalInscription` /
`evalGuard` are the three closed-term evaluators, each `Either[EvalError, _]` — `Left` on an
unbound variable (incomplete binding), a `Succ` off an unordered/last color, or an unknown
subclass. Deferred beyond the SN fragment: infinite carriers, arithmetic/string operators, and
inscription difference.

Design stance: **model the symmetric-net operator set (Concept 16/17) as first-class, and put the
open-ended HLPN operators behind it.** That keeps mode-search decidable for the fragment we
actually need (finite peer sets, vote values, evacuee batches) and defers the undecidable general
case. It also lines up 1:1 with PNML conformance (§6).

### 3.3 Transition modes and the new arc semantics

A **binding** is a typed environment; a **mode** is a binding that enables a transition.

Implemented in `hlpn/ArcSemanticsH.scala`. Direction-neutral (§2.3): the arc carries no place/
transition order; consume-vs-produce is *which of `pre`/`post` is non-empty*. `pre`/`post` take the
current marking as well as the binding, because `Reset.pre` is the whole marking and the empty bag
is derived as `marking.filter(_ => false)` (reusing the marking's own `Order`, so `Inhibitor`/
`Reset`/output arcs need no `Order[C]` given).

```scala
trait ArcSemanticsH[C]:
    def pre(binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]]   // W⁻(β)
    def post(binding: Binding, marking: MultiSet[C]): Either[EvalError, MultiSet[C]]  // W⁺(β)
    def sideCondition(binding: Binding, marking: MultiSet[C]): Boolean               // e.g. inhibitor
    // derived, final:
    def enabled(binding, marking): Either[EvalError, Boolean]   // pre ≤ M(p) ∧ sideCondition
    def fire(binding, marking): Either[EvalError, MultiSet[C]]  // M − pre + post
```

The five Petri arcs are the cases, each over an `Inscription`: `Consume` (pre only), `Produce`
(post only), `Read` (`pre = post` — required present, net-zero), `Inhibitor` (`sideCondition:
M(p) = ∅`), `Reset` (`pre = M(p)`). Crucially `pre`/`post` are *functions of the binding*, which
is what the current unary `Arc.Semantics[P]` cannot represent. Wiring these into a net alongside a
mode-search simulator is §3.4 / increment 6.

### 3.4 Mode search (the enabling engine)

Enabling is now a *search*, and this is the genuinely new algorithmic content. Implemented in
`hlpn/ModeSearch.scala` against a minimal `hlpn/TransitionH.scala` (variables + guard + arcs) and a
marking `PlaceId => MultiSet[C]`:

```scala
def enabledModes(t, marking): LazyList[Binding]   // guard holds ∧ every arc enabled
def isEnabled(t, marking): Boolean                // = enabledModes.nonEmpty
def fire(t, mode, marking): Either[EvalError, Map[PlaceId, MultiSet[C]]]   // M − pre + post per arc
```

The MVP enumerates the **cartesian product of the variables' finite carriers** and filters by
`Φ⟦β⟧ ∧ ∀arc. arc.enabled(β, M(p))` — correct and terminating for the symmetric-net fragment,
exponential in the variable count. The unification-based prune (match each inscription against the
tokens actually in `M(p)` to build partial bindings, then join) is the future optimization, not
needed for correctness. A binding whose terms are undefined for it (e.g. `Succ` off a linear-class
end) is simply not a mode; structural ill-sortedness is the sort-checker's job (§5). Because arcs
are direction-neutral, enabling is "*all* arcs enabled" — output arcs never block — so `TransitionH`
needs no input/output split.

Two MVP simplifications, both handled at net integration (§3.5): a **single net color type `C`**,
and the marking passed in as a function rather than held by a net.

### 3.5 Assembling a net: per-place types, no universal color

`HlNet[PlaceId, TransitionId, ArcId, C]` (`hlpn/HlNet.scala`) holds colored places, transition
declarations, and arcs, and delegates enabling/firing to `ModeSearch`; firing checks each updated
place's E1 validity. The open question was how to hold places of *different* color domains (a
`Bag(Peer)` place next to a `Bag(Peer×Vote)` place) in one net.

Answer: **there is no universal net color.** Each place keeps its own color type, and the net is
their *erasure*, assembled by a typed builder (`hlpn/NetBuilder.scala`):

- `place[C](id, ColoredPlace[C])` hands back a typed `PlaceRef[PlaceId, C]`; `arc[C](id, ref:
  PlaceRef[…, C], t, ArcSemanticsH[C])` unifies the two `C`s, so a `Peer`-arc on a `Vote`-place is a
  **compile error**. That is the per-place-type safety, exactly at the one place a single color is
  meaningful (a place and the arc touching it) — transition variables stay `List[Var[?]]`, since a
  transition binds mixed colors.
- Building is a plain `State` monad (never fails, just records + issues refs); `build` runs a
  separate `ValidatedNel` pass that **accumulates** every id clash / dangling reference, then erases
  colors to `Any`. The erasure is sound because wiring was checked, and every downstream op is
  parametric in `C` (multiset ops need only the marking's stored `Order`), so the net runs at `Any`.
- `SortCheck` (§5) is the *runtime* counterpart of the builder's compile-time check, for nets
  decoded from PNML that arrive without static types. Belt for hand-authored, suspenders for
  imported.

---

## 4. Worked example: the two RBR blow-ups, folded

**Voting.** Today: `Unvoted`/`Voted` places × `Vote`/`Abstain`/`Ratchet` transitions, one copy
per peer — the unfolded P/T net. As HLPN:

- One color class `Peer = Finite({peer₁ … peerₙ})`.
- One place `Ballots : Bag(Peer × VoteValue)`, one place `Pending : Bag(Peer)`.
- One transition `CastVote` with variables `p : Peer`, `v : VoteValue`, input arc `⟨p⟩` from
  `Pending`, output arc `⟨(p, v)⟩` to `Ballots`, guard `Φ = (p has not yet voted)`.

`n` peers × 3 actions collapse from `O(n)` places and transitions to **one** transition; the
peer and the chosen action are *modes*, enumerated by the engine.

**Evacuation.** Today we cap evacuees per transition to keep the drawing legible. As HLPN, the
evacuation transition takes a variable-cardinality multiset via a `Weighted`/`all`-style
inscription — "remove *k* evacuees, `k` bound at firing" — so the artificial cap is gone; the
batch size is a mode, not a topology constant. (The current `EvacuationNet` already fakes this
with `payoutBatchSize` weights on `NonNegativeInt` places — colored inscriptions make it real.)

---

## 5. The compilation pipeline

The task asks to distinguish "a map builder with variables" from "a coherent net that could
actually be simulated," and where `ValidatedSimulator` fits. Four stages, each a total function
into a more-refined type:

```
 (1) build ──▶ HlNet          structurally present: places (+domains), transitions (vars+guard), arcs
        ▼
 (2) HlNet ──SortCheck.errors──▶ well-sorted?   every guard & arc inscription checks against Σ;
        │  SortError: undeclared var, Succ/Lt on unordered class, unknown subclass,
        ▼              arc inscription sort ≠ place domain (W(p,t) ∉ Bag(C(p)))
 (3) well-sorted HlNet ──enabledModes/fire──▶ simulation
            per firing: ModeSearch enumerates modes; fire checks E1 (ColoredPlace.markingError).
```

Implemented as `hlpn/HlNet.scala` + `hlpn/SortCheck.scala` (the standalone HLPN net, not
`MapNet`/`ValidatedSimulator` — those stay unary-P/T; `ArcSemanticsH` is not `Arc.Semantics[P]`).

- **Stage 2 is the "compilation step"** the task anticipated: `SortCheck.errors(net)` turns a bag
  of terms with variables into a net whose every annotation is guaranteed evaluable *once a
  complete binding is supplied*. Evaluation deliberately does not police these — `evalGuard` will
  compare an unordered class by carrier index — so the check must run before simulation. A
  `validate → Either[NonEmptyList[SortError], well-sorted]` opaque wrapper (mirroring
  `ValidatedSimulator`) is the natural next refinement.
- **Stage 3's partial-vs-complete distinction** is surfaced by `Binding`: `enabledModes` yields
  *complete* bindings (a partial one that can't be completed is not a mode); `fire(t, mode)` takes
  a complete mode. The engine's internal search carries partial bindings; only complete ones cross
  the API boundary.

---

## 6. ISO 15909-2 (PNML) codec — effort estimate

PNML is the XML transfer format for HLPNs (RELAX NG grammar in Annexes A/B; a full symmetric-net
example in Annex C). The document structure is easy; the **term/sort abstract syntax is the
cost**.

**What's cheap (1–2 days):** the object graph. `<pnml>/<net type=…>/<page>/{<place>,<transition>,
<arc source= target=>}`, plus `<name>`, `<graphics>` (skippable — purely cosmetic and optional),
`<toolspecific>` (opaque, pass-through). This is a flat structural walk; our `MapNet` maps onto it
directly (single page, no reference nodes needed).

**What's the work: structured labels.** HLPN annotations carry a `<structure>` holding a typed
term AST: `<hlinitialMarking>` (a multiset term), `<hlinscription>` (arc term), `<condition>`
(boolean guard), `<type>` (a sort), and a net-level `<declarations>` block (variable/sort/operator
declarations). The term grammar is **~16 sort tags + ~55–60 operator/constant tags ≈ 75 element
names**, each an ADT case with a small arity/typing rule, using a recurring
`<subterm>`-wrapped-n-ary-tree pattern with `refvariable`/`declaration` IDREF back-references.
Operator result/argument sorts are **not serialized** — a decoder must re-infer them (which our
Stage-2 sort-checker already does).

**Effort by conformance target** (the standard is explicitly layered — pick one):

| Target | What it covers | Term tags | Estimate |
|--------|----------------|-----------|----------|
| **P/T-in-HL-notation** (§2.5) | dot + multiset-of-dot only | ~6 | **~1 week** round-trip |
| **Symmetric Net** (§2.6) | finite classes, projection/successor/`all`, guards, no infinite sorts | ~25 | **~2–3 weeks** |
| **Structural HLPNG** (§2.4) | full term language incl. Int/String/List/arbitrary sorts | ~75 | **~5–8 weeks** |

**Recommendation.** Target **Symmetric Net** conformance. It is the exact fragment §3.2 makes
first-class, it round-trips everything the RBR model needs (finite peers, vote values, evacuee
batches), tools speak it natively, and it earns a real ISO 15909-2 conformance badge. Add three
things and the cost drops further: skip `<graphics>` (emit none, tolerate any on read), vendor the
RELAX NG grammars rather than fetch remote URIs, and drive the encode/decode from a single
tag↔constructor table shared by both directions. The `<subterm>` recursion is one walker written
once. The EMF-based **PNML Framework** (Annex D, Java) is a usable behavioral oracle for tests.

Non-obvious risks: (a) the standard's own case inconsistency (`<hlinitialMarking>` in Annex C vs
`hlinitialmarking` in Table 8 — accept both on read); (b) several well-formedness constraints
(same-page arcs, no reference cycles, operator arities) live in OCL/prose, *not* in the RELAX NG —
a validating decoder must enforce them itself, but our Stage-2/Stage-3 validation is the natural
home.

---

## 7. Recommended increments

Ordered so each step compiles and the RBR model keeps working throughout:

1. **`MultiSet[C]`**: recover George's `Multiset` from `ac15ef20d^`, re-home to a neutral lib
   package, make it public, add the `MultiSet[C]` facade (§3.1), and port a direct laws suite
   (its `Gen` exists to seed it). Standalone; no framework change.
2. **`Sort` / `Term` / `Var` / `Binding`** DSL with closed-term `eval`. Standalone; unit-tested
   against hand-written terms. This is the DSL from §3.2.
3. **`ColoredPlace[C]`** as a `Place.Syntax`/`Place.Semantics` instance (§3.1). Proves the
   metamodel absorbs multiset markings with zero changes to `Place`/`Net`.
4. **`ArcSemanticsH` (pre/post/side-condition)** + re-express PT/TP/Read/Inhibitor/Reset over it
   (§3.3). The unary `Arc.Semantics` stays for the P/T path; HLPN is the parallel path.
5. **Sort-checker** → new `ValidationError.SortInvalid` in `ValidatedSimulator` (Stage 2, §5).
6. **Mode-search engine** for the symmetric-net fragment: `enabledModes(t)` + `fire(t, mode)`
   (§3.4). New `HlSimulator` alongside `SequentialSimulator`.
7. **Port the RBR voting sub-net** to one colored `CastVote` transition (§4) — the acceptance
   test that the fold actually kills the `Unvoted`/`Voted` explosion.
8. **PNML symmetric-net codec** (§6), once 1–6 exist and give it a typed target.

Steps 1–3 are pure additions with no risk to existing code. Step 4 is where the "keep unary vs
go mode-relative" decision is committed; §2.3 argues for keeping topology direction-neutral and
carrying direction in `pre`/`post`. Steps 6–7 are the payoff.
