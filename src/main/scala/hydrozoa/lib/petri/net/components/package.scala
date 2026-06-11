package hydrozoa.lib.petri.net

/** Package-level documentation for [[hydrozoa.lib.petri.net.components]].
  *
  * TODO: Document the following patterns used throughout this package:
  *   - F-bounded polymorphism (`Self <: Trait[Self]` + `self: Self =>`) for type-preserving
  *     `withXyz` and `mark` methods on mixin traits
  *   - Stackable trait / abstract override pattern for accumulating lists of predicates or
  *     endomorphisms through mixin linearization (see e.g. [[Place.Syntax.HasTokens]],
  *     [[Arc.Semantics.PT]])
  *   - The ontological ordering Topology → Syntax → Semantics → Presentation and what each layer is
  *     responsible for
  *   - The split between arc-side enabling ([[Arc.Semantics]]) and place-side validity
  *     ([[Place.Semantics.validMarking]]), and why coherence is checked at the simulator level
  *
  * ## Enabling predicate commutativity
  *
  * Enabling predicates at all levels (place-side, arc-side, transition-side, net-wide) are composed
  * under conjunction (forall / logical AND). Conjunction is commutative and associative, so the
  * _order_ in which predicates from different levels (or within the same level) are evaluated does
  * not affect correctness — only short-circuit performance. This means net-wide enabling can be
  * computed as:
  *
  * `placePreds.forall && arcPreds.forall && transitionPreds.forall && netPreds.forall`
  *
  * in any order without loss of generality. We can refine this claim (e.g., with a formal
  * definition of what "net-wide enabling" means for composed nets) later.
  *
  * ## Firing endomorphism commutativity (or lack thereof)
  *
  * Unlike enabling predicates, firing endomorphisms do NOT commute in general. For example, a PT(1)
  * arc followed by a Reset arc on the same place yields a different result than Reset followed by
  * PT(1). The resolution of this depends on the net topology:
  *   - The standard approach (and the default) is to restrict to _at most one arc per (place,
  *     transition) direction_ at the topology level. This makes firing trivially order-independent.
  *   - More permissive topologies must either impose an explicit arc ordering, or restrict arc
  *     combinations to those that can be proven (or property-tested) to commute.
  * See also [[hydrozoa.lib.petri.net.Topology]].
  */
package object components
