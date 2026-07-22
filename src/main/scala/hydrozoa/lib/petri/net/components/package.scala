package hydrozoa.lib.petri.net

/** Package-level documentation for [[hydrozoa.lib.petri.net.components]].
  *
  * TODO: Document the following patterns used throughout this package:
  *   - F-bounded polymorphism (`Self <: Trait[Self]` + `self: Self =>`) for type-preserving
  *     `withXyz` and `mark` methods on mixin traits
  *   - The ontological ordering Topology → Syntax → Semantics → Presentation and what each layer is
  *     responsible for
  *
  * ## Enabling and firing are net-level rules
  *
  * Components carry no behavior: an arc is its flow element plus annotation ([[Arc.Flow]],
  * [[Arc.Syntax]]); a place carries its marking and marking invariants. The ISO 15909-1 enabling
  * rule (`M(p) ≥ W(p,t)`, Concept 10) and firing rule (`M − W(p,t) + W(t,p)`, Concept 12) are
  * implemented by the simulator. Enabling conditions compose under conjunction (commutative), and
  * the per-place firing update sums annotations (commutative) — evaluation order never affects
  * correctness.
  */
package object components
