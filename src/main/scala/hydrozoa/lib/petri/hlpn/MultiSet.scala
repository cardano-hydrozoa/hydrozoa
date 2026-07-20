package hydrozoa.lib.petri.hlpn

import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.petri.net.MarkingAlgebra
import spire.math.SafeLong

/** A bag over color values C — an HLPN place marking `M(p) ∈ Bag(C(p))`. The multiplicity is the
  * safe signed integer [[SafeLong]]: firing computes `M(p) − W⁻` in one abelian group, and
  * "multiplicity ≥ 0" is enforced as a place invariant (see [[ColoredPlace.markingError]]), not by
  * the carrier type. `MultiSet[Unit] ≅ ℕ`, so a Dot-sorted place is an ordinary P/T place.
  */
type MultiSet[C] = Multiset[C, SafeLong]

/** The HLPN instance of the framework's token algebra (ISO Concepts 23/24): enabling is the
  * sub-multiset test `W(p,t)⟦β⟧ ≤ M(p)`, firing the multiset group operations. Instantiated at the
  * *evaluated* annotation — inscriptions are evaluated under a mode before reaching the algebra.
  */
given multisetAlgebra[C]: MarkingAlgebra[MultiSet[C], MultiSet[C]] with {
    def covers(m: MultiSet[C], w: MultiSet[C]): Boolean =
        w.multiplicityMap.forall((c, n) => n <= m.get(c))
    def minus(m: MultiSet[C], w: MultiSet[C]): MultiSet[C] = m.combineWith(w)(_ - _)
    def plus(m: MultiSet[C], w: MultiSet[C]): MultiSet[C] = m.combineWith(w)(_ + _)
}
