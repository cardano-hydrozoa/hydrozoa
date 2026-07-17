package hydrozoa.lib.petri.hlpn

import hydrozoa.lib.collection.Multiset
import spire.math.SafeLong

/** A bag over color values C — an HLPN place marking `M(p) ∈ Bag(C(p))`. The multiplicity is the
  * safe signed integer [[SafeLong]]: firing computes `M(p) − W⁻` in one abelian group, and
  * "multiplicity ≥ 0" is enforced as a place invariant (see [[ColoredPlace.markingError]]), not by
  * the carrier type. `MultiSet[Unit] ≅ ℕ`, so a Dot-sorted place is an ordinary P/T place.
  */
type MultiSet[C] = Multiset[C, SafeLong]
