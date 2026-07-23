package hydrozoa.lib.petri

import spire.math.Natural

/** A strictly positive count (≥ 1) — an arc weight, per ISO 15909-1. The flow relation `F` holds
  * only positive-weight arcs: a weight of 0 denotes the *absence* of an arc, not a present arc that
  * moves nothing (an inert-at-firing but structurally phantom edge that misleads incidence/siphon/
  * trap analysis). Backed by spire's [[Natural]], so the framework depends only on spire.
  */
opaque type Positive = Natural

object Positive:

    /** `Some(n)` iff `n > 0`. */
    def apply(n: Int): Option[Positive] = Option.when(n > 0)(Natural(n))

    /** `n` as a `Positive`; throws `IllegalArgumentException` if `n <= 0`. */
    def unsafe(n: Int): Positive =
        require(n > 0, s"Positive requires n > 0, got $n")
        Natural(n)

    extension (p: Positive)
        /** The underlying non-negative value. */
        def toNatural: Natural = p
        def toLong: Long = p.toLong
