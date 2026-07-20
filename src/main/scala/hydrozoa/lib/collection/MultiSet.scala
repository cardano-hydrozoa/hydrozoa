package hydrozoa.lib.collection

import cats.implicits.catsKernelOrderingForOrder
import scala.collection.immutable.SortedMap
import spire.algebra.*
import spire.std.int.IntAlgebra

/** A multiset (AKA a "bag") allows for multiple instances to exist for each of its elements,
  * generalizing the concept of a set (which only allows one instance per element).
  *
  * The number of instances for each element in a multiset is called the "multiplicity" of that
  * element in the set, which generalizes the indicator function of sets.
  *
  * The multiplicity function is implemented via a map from keys to values. In the simplest case,
  * the values are natural numbers directly representing the number of instances per key, defaulting
  * to zero for missing keys. However, the value type is only required to be an additive monoid in
  * this class, which allows for more general concepts of multiplicity (finite, signed,
  * rational-valued, nested, etc.).
  *
  * For the purposes of defining a multiset's multiplicity function, two maps are equivalent if they
  * give the same value for a given key. Therefore, this class canonicalizes its multiplicity map by
  * sorting its keys, removing zero-valued entries, and setting the default to zero.
  *
  * @param multiplicityMap
  *   a sorted map with additively monoidal values (only the additive identity is used here).
  */
case class Multiset[K, V] private (
    multiplicityMap: SortedMap[K, V]
)(using vMonoid: AdditiveMonoid[V], kOrder: Order[K]) {

    /** Lookup the value in the multiset's canonical map that corresponds to the given key.
      */
    def get(k: K): V = multiplicityMap.getOrElse(k, vMonoid.zero)

    /** Apply a zero-preserving function to every value in the multiset's canonical map.
      *
      * Precondition: `f(V.zero) == VNew.zero`. The function is only applied to the finitely many
      * non-zero entries; missing keys remain at `VNew.zero` in the result. If `f` does not preserve
      * zero, the result does not agree with the semantic multiplicity function `k => f(get(k))`.
      */
    private[collection] def mapValues[VNew](
        f: V => VNew
    )(using AdditiveMonoid[VNew]): Multiset[K, VNew] =
        Multiset.apply(multiplicityMap.view.mapValues(f).to(SortedMap))

    def combineWith[VOther, VResult](
        other: Multiset[K, VOther]
    )(combiner: (V, VOther) => VResult)(using
        AdditiveMonoid[VOther],
        AdditiveMonoid[VResult]
    ): Multiset[K, VResult] = MultisetOps.combineWith(this, other)(combiner)
}

object Multiset extends MultisetInstances1 {

    /** Create a multiset.
      *
      * @param sortedMap
      *   a sorted map with additively monoidal values (only the additive identity is used here).
      * @return
      *   a multiset with a [[V]]-valued multiplicity function corresponding to the canonicalized
      *   sorted map. Canonicalizing a sorted map means removing all zero-valued entries; missing
      *   keys default to zero via [[Multiset.get]].
      */
    def apply[K, V](
        sortedMap: SortedMap[K, V]
    )(using vMonoid: AdditiveMonoid[V], kOrder: Order[K]): Multiset[K, V] =
        new Multiset[K, V](sortedMap.filterNot(_._2 == vMonoid.zero))

    def empty[K, V](using
        AdditiveMonoid[V],
        Order[K]
    ): Multiset[K, V] =
        new Multiset[K, V](SortedMap.empty)

    /** Sub-multiset partial order: `a ≤ b` iff `a(k) ≤ b(k)` for every key. */
    given partialOrder[K, V](using
        kOrder: Order[K],
        vMonoid: AdditiveMonoid[V],
        vOrder: Order[V]
    ): PartialOrder[Multiset[K, V]] with {
        override def eqv(x: Multiset[K, V], y: Multiset[K, V]): Boolean =
            x.multiplicityMap == y.multiplicityMap

        override def partialCompare(x: Multiset[K, V], y: Multiset[K, V]): Double =
            // Compare pairwise across every key present in either map. Missing keys default to
            // zero (via combineWith's monoidal fill), so equal comparisons at any key drop out of
            // the canonical result. The remaining non-zero comparisons must all agree in sign for
            // the multisets to be comparable.
            val comparisons =
                MultisetOps.combineWith(x, y)(vOrder.compare).multiplicityMap.values
            comparisons.headOption.fold(0d) { first =>
                if comparisons.forall(_ == first) then first.toDouble else Double.NaN
            }
    }

    given additiveAbGroup[K, V](using
        kOrder: Order[K],
        vGroup: AdditiveAbGroup[V]
    ): AdditiveAbGroup[Multiset[K, V]] with {
        override def zero: Multiset[K, V] = Multiset.empty
        override def plus(x: Multiset[K, V], y: Multiset[K, V]): Multiset[K, V] =
            MultisetOps.combineWith(x, y)(vGroup.plus)
        override def negate(x: Multiset[K, V]): Multiset[K, V] =
            x.mapValues(vGroup.negate)
        override def minus(x: Multiset[K, V], y: Multiset[K, V]): Multiset[K, V] =
            MultisetOps.combineWith(x, y)(vGroup.minus)
    }
}

/** Lower-priority instances for [[Multiset]]. Placed on a parent trait so a direct summon for
  * `AdditiveAbGroup[Multiset[K, V]]` prefers the explicit companion given (which has no free scalar
  * type to pin down) over `cModule`'s inherited-via-CModule alternative.
  */
private[collection] sealed trait MultisetInstances1 {
    given cModule[K, V, S](using
        kOrder: Order[K],
        sRing: CRing[S],
        vModule: CModule[V, S]
    ): CModule[Multiset[K, V], S] with {
        override def scalar: CRing[S] = sRing
        override def zero: Multiset[K, V] = Multiset.empty
        override def plus(x: Multiset[K, V], y: Multiset[K, V]): Multiset[K, V] =
            MultisetOps.combineWith(x, y)(vModule.plus)
        override def negate(x: Multiset[K, V]): Multiset[K, V] =
            x.mapValues(vModule.negate)
        override def timesl(s: S, x: Multiset[K, V]): Multiset[K, V] =
            x.mapValues(vModule.timesl(s, _))
    }
}

private object MultisetOps {
    def combineWith[K, VSelf, VOther, VResult](
        self: Multiset[K, VSelf],
        other: Multiset[K, VOther]
    )(combiner: (VSelf, VOther) => VResult)(using
        kOrder: Order[K],
        vSelfMonoid: AdditiveMonoid[VSelf],
        vOtherMonoid: AdditiveMonoid[VOther],
        vResultMonoid: AdditiveMonoid[VResult]
    ): Multiset[K, VResult] = {
        import scala.annotation.tailrec
        import scala.collection.mutable
        import scala.math.Ordered.orderingToOrdered

        val selfIterator: Iterator[(K, VSelf)] = self.multiplicityMap.iterator
        val otherIterator: Iterator[(K, VOther)] = other.multiplicityMap.iterator
        val resultBuilder: mutable.Builder[(K, VResult), SortedMap[K, VResult]] =
            SortedMap.newBuilder

        inline def processBoth(x: (K, VSelf), y: (K, VOther)): (K, VResult) =
            (x._1, combiner(x._2, y._2))

        inline def processSelf(x: (K, VSelf)): (K, VResult) =
            (x._1, combiner(x._2, vOtherMonoid.zero))

        inline def processOther(y: (K, VOther)): (K, VResult) =
            (y._1, combiner(vSelfMonoid.zero, y._2))

        inline def appendNonZero(
            builder: mutable.Builder[(K, VResult), SortedMap[K, VResult]],
            z: (K, VResult)
        ): Unit = if z._2 == vResultMonoid.zero then () else builder += z

        inline def concatNonZero(
            builder: mutable.Builder[(K, VResult), SortedMap[K, VResult]],
            m: Iterator[(K, VResult)]
        ): Unit = builder ++= m.iterator.filterNot(_._2 == vResultMonoid.zero)

        // Warning: this function mutates its arguments!
        @tailrec
        def loop(
            x: (K, VSelf),
            xs: Iterator[(K, VSelf)],
            y: (K, VOther),
            ys: Iterator[(K, VOther)],
            builder: mutable.Builder[(K, VResult), SortedMap[K, VResult]]
        ): SortedMap[K, VResult] = {
            if x._1 < y._1 then {
                // Process `x` now and `y` later
                appendNonZero(builder, processSelf(x))
                if xs.hasNext then {
                    // Continue looping with the next `x` and same `y`
                    val xNext = xs.next()
                    loop(xNext, xs, y, ys, builder)
                } else {
                    // Process `y` and the remaining `ys` and return
                    appendNonZero(builder, processOther(y))
                    concatNonZero(builder, ys.map(processOther))
                    builder.result()
                }
            } else if x._1 == y._1 then {
                // Process both `x` and `y` now
                appendNonZero(builder, processBoth(x, y))
                if xs.hasNext then
                    if ys.hasNext then {
                        // Continue looping with the next `x` and next `y`
                        val xNext = xs.next()
                        val yNext = ys.next()
                        loop(xNext, xs, yNext, ys, builder)
                    } else {
                        // Process the remaining `xs` and return
                        concatNonZero(builder, xs.map(processSelf))
                        builder.result()
                    }
                else {
                    // Process the remaining `ys` and return
                    concatNonZero(builder, ys.map(processOther))
                    builder.result()
                }
            } else {
                // Process `y` now and `x` later
                appendNonZero(builder, processOther(y))
                if ys.hasNext then {
                    // Continue looping with the same `x` and next `y`
                    val yNext = ys.next()
                    loop(x, xs, yNext, ys, builder)
                } else {
                    // Process `x` and the remaining `xs` and return
                    appendNonZero(builder, processSelf(x))
                    concatNonZero(builder, xs.map(processSelf))
                    builder.result()
                }
            }
        }

        val newCanonicalMap = if selfIterator.hasNext then {
            if otherIterator.hasNext then {
                // Start looping with the first entries in `self` and `other`
                val selfFirst = selfIterator.next()
                val otherFirst = otherIterator.next()
                loop(selfFirst, selfIterator, otherFirst, otherIterator, resultBuilder)
            } else {
                // Process the entries in `self` and return
                concatNonZero(resultBuilder, selfIterator.map(processSelf))
                resultBuilder.result()
            }
        } else {
            // Process the entries in `other` and return
            concatNonZero(resultBuilder, otherIterator.map(processOther))
            resultBuilder.result()
        }

        Multiset(newCanonicalMap)
    }
}
