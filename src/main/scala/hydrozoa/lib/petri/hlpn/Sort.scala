package hydrozoa.lib.petri.hlpn

import cats.data.NonEmptyList
import spire.algebra.Order

/** A sort names a color domain — the type of color a token carries — together with the finite
  * structure the symmetric-net operators need: a total order (used both as the [[MultiSet]] key
  * order and, for ordered classes, as the semantic `<` of guards) and a static partition into named
  * subclasses (ISO 15909-1 Concept 15).
  *
  * Symmetric-net-first: sorts are finite color classes (Concept 13) and their cartesian products
  * (Concept 14, the color domains). Infinite HLPN carriers are deferred.
  */
sealed trait Sort[C]:
    /** Canonical total order over the carrier — the [[MultiSet]] key order for markings of this
      * sort, and the comparison used by ordered-class `<` guards.
      */
    def order: Order[C]

object Sort:

    /** The singleton color `•`. `Bag(Dot) ≅ ℕ`, so a Dot-typed place is an ordinary P/T place and a
      * P/T net is the sub-case of an HLPN over this one sort.
      */
    case object Dot extends Sort[Unit]:
        def order: Order[Unit] = Order.from((_, _) => 0)

    /** A finite color class (Concept 13): an ordered carrier, a discipline governing the successor
      * function, and a static partition into named subclasses (Concept 15).
      */
    final case class Class[C](
        name: String,
        carrier: NonEmptyList[C],
        discipline: Discipline,
        subclasses: Map[String, Set[C]]
    ) extends Sort[C]:
        private lazy val rank: Map[C, Int] = carrier.toList.zipWithIndex.toMap
        def order: Order[C] =
            Order.from((a, b) => Integer.compare(rank.getOrElse(a, -1), rank.getOrElse(b, -1)))

    /** A color domain that is the cartesian product of two sorts (Concept 14). N-ary domains nest
      * to the right.
      */
    final case class Prod[A, B](left: Sort[A], right: Sort[B]) extends Sort[(A, B)]:
        def order: Order[(A, B)] = Order.from { (x, y) =>
            val c = left.order.compare(x._1, y._1)
            if c != 0 then c else right.order.compare(x._2, y._2)
        }

    /** Whether a class carrier admits the successor function (Concept 16 `X++`): `Circular` wraps,
      * `Linear` has no successor for the last element, `Unordered` has no successor at all.
      */
    enum Discipline:
        case Unordered, Linear, Circular
