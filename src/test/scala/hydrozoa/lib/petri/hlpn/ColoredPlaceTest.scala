package hydrozoa.lib.petri.hlpn

import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.SortedMap
import spire.algebra.Order
import spire.math.SafeLong

class ColoredPlaceTest extends AnyFunSuite:

    private given Order[String] = Order.from((a, b) => a.compareTo(b))

    private val peer =
        Sort.Class("Peer", Set("p0", "p1", "p2"), Sort.Discipline.Unordered, Map.empty)

    private def ms(entries: (String, Int)*): MultiSet[String] =
        Multiset(entries.map((k, v) => k -> SafeLong(v)).to(SortedMap))

    /** A minimal concrete colored place: a bag of colors over a fixed domain. */
    private case class Tokens(marking: MultiSet[String], colorDomain: Sort[String])
        extends ColoredPlace[String]:
        def mark(m: MultiSet[String]): Tokens = copy(marking = m)

    test("a marking within the color domain is valid") {
        val p = Tokens(ms("p0" -> 2, "p1" -> 1), peer)
        val _ = assert(p.markingError.isEmpty)
        assert(p.validMarking)
    }

    test("a color outside the domain is invalid") {
        val p = Tokens(ms("pX" -> 1), peer)
        assert(p.markingError.contains(ColoredPlace.OutOfDomain("pX")))
    }

    test("a negative multiplicity is invalid") {
        val p = Tokens(ms("p0" -> -1), peer)
        assert(p.markingError.contains(ColoredPlace.NegativeMultiplicity("p0", SafeLong(-1))))
    }

    test("markValid rejects an out-of-domain marking; mark updates the place") {
        val p = Tokens(ms("p0" -> 1), peer)
        val _ = assert(p.markValid(ms("pX" -> 1)).isLeft)
        assert(p.mark(ms("p2" -> 3)).marking.get("p2") == SafeLong(3))
    }
