package hydrozoa.lib.petri.hlpn

import cats.data.NonEmptySet
import org.scalatest.funsuite.AnyFunSuite
import spire.algebra.Order

class SortTest extends AnyFunSuite:

    private given Order[String] = Order.from((a, b) => a.compareTo(b))

    private val peers = NonEmptySet.of("p0", "p1", "p2")

    test("undivided class is total and has no declared subclasses") {
        val c = Sort.Class("Peer", peers, Sort.Discipline.Unordered)
        assert(c.subclasses.isEmpty)
    }

    test("an empty subclass map is the undivided class") {
        assert(
          Sort.Class("Peer", peers, Sort.Discipline.Unordered, Map.empty).map(_.subclasses)
              == Right(Map.empty)
        )
    }

    test("a genuine partition is accepted") {
        val partition = Map("evens" -> Set("p0", "p2"), "rest" -> Set("p1"))
        assert(
          Sort.Class("Peer", peers, Sort.Discipline.Unordered, partition).map(_.subclasses)
              == Right(partition)
        )
    }

    test("an empty block is rejected") {
        assert(
          Sort.Class(
            "Peer",
            peers,
            Sort.Discipline.Unordered,
            Map("evens" -> Set("p0", "p2"), "rest" -> Set("p1"), "none" -> Set.empty)
          ).isLeft
        )
    }

    test("a color outside the carrier is rejected") {
        assert(
          Sort.Class(
            "Peer",
            peers,
            Sort.Discipline.Unordered,
            Map("evens" -> Set("p0", "p2"), "rest" -> Set("p1", "p3"))
          ).isLeft
        )
    }

    test("overlapping blocks are rejected") {
        assert(
          Sort.Class(
            "Peer",
            peers,
            Sort.Discipline.Unordered,
            Map("a" -> Set("p0", "p1"), "b" -> Set("p1", "p2"))
          ).isLeft
        )
    }

    test("a partition that does not cover the carrier is rejected") {
        assert(
          Sort.Class(
            "Peer",
            peers,
            Sort.Discipline.Unordered,
            Map("evens" -> Set("p0", "p2"))
          ).isLeft
        )
    }
