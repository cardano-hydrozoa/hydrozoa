package hydrozoa.lib.petri.hlpn

import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.number.PositiveInt
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.SortedMap
import spire.algebra.Order
import spire.math.SafeLong

class SortCheckTest extends AnyFunSuite:

    private given Order[String] = Order.from((a, b) => a.compareTo(b))

    private val peer = Sort.Class(
      "Peer",
      NonEmptySet.of("p0", "p1", "p2"),
      Sort.Discipline.Unordered,
      Map("evens" -> Set("p0", "p2"))
    )
    private val vote =
        Sort.Class("Vote", NonEmptySet.of("No", "Yes"), Sort.Discipline.Linear, Map.empty)

    private def ms(entries: (String, Int)*): MultiSet[String] =
        Multiset(entries.map((k, v) => k -> SafeLong(v)).to(SortedMap))

    private case class Tokens(marking: MultiSet[String], colorDomain: Sort[String])
        extends ColoredPlace[String]:
        def mark(m: MultiSet[String]): Tokens = copy(marking = m)

    private val p = Var("p", peer)
    private val v = Var("v", vote)
    private val wp = Inscription.Weighted(PositiveInt.unsafeApply(1), ColorTerm.Ref(p))

    /** A one-place, one-transition, one-arc net for exercising a single term. */
    private def netWith(
        variables: List[Var[String]],
        guard: Guard,
        arc: ArcSemanticsH[String],
        placeDomain: Sort[String] = peer
    ): HlNet[String, String, String, String] =
        HlNet(
          places = Map[String, ColoredPlace[String]]("in" -> Tokens(ms(), placeDomain)),
          transitions = Map("t" -> HlNet.Transition(variables, guard)),
          arcs = Map[String, HlNet.Arc[String, String, String]]("a" -> HlNet.Arc("in", "t", arc))
        )

    test("a well-sorted net has no errors") {
        assert(SortCheck.errors(netWith(List(p), Guard.True, ArcSemanticsH.Consume(wp))).isEmpty)
    }

    test("succ over an unordered class is rejected") {
        val sem = ArcSemanticsH.Consume(
          Inscription.Weighted(PositiveInt.unsafeApply(1), ColorTerm.Succ(ColorTerm.Ref(p)))
        )
        assert(
          SortCheck.errors(netWith(List(p), Guard.True, sem)).exists {
              case _: SortError.SuccOnUnordered => true
              case _                            => false
          }
        )
    }

    test("lt over an unordered class is rejected") {
        val g = Guard.Lt(ColorTerm.Ref(p), ColorTerm.Const("p1", peer))
        assert(
          SortCheck.errors(netWith(List(p), g, ArcSemanticsH.Consume(wp))).exists {
              case _: SortError.LtOnUnordered => true
              case _                          => false
          }
        )
    }

    test("an unknown subclass is rejected") {
        val g = Guard.InSubclass(ColorTerm.Ref(p), "odds")
        assert(
          SortCheck.errors(netWith(List(p), g, ArcSemanticsH.Consume(wp))).exists {
              case _: SortError.UnknownSubclass => true
              case _                            => false
          }
        )
    }

    test("a variable the transition does not declare is rejected") {
        val g = Guard.Eq(ColorTerm.Ref(v), ColorTerm.Const("No", vote))
        assert(
          SortCheck.errors(netWith(List(p), g, ArcSemanticsH.Consume(wp))).exists {
              case _: SortError.UndeclaredVariable => true
              case _                               => false
          }
        )
    }

    test("an arc inscription whose sort is not the place domain is rejected") {
        val wv = Inscription.Weighted(PositiveInt.unsafeApply(1), ColorTerm.Ref(v))
        val net = netWith(List(p, v), Guard.True, ArcSemanticsH.Consume(wv))
        assert(SortCheck.errors(net).exists {
            case _: SortError.ArcDomainMismatch => true
            case _                              => false
        })
    }
