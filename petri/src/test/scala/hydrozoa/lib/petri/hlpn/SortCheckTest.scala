package hydrozoa.lib.petri.hlpn

import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.petri.Positive
import hydrozoa.lib.petri.net.components.Arc.Flow
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.SortedMap
import spire.algebra.Order
import spire.math.SafeLong

class SortCheckTest extends AnyFunSuite:

    private given Order[String] = Order.from((a, b) => a.compareTo(b))

    private val peer = Sort
        .Class(
          "Peer",
          NonEmptySet.of("p0", "p1", "p2"),
          Sort.Discipline.Unordered,
          Map("evens" -> Set("p0", "p2"), "rest" -> Set("p1"))
        )
        .getOrElse(fail("peer fixture is not a genuine partition"))
    private val vote =
        Sort.Class("Vote", NonEmptySet.of("No", "Yes"), Sort.Discipline.Linear)

    private def ms(entries: (String, Int)*): MultiSet[String] =
        Multiset(entries.map((k, v) => k -> SafeLong(v)).to(SortedMap))

    private case class Tokens(marking: MultiSet[String], colorDomain: Sort[String])
        extends ColoredPlace[String]:
        def mark(m: MultiSet[String]): Tokens = copy(marking = m)

    private val p = Var("p", peer)
    private val v = Var("v", vote)
    private val wp = Inscription.Weighted(Positive.unsafe(1), ColorTerm.Ref(p))

    /** A one-place, one-transition, one-input-arc net for exercising a single term. */
    private def netWith(
        variables: List[Var[String]],
        guard: Guard,
        inscription: Inscription[String],
        placeDomain: Sort[String] = peer
    ): HlNet[String, String, String] =
        HlNet(
          placesMap = Map[String, ColoredPlace[String]]("in" -> Tokens(ms(), placeDomain)),
          transitionsMap = Map("t" -> HlTransition(variables, guard)),
          arcsMap = Map(Flow.Pt("in", "t") -> InscribedArc(inscription))
        )

    test("a well-sorted net has no errors") {
        assert(SortCheck.errors(netWith(List(p), Guard.True, wp)).isEmpty)
    }

    test("succ over an unordered class is rejected") {
        val inscription =
            Inscription.Weighted(Positive.unsafe(1), ColorTerm.Succ(ColorTerm.Ref(p)))
        assert(
          SortCheck.errors(netWith(List(p), Guard.True, inscription)).exists {
              case _: SortError.SuccOnUnordered => true
              case _                            => false
          }
        )
    }

    test("lt over an unordered class is rejected") {
        val g = Guard.Lt(ColorTerm.Ref(p), ColorTerm.Const("p1", peer))
        assert(
          SortCheck.errors(netWith(List(p), g, wp)).exists {
              case _: SortError.LtOnUnordered => true
              case _                          => false
          }
        )
    }

    test("an unknown subclass is rejected") {
        val g = Guard.InSubclass(ColorTerm.Ref(p), "odds")
        assert(
          SortCheck.errors(netWith(List(p), g, wp)).exists {
              case _: SortError.UnknownSubclass => true
              case _                            => false
          }
        )
    }

    test("a variable the transition does not declare is rejected") {
        val g = Guard.Eq(ColorTerm.Ref(v), ColorTerm.Const("No", vote))
        assert(
          SortCheck.errors(netWith(List(p), g, wp)).exists {
              case _: SortError.UndeclaredVariable => true
              case _                               => false
          }
        )
    }

    test("an arc inscription whose sort is not the place domain is rejected") {
        val wv = Inscription.Weighted(Positive.unsafe(1), ColorTerm.Ref(v))
        val net = netWith(List(p, v), Guard.True, wv)
        assert(SortCheck.errors(net).exists {
            case _: SortError.ArcDomainMismatch => true
            case _                              => false
        })
    }

    // The enumerated-sub-domain relaxation: a `Sort.Class` place domain whose carrier is a subset of
    // the inscription's sort is accepted — exact membership is a marking invariant, not static. (The
    // product-into-class case is exercised by the RBR model; here both sorts are String classes.)
    test("an enumerated sub-domain accepts an inscription of a larger sort") {
        val evenPeers = Sort.Class("EvenPeer", NonEmptySet.of("p0", "p2"), Sort.Discipline.Unordered)
        val net = netWith(List(p), Guard.True, wp, placeDomain = evenPeers)
        assert(!SortCheck.errors(net).exists {
            case _: SortError.ArcDomainMismatch => true
            case _                              => false
        })
    }

    test("a sub-domain carrying a color outside the inscription's sort is still rejected") {
        val weird = Sort.Class("Weird", NonEmptySet.of("p0", "pX"), Sort.Discipline.Unordered)
        val net = netWith(List(p), Guard.True, wp, placeDomain = weird)
        assert(SortCheck.errors(net).exists {
            case _: SortError.ArcDomainMismatch => true
            case _                              => false
        })
    }

    // (A) Peer and Vote are both `Sort.Class` over `String`, so `Eq`/`Lt` across them typecheck.
    test("a guard comparing operands of different sorts is rejected") {
        val g = Guard.Eq(ColorTerm.Ref(p), ColorTerm.Const("No", vote))
        assert(SortCheck.errors(netWith(List(p), g, wp)).exists {
            case SortError.OperandSortMismatch("Eq", _, _) => true
            case _                                         => false
        })
    }

    // (B) A Union whose left branch matches the place domain but whose right branch does not — the
    // old check only inspected the root sort (= the left branch), missing this.
    test("a union inscription whose non-left branch mismatches the place domain is rejected") {
        val wv = Inscription.Weighted(Positive.unsafe(1), ColorTerm.Ref(v))
        val net = netWith(List(p, v), Guard.True, Inscription.Union(wp, wv))
        assert(SortCheck.errors(net).exists {
            case _: SortError.ArcDomainMismatch => true
            case _                              => false
        })
    }

    // (C) An arc to a transition absent from the net: reported cleanly, without a spurious
    // UndeclaredVariable per referenced variable.
    test("an arc referencing a missing transition is reported without a variable cascade") {
        val net = HlNet[String, String, String](
          placesMap = Map[String, ColoredPlace[String]]("in" -> Tokens(ms(), peer)),
          transitionsMap = Map("t" -> HlTransition(List(p), Guard.True)),
          arcsMap = Map(Flow.Pt("in", "ghost") -> InscribedArc(wp))
        )
        val errs = SortCheck.errors(net)
        val _ = assert(errs.exists {
            case SortError.MissingTransition("ghost") => true
            case _                                    => false
        })
        assert(!errs.exists {
            case _: SortError.UndeclaredVariable => true
            case _                               => false
        })
    }
