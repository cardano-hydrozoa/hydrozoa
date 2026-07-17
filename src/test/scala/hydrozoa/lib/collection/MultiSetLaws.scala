package hydrozoa.lib.collection

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import scala.collection.immutable.SortedMap
import spire.algebra.{CModule, CRing, Order, PartialOrder}
import spire.implicits.*
import spire.laws.arb.given
import spire.laws.{GroupLaws, OrderLaws, PredicateFromMonoid, VectorSpaceLaws}
import spire.math.SafeLong

/** Direct laws for [[Multiset]]. The multiplicity carrier is [[SafeLong]] — the signed, overflow-
  * safe integer the petri `MultiSet[C]` facade fixes — so the group / module structure firing
  * relies on is exercised head-on rather than transitively through a consumer.
  */
class MultiSetLaws extends AnyFunSuite, FunSuiteDiscipline, Checkers {

    private type MS = Multiset[Int, SafeLong]

    // SafeLong is a commutative ring but spire provides no ring-as-self-module instance, so the
    // scalar action `n · multiset` needs one supplied explicitly.
    private given CModule[SafeLong, SafeLong] with {
        def scalar: CRing[SafeLong] = CRing[SafeLong]
        def zero: SafeLong = SafeLong.zero
        def plus(x: SafeLong, y: SafeLong): SafeLong = x + y
        def negate(x: SafeLong): SafeLong = -x
        def timesl(r: SafeLong, v: SafeLong): SafeLong = r * v
    }

    private val algebra = new Multiset.Algebra[Int, SafeLong]

    private given PartialOrder[MS] =
        new algebra.PartialOrder.OrderedElements(Order[SafeLong].compare)
    // Provides AdditiveAbGroup[MS] too, since CModule extends it.
    private given CModule[MS, SafeLong] = new algebra.CModule[SafeLong] {}

    private given Arbitrary[MS] = Arbitrary(
      for
          n <- Gen.choose(0, 6)
          entries <- Gen.listOfN(
            n,
            for
                k <- Gen.choose(0, 10)
                v <- Arbitrary.arbitrary[SafeLong]
            yield k -> v
          )
      yield Multiset(entries.to(SortedMap))
    )

    checkAll("Multiset.partialOrder", OrderLaws[MS].partialOrder)
    checkAll("Multiset.additiveAbGroup", GroupLaws[MS].additiveAbGroup)
    checkAll("Multiset.cModule", VectorSpaceLaws[MS, SafeLong].cModule)

    // Petri-specific properties not implied by the generic laws above.

    test("canonical form stores no zero-multiplicity entries") {
        check(forAll { (m: MS) => m.multiplicityMap.values.forall(_ != SafeLong.zero) })
    }

    test("get defaults to zero for keys absent from the multiplicity map") {
        check(forAll { (m: MS, k: Int) =>
            m.multiplicityMap.contains(k) || m.get(k) == SafeLong.zero
        })
    }

    // The partial order is the sub-multiset relation — the HLPN enabling test W(p,t) ≤ M(p).
    test("partial order agrees with pointwise multiplicity comparison") {
        check(forAll { (a: MS, b: MS) =>
            val keys = a.multiplicityMap.keySet ++ b.multiplicityMap.keySet
            val pointwiseLe = keys.forall(k => Order[SafeLong].lteqv(a.get(k), b.get(k)))
            PartialOrder[MS].lteqv(a, b) == pointwiseLe
        })
    }
}
