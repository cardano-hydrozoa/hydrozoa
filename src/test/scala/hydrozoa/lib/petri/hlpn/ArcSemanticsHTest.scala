package hydrozoa.lib.petri.hlpn

import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.number.PositiveInt
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.SortedMap
import spire.algebra.Order
import spire.math.SafeLong

class ArcSemanticsHTest extends AnyFunSuite:

    private given Order[String] = Order.from((a, b) => a.compareTo(b))

    private val peer =
        Sort.Class("Peer", NonEmptySet.of("p0", "p1", "p2"), Sort.Discipline.Unordered, Map.empty)

    private def ms(entries: (String, Int)*): MultiSet[String] =
        Multiset(entries.map((k, v) => k -> SafeLong(v)).to(SortedMap))

    private val x = Var("x", peer)
    // ⟨x⟩ : one token of whatever color x is bound to.
    private val wx = Inscription.Weighted(PositiveInt.unsafeApply(1), ColorTerm.Ref(x))
    private val bx = Binding.bind(Binding.empty, x, "p0")

    test("consume removes the inscription and is enabled when the color is present") {
        val arc = ArcSemanticsH.Consume(wx)
        val m = ms("p0" -> 2)
        val _ = assert(arc.enabled(bx, m) == Right(true))
        assert(arc.fire(bx, m).toOption.get == ms("p0" -> 1))
    }

    test("consume is disabled when the required color is absent") {
        val arc = ArcSemanticsH.Consume(wx)
        assert(arc.enabled(bx, ms("p1" -> 1)) == Right(false))
    }

    test("produce adds the inscription; an empty pre is always enabled") {
        val arc = ArcSemanticsH.Produce(wx)
        val _ = assert(arc.enabled(bx, ms()) == Right(true))
        assert(arc.fire(bx, ms()).toOption.get == ms("p0" -> 1))
    }

    test("read requires presence but leaves the marking unchanged") {
        val arc = ArcSemanticsH.Read(wx)
        val m = ms("p0" -> 1)
        val _ = assert(arc.enabled(bx, m) == Right(true))
        val _ = assert(arc.enabled(bx, ms()) == Right(false))
        assert(arc.fire(bx, m).toOption.get == m)
    }
