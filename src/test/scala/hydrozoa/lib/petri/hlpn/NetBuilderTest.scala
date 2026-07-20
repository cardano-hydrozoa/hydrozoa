package hydrozoa.lib.petri.hlpn

import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.number.PositiveInt
import hydrozoa.lib.petri.net.components.Arc.Flow
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.SortedMap
import spire.algebra.Order
import spire.math.SafeLong

class NetBuilderTest extends AnyFunSuite:

    private given Order[String] = Order.from((a, b) => a.compareTo(b))

    private val peer =
        Sort.Class("Peer", NonEmptySet.of("p0", "p1", "p2"), Sort.Discipline.Unordered, Map.empty)

    private def ms(entries: (String, Int)*): MultiSet[String] =
        Multiset(entries.map((k, v) => k -> SafeLong(v)).to(SortedMap))

    private case class Tokens(marking: MultiSet[String], colorDomain: Sort[String])
        extends ColoredPlace[String]:
        def mark(m: MultiSet[String]): Tokens = copy(marking = m)

    private val p = Var("p", peer)
    private val wp = Inscription.Weighted(PositiveInt.unsafeApply(1), ColorTerm.Ref(p))

    private val b = NetBuilder[String, String]()

    test("a well-formed program assembles and fires") {
        val program =
            for
                in <- b.place("pending", Tokens(ms("p0" -> 1, "p2" -> 1), peer))
                out <- b.place("done", Tokens(ms(), peer))
                t <- b.transition("advance", List(p), Guard.True)
                _ <- b.input(in, t, wp)
                _ <- b.output(t, out, wp)
            yield ()

        val net = b.build(program).toOption.get
        val n2 = net.fire("advance", Binding.bind(Binding.empty, p, "p0")).toOption.get
        val _ = assert(n2.placesMap("pending").marking == ms("p2" -> 1))
        assert(n2.placesMap("done").marking == ms("p0" -> 1))
    }

    test("duplicate ids and duplicate flow elements are reported and accumulated") {
        val program =
            for
                in <- b.place("pending", Tokens(ms(), peer))
                _ <- b.place("pending", Tokens(ms(), peer)) // duplicate place id
                t <- b.transition("advance", List(p), Guard.True)
                _ <- b.input(in, t, wp)
                _ <- b.input(in, t, wp) // duplicate flow element — W must be a function on F
            yield ()

        val errors = b.build(program).fold(_.toList, _ => Nil)
        val _ = assert(errors.exists {
            case NetBuilder.Error.DuplicatePlace("pending") => true
            case _                                          => false
        })
        assert(errors.exists {
            case NetBuilder.Error.DuplicateArc(Flow.Pt("pending", "advance")) => true
            case _                                                            => false
        })
    }
