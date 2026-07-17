package hydrozoa.lib.petri.hlpn

import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.number.PositiveInt
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.SortedMap
import spire.algebra.Order
import spire.math.SafeLong

/** The folded voting sub-net as a real [[HlNet]]: one `advance` transition moves a peer token from
  * `pending` to `done`, the peer chosen by the firing mode.
  */
class HlNetTest extends AnyFunSuite:

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

    private def net(pending: MultiSet[String]): HlNet[String, String, String, String] =
        HlNet(
          places = Map[String, ColoredPlace[String]](
            "pending" -> Tokens(pending, peer),
            "done" -> Tokens(ms(), peer)
          ),
          transitions = Map("advance" -> HlNet.Transition(List(p), Guard.True)),
          arcs = Map[String, HlNet.Arc[String, String, String]](
            "a1" -> HlNet.Arc("pending", "advance", ArcSemanticsH.Consume(wp)),
            "a2" -> HlNet.Arc("done", "advance", ArcSemanticsH.Produce(wp))
          )
        )

    test("a mode is enabled iff its peer is present in the input place") {
        val n = net(ms("p0" -> 1, "p2" -> 1))
        def mode(peer: String) = Binding.bind(Binding.empty, p, peer)
        val _ = assert(n.isModeEnabled("advance", mode("p0")))
        val _ = assert(!n.isModeEnabled("advance", mode("p1")))
        assert(n.isModeEnabled("advance", mode("p2")))
    }

    test("firing moves the chosen token and yields a new net") {
        val n = net(ms("p0" -> 1, "p2" -> 1))
        val mode = Binding.bind(Binding.empty, p, "p0")
        val n2 = n.fire("advance", mode).toOption.get
        val _ = assert(n2.places("pending").marking == ms("p2" -> 1))
        assert(n2.places("done").marking == ms("p0" -> 1))
    }

    test("firing an un-enabled mode is rejected") {
        val n = net(ms("p1" -> 1))
        val mode = Binding.bind(Binding.empty, p, "p0")
        assert(n.fire("advance", mode) == Left(HlNet.FiringError.NotEnabled("advance")))
    }

    test("firing an unknown transition is rejected") {
        val n = net(ms("p0" -> 1))
        val mode = Binding.bind(Binding.empty, p, "p0")
        assert(n.fire("nope", mode) == Left(HlNet.FiringError.TransitionNotFound("nope")))
    }
