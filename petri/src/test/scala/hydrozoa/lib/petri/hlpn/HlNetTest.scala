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

/** The folded voting sub-net as an [[HlNet]] over the framework: one `advance` transition moves a
  * peer token from `pending` to `done`, the peer chosen by the firing mode.
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
    private val wp = Inscription.Weighted(Positive.unsafe(1), ColorTerm.Ref(p))

    private def net(pending: MultiSet[String]): HlNet[String, String, String] =
        HlNet(
          placesMap = Map[String, ColoredPlace[String]](
            "pending" -> Tokens(pending, peer),
            "done" -> Tokens(ms(), peer)
          ),
          transitionsMap = Map("advance" -> HlTransition(List(p), Guard.True)),
          arcsMap = Map(
            Flow.Pt("pending", "advance") -> InscribedArc(wp),
            Flow.Tp("advance", "done") -> InscribedArc(wp)
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
        val _ = assert(n2.placesMap("pending").marking == ms("p2" -> 1))
        assert(n2.placesMap("done").marking == ms("p0" -> 1))
    }

    test("firing an un-enabled mode is rejected with the failing arc") {
        val n = net(ms("p1" -> 1))
        val mode = Binding.bind(Binding.empty, p, "p0")
        n.fire("advance", mode) match
            case Left(HlNet.FiringError.ArcNotEnabled(flow, _, _)) =>
                assert(flow == Flow.Pt("pending", "advance"))
            case other => fail(s"expected ArcNotEnabled, got $other")
    }

    test("firing an unknown transition is rejected") {
        val n = net(ms("p0" -> 1))
        val mode = Binding.bind(Binding.empty, p, "p0")
        assert(n.fire("nope", mode) == Left(HlNet.FiringError.TransitionNotFound("nope")))
    }

    test("a read self-loop (Pt + Tp on the same place) requires presence but leaves it unchanged") {
        val n = HlNet(
          placesMap = Map[String, ColoredPlace[String]]("ref" -> Tokens(ms("p0" -> 1), peer)),
          transitionsMap = Map("observe" -> HlTransition(List(p), Guard.True)),
          arcsMap = Map(
            Flow.Pt("ref", "observe") -> InscribedArc(wp),
            Flow.Tp("observe", "ref") -> InscribedArc(wp)
          )
        )
        val _ = assert(!n.isModeEnabled("observe", Binding.bind(Binding.empty, p, "p1")))
        val fired = n.fire("observe", Binding.bind(Binding.empty, p, "p0")).toOption.get
        assert(fired.placesMap("ref").marking == ms("p0" -> 1))
    }
