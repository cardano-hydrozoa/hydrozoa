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

class HlSimulatorTest extends AnyFunSuite:

    private given Order[String] = Order.from((a, b) => a.compareTo(b))

    private val peer =
        Sort.Class("Peer", NonEmptySet.of("p0", "p1", "p2"), Sort.Discipline.Unordered, Map.empty)
    private val vote =
        Sort.Class("Vote", NonEmptySet.of("No", "Yes"), Sort.Discipline.Unordered, Map.empty)

    private def ms(entries: (String, Int)*): MultiSet[String] =
        Multiset(entries.map((k, v) => k -> SafeLong(v)).to(SortedMap))

    private case class Tokens(marking: MultiSet[String], colorDomain: Sort[String])
        extends ColoredPlace[String]:
        def mark(m: MultiSet[String]): Tokens = copy(marking = m)

    private val p = Var("p", peer)
    private val v = Var("v", vote)
    private val wp = Inscription.Weighted(PositiveInt.unsafeApply(1), ColorTerm.Ref(p))

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

    private def simulator(pending: MultiSet[String]): HlSimulator[String, String, String] =
        HlSimulator(net(pending), ModeSelector.enumerating)

    test("the enumerating selector finds exactly the enabled modes") {
        val sim = simulator(ms("p0" -> 1, "p2" -> 1))
        val peers = sim.enabledModes("advance").flatMap(Binding.lookup(_, p)).toSet
        assert(peers == Set("p0", "p2"))
    }

    test("enabledTransitions reflects the selector's findings") {
        val _ = assert(simulator(ms("p0" -> 1)).enabledTransitions == Set("advance"))
        assert(simulator(ms()).enabledTransitions == Set.empty)
    }

    test("fire advances under the first enabled candidate and returns the chosen mode") {
        val sim = simulator(ms("p1" -> 1, "p2" -> 1))
        val (advanced, mode) = sim.fire("advance").toOption.get
        // enumeration proposes carrier order p0, p1, p2 — p0 is absent, so p1 fires first
        val _ = assert(Binding.lookup(mode, p).contains("p1"))
        val _ = assert(advanced.net.placesMap("pending").marking == ms("p2" -> 1))
        assert(advanced.net.placesMap("done").marking == ms("p1" -> 1))
    }

    test("fire reports NoEnabledMode when nothing can fire") {
        assert(
          simulator(ms()).fire("advance") == Left(HlSimulator.Error.NoEnabledMode("advance"))
        )
    }

    test("fire reports TransitionNotFound for an unknown transition") {
        assert(
          simulator(ms("p0" -> 1)).fire("nope") ==
              Left(HlSimulator.Error.TransitionNotFound("nope"))
        )
    }

    test("a domain-specific selector overrides the search policy") {
        // Propose only p2 — a scenario-specific driver that knows which peer should act next.
        val onlyP2: ModeSelector[String, String, String] =
            (_, _) => LazyList(Binding.bind(Binding.empty, p, "p2"))
        val sim = HlSimulator(net(ms("p0" -> 1, "p2" -> 1)), onlyP2)
        val (advanced, mode) = sim.fire("advance").toOption.get
        val _ = assert(Binding.lookup(mode, p).contains("p2"))
        assert(advanced.net.placesMap("done").marking == ms("p2" -> 1))
    }

    test("a free variable (no input arc constrains it) multiplies the enabled modes") {
        // castVote consumes ⟨p⟩ and binds v without any arc constraining it — v is a free choice.
        val b = NetBuilder[String, String]()
        val program =
            for
                pending <- b.place("pending", Tokens(ms("p0" -> 1, "p1" -> 1), peer))
                castVote <- b.transition("castVote", List(p, v), Guard.True)
                _ <- b.input(pending, castVote, wp)
            yield ()
        val sim = HlSimulator(b.build(program).toOption.get, ModeSelector.enumerating)
        // 2 present peers × 2 free vote values
        assert(sim.enabledModes("castVote").size == 4)
    }
