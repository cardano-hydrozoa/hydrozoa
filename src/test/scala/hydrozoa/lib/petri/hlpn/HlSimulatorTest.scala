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

    private case class Pairs(
        marking: MultiSet[(String, String)],
        colorDomain: Sort[(String, String)]
    ) extends ColoredPlace[(String, String)]:
        def mark(m: MultiSet[(String, String)]): Pairs = copy(marking = m)

    private val p = Var("p", peer)
    private val v = Var("v", vote)
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

    test("the unifying selector finds the same enabled modes as enumerating") {
        val n = net(ms("p0" -> 1, "p2" -> 1))
        val enumerated = HlSimulator(n, ModeSelector.enumerating)
            .enabledModes("advance")
            .flatMap(Binding.lookup(_, p))
            .toSet
        val unified = HlSimulator(n, ModeSelector.unifying)
            .enabledModes("advance")
            .flatMap(Binding.lookup(_, p))
            .toSet
        // unification binds p from the *present* tokens — never proposing the absent p1.
        val _ = assert(unified == Set("p0", "p2"))
        assert(unified == enumerated)
    }

    test("the unifying selector unifies the input variable and enumerates the free one") {
        val b = NetBuilder[String, String]()
        val program =
            for
                pending <- b.place("pending", Tokens(ms("p0" -> 1, "p1" -> 1), peer))
                castVote <- b.transition("castVote", List(p, v), Guard.True)
                _ <- b.input(pending, castVote, wp)
            yield ()
        val net = b.build(program).toOption.get // HlNet[String, String, Any]
        def assignments(selector: ModeSelector[String, String, Any]) =
            HlSimulator(net, selector)
                .enabledModes("castVote")
                .map(m => (Binding.lookup(m, p), Binding.lookup(m, v)))
                .toSet
        // p unified against the 2 present peers, v enumerated over its 2 carrier values.
        val _ = assert(assignments(ModeSelector.unifying).size == 4)
        assert(assignments(ModeSelector.unifying) == assignments(ModeSelector.enumerating))
    }

    test("a collection arc drains a whole batch in one firing, capped at its bound") {
        val batch = CollectVar("batch", peer, bound = 2)
        val b = NetBuilder[String, String]()
        val program =
            for
                pending <- b.place("pending", Tokens(ms("p0" -> 1, "p1" -> 1, "p2" -> 1), peer))
                done <- b.place("done", Tokens(ms(), peer))
                drain <- b.transition("drain", List.empty, Guard.True)
                _ <- b.input(pending, drain, Inscription.Collect(batch, ColorTerm.Wildcard(peer)))
                _ <- b.output(drain, done, Inscription.Collect(batch, ColorTerm.Wildcard(peer)))
            yield ()
        val net = b.build(program).toOption.get
        val (advanced, _) = HlSimulator(net, ModeSelector.unifying).fire("drain").toOption.get
        // bound = 2 → exactly two tokens move in one firing; one stays behind
        val moved = advanced.net.placesMap("done").marking.multiplicityMap.values.toList
        val left = advanced.net.placesMap("pending").marking.multiplicityMap.values.toList
        val _ = assert(moved.foldLeft(SafeLong(0))(_ + _) == SafeLong(2))
        assert(left.foldLeft(SafeLong(0))(_ + _) == SafeLong(1))
    }

    test("a collection arc with nothing to collect does not enable its transition") {
        val batch = CollectVar("batch", peer, bound = 2)
        val b = NetBuilder[String, String]()
        val program =
            for
                pending <- b.place("pending", Tokens(ms(), peer))
                done <- b.place("done", Tokens(ms(), peer))
                drain <- b.transition("drain", List.empty, Guard.True)
                _ <- b.input(pending, drain, Inscription.Collect(batch, ColorTerm.Wildcard(peer)))
                _ <- b.output(drain, done, Inscription.Collect(batch, ColorTerm.Wildcard(peer)))
            yield ()
        val net = b.build(program).toOption.get
        // an empty batch would be a no-op firing — the selector must not propose it
        assert(HlSimulator(net, ModeSelector.unifying).enabledTransitions.isEmpty)
    }

    test("a collection arc drains only the batch matching its bound filter") {
        val votePeer: Sort[(String, String)] = Sort.Prod(vote, peer)
        given Order[(String, String)] = votePeer.order
        def pairs(es: ((String, String), Int)*): MultiSet[(String, String)] =
            Multiset(es.map((k, n) => k -> SafeLong(n)).to(SortedMap))
        val batch = CollectVar("batch", votePeer, bound = 64)
        val wp = Inscription.Weighted(Positive.unsafe(1), ColorTerm.Ref(v))
        val pattern = ColorTerm.Tuple(ColorTerm.Ref(v), ColorTerm.Wildcard(peer))
        val b = NetBuilder[String, String]()
        val program =
            for
                // the selector fixes `v = Yes`; the collection then drains every (Yes, *) token
                selector <- b.place("selector", Tokens(ms("Yes" -> 1), vote))
                pool <- b.place(
                  "pool",
                  Pairs(pairs(("Yes", "p0") -> 1, ("Yes", "p1") -> 1, ("No", "p2") -> 1), votePeer)
                )
                drained <- b.place("drained", Pairs(pairs(), votePeer))
                t <- b.transition("drain", List(v), Guard.True)
                _ <- b.input(selector, t, wp)
                _ <- b.output(t, selector, wp)
                _ <- b.input(pool, t, Inscription.Collect(batch, pattern))
                _ <- b.output(t, drained, Inscription.Collect(batch, pattern))
            yield ()
        val net = b.build(program).toOption.get
        val (advanced, _) = HlSimulator(net, ModeSelector.unifying).fire("drain").toOption.get
        val drainedTokens = advanced.net.placesMap("drained").marking
        val poolTokens = advanced.net.placesMap("pool").marking
        // both Yes tokens drained in one firing; the No token stays in the pool
        val _ = assert(drainedTokens.get(("Yes", "p0")) == SafeLong(1))
        val _ = assert(drainedTokens.get(("Yes", "p1")) == SafeLong(1))
        val _ = assert(drainedTokens.get(("No", "p2")) == SafeLong(0))
        assert(poolTokens.get(("No", "p2")) == SafeLong(1))
    }

    test("an inhibitor arc disables its transition while a matching token is present") {
        def sim(blocker: MultiSet[String]) =
            val b = NetBuilder[String, String]()
            val program =
                for
                    source <- b.place("source", Tokens(ms("p0" -> 1), peer))
                    blk <- b.place("blocker", Tokens(blocker, peer))
                    sink <- b.place("sink", Tokens(ms(), peer))
                    t <- b.transition("go", List(p), Guard.True)
                    _ <- b.input(source, t, wp)
                    _ <- b.output(t, sink, wp)
                    _ <- b.input(blk, t, Inscription.Inhibit(ColorTerm.Wildcard(peer)))
                yield ()
            HlSimulator(b.build(program).toOption.get, ModeSelector.unifying)
        // any blocker token disables `go`; it re-enables once the blocker place is empty
        val _ = assert(!sim(ms("p1" -> 1)).enabledTransitions.contains("go"))
        assert(sim(ms()).enabledTransitions.contains("go"))
    }

    test("an inhibitor arc blocks only on a token matching its pattern") {
        def sim(blocker: MultiSet[String]) =
            val b = NetBuilder[String, String]()
            val program =
                for
                    source <- b.place("source", Tokens(ms("p0" -> 1), peer))
                    blk <- b.place("blocker", Tokens(blocker, vote))
                    sink <- b.place("sink", Tokens(ms(), peer))
                    t <- b.transition("go", List(p), Guard.True)
                    _ <- b.input(source, t, wp)
                    _ <- b.output(t, sink, wp)
                    _ <- b.input(blk, t, Inscription.Inhibit(ColorTerm.Const("No", vote)))
                yield ()
            HlSimulator(b.build(program).toOption.get, ModeSelector.unifying)
        // a "No" token blocks; a non-matching "Yes" token does not
        val _ = assert(!sim(ms("No" -> 1)).enabledTransitions.contains("go"))
        assert(sim(ms("Yes" -> 1)).enabledTransitions.contains("go"))
    }

    test("a read arc requires a token but consumes none") {
        val readP1 = Inscription.Weighted(Positive.unsafe(1), ColorTerm.Const("p1", peer))
        val b = NetBuilder[String, String]()
        val program =
            for
                source <- b.place("source", Tokens(ms("p0" -> 1), peer))
                gate <- b.place("gate", Tokens(ms("p1" -> 1), peer))
                sink <- b.place("sink", Tokens(ms(), peer))
                t <- b.transition("go", List(p), Guard.True)
                _ <- b.input(source, t, wp)
                _ <- b.output(t, sink, wp)
                _ <- b.read(gate, t, readP1)
            yield ()
        val net = b.build(program).toOption.get
        val (advanced, _) = HlSimulator(net, ModeSelector.unifying).fire("go").toOption.get
        // the gate token is untouched (read, not consumed); the source token was consumed
        val _ = assert(advanced.net.placesMap("gate").marking.get("p1") == SafeLong(1))
        assert(advanced.net.placesMap("source").marking.get("p0") == SafeLong(0))
    }

    test("a read arc disables its transition when the token is absent") {
        val readP1 = Inscription.Weighted(Positive.unsafe(1), ColorTerm.Const("p1", peer))
        def sim(gate: MultiSet[String]) =
            val b = NetBuilder[String, String]()
            val program =
                for
                    source <- b.place("source", Tokens(ms("p0" -> 1), peer))
                    g <- b.place("gate", Tokens(gate, peer))
                    sink <- b.place("sink", Tokens(ms(), peer))
                    t <- b.transition("go", List(p), Guard.True)
                    _ <- b.input(source, t, wp)
                    _ <- b.output(t, sink, wp)
                    _ <- b.read(g, t, readP1)
                yield ()
            HlSimulator(b.build(program).toOption.get, ModeSelector.unifying)
        val _ = assert(sim(ms("p1" -> 1)).enabledTransitions.contains("go"))
        assert(!sim(ms()).enabledTransitions.contains("go"))
    }
