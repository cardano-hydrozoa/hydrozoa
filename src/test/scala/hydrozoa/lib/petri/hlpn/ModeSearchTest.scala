package hydrozoa.lib.petri.hlpn

import cats.data.NonEmptyList
import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.number.PositiveInt
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.SortedMap
import spire.algebra.Order
import spire.math.SafeLong

/** Mode search over the folded voting sub-net: a single `advance` transition whose peer is a
  * *mode*, replacing what would be one transition per peer in an unfolded P/T net.
  */
class ModeSearchTest extends AnyFunSuite:

    private val peer = Sort.Class(
      "Peer",
      NonEmptyList.of("p0", "p1", "p2"),
      Sort.Discipline.Unordered,
      Map("evens" -> Set("p0", "p2"))
    )

    private given Order[String] = peer.order

    private def ms(entries: (String, Int)*): MultiSet[String] =
        Multiset(entries.map((k, v) => k -> SafeLong(v)).to(SortedMap))

    private def marking(m: Map[String, MultiSet[String]]): String => MultiSet[String] =
        pid => m.getOrElse(pid, ms())

    private val p = Var("p", peer)
    private val wp = Inscription.Weighted(PositiveInt.unsafeApply(1), ColorTerm.Ref(p))

    /** Move one peer token from `pending` to `done`, under `guard`. */
    private def advance(guard: Guard): TransitionH[String, String] =
        TransitionH(
          variables = List(p),
          guard = guard,
          arcs = List(
            "pending" -> ArcSemanticsH.Consume(wp),
            "done" -> ArcSemanticsH.Produce(wp)
          )
        )

    test("one mode per peer present in the input place") {
        val mk = marking(Map("pending" -> ms("p0" -> 1, "p2" -> 1)))
        val modes = ModeSearch.enabledModes(advance(Guard.True), mk).toList
        assert(modes.flatMap(b => Binding.lookup(b, p)).toSet == Set("p0", "p2"))
    }

    test("an equality guard selects a single mode") {
        val mk = marking(Map("pending" -> ms("p0" -> 1, "p1" -> 1, "p2" -> 1)))
        val t = advance(Guard.Eq(ColorTerm.Ref(p), ColorTerm.Const("p1", peer)))
        val modes = ModeSearch.enabledModes(t, mk).toList
        val _ = assert(modes.size == 1)
        assert(Binding.lookup(modes.head, p).contains("p1"))
    }

    test("a subclass guard restricts modes to the partition") {
        val mk = marking(Map("pending" -> ms("p0" -> 1, "p1" -> 1, "p2" -> 1)))
        val t = advance(Guard.InSubclass(ColorTerm.Ref(p), "evens"))
        val modes = ModeSearch.enabledModes(t, mk).toList
        assert(modes.flatMap(b => Binding.lookup(b, p)).toSet == Set("p0", "p2"))
    }

    test("no mode when the input place is empty") {
        assert(!ModeSearch.isEnabled(advance(Guard.True), marking(Map.empty)))
    }

    test("firing under a mode moves the chosen token") {
        val mk = marking(Map("pending" -> ms("p0" -> 1, "p2" -> 1)))
        val mode = Binding.bind(Binding.empty, p, "p0")
        val fired = ModeSearch.fire(advance(Guard.True), mode, mk).toOption.get
        val _ = assert(fired("pending") == ms("p2" -> 1))
        assert(fired("done") == ms("p0" -> 1))
    }
