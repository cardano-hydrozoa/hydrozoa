package hydrozoa.lib.petri.hlpn

import cats.data.NonEmptySet
import cats.implicits.catsKernelOrderingForOrder
import hydrozoa.lib.collection.Multiset
import hydrozoa.lib.petri.Positive
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.SortedMap
import spire.algebra.Order
import spire.math.SafeLong

/** The full RBR voting fold as a genuinely *heterogeneous* net: `pending : Bag(Peer)` and
  * `ballots : Bag(Peer × Vote)` — two different color types — assembled into one net, erased to
  * `Any`, and fired. `castVote` binds a peer and a vote; the vote value is unconstrained by any
  * input, so any vote is castable by a present peer. Proves per-place types + `Tuple` inscriptions
  * + product colors work end-to-end over the framework net, not just at the type level.
  */
class HeteroNetTest extends AnyFunSuite:

    private given Order[String] = Order.from((a, b) => a.compareTo(b))

    private val peer =
        Sort.Class("Peer", NonEmptySet.of("p0", "p1", "p2"), Sort.Discipline.Unordered)
    private val vote =
        Sort.Class("Vote", NonEmptySet.of("No", "Yes"), Sort.Discipline.Unordered)
    private val peerVote: Sort[(String, String)] = Sort.Prod(peer, vote)

    private given Order[(String, String)] = peerVote.order

    private def peerBag(entries: (String, Int)*): MultiSet[String] =
        Multiset(entries.map((k, n) => k -> SafeLong(n)).to(SortedMap))
    private def voteBag(entries: ((String, String), Int)*): MultiSet[(String, String)] =
        Multiset(entries.map((k, n) => k -> SafeLong(n)).to(SortedMap))

    private case class Tokens[C](marking: MultiSet[C], colorDomain: Sort[C])
        extends ColoredPlace[C]:
        def mark(m: MultiSet[C]): Tokens[C] = copy(marking = m)

    private val p = Var("p", peer)
    private val v = Var("v", vote)
    // ⟨p⟩ over Peer, and ⟨(p, v)⟩ over Peer × Vote — different color types.
    private val consumePeer = Inscription.Weighted(Positive.unsafe(1), ColorTerm.Ref(p))
    private val produceBallot = Inscription.Weighted(
      Positive.unsafe(1),
      ColorTerm.Tuple(ColorTerm.Ref(p), ColorTerm.Ref(v))
    )

    private val b = NetBuilder[String, String]()

    private def program(pending: MultiSet[String]) =
        for
            pendingRef <- b.place("pending", Tokens(pending, peer)) // PlaceRef[String, String]
            ballotsRef <- b.place(
              "ballots",
              Tokens(voteBag(), peerVote)
            ) // PlaceRef[String, (String, String)]
            castVote <- b.transition("castVote", List(p, v), Guard.True)
            _ <- b.input(pendingRef, castVote, consumePeer)
            _ <- b.output(castVote, ballotsRef, produceBallot)
        yield ()

    test("a heterogeneous net assembles and is well-sorted") {
        val net = b.build(program(peerBag("p0" -> 1, "p1" -> 1))).toOption.get
        assert(SortCheck.errors(net).isEmpty)
    }

    test("a present peer can cast any vote; an absent peer cannot") {
        val net = b.build(program(peerBag("p0" -> 1, "p1" -> 1))).toOption.get
        def mode(peer: String, vote: String) =
            Binding.bind(Binding.bind(Binding.empty, p, peer), v, vote)
        // present peer, either vote value (v is a free choice — unconstrained by any input arc)
        val _ = assert(net.isModeEnabled("castVote", mode("p0", "No")))
        val _ = assert(net.isModeEnabled("castVote", mode("p1", "Yes")))
        // p2 is not in `pending`
        assert(!net.isModeEnabled("castVote", mode("p2", "Yes")))
    }

    test("firing records the product-colored ballot and consumes the peer") {
        val net = b.build(program(peerBag("p0" -> 1, "p1" -> 1))).toOption.get
        val mode = Binding.bind(Binding.bind(Binding.empty, p, "p0"), v, "Yes")
        val fired = net.fire("castVote", mode).toOption.get
        val _ = assert(fired.placesMap("pending").marking == peerBag("p1" -> 1))
        assert(fired.placesMap("ballots").marking == voteBag(("p0", "Yes") -> 1))
    }
