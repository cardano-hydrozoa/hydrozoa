package hydrozoa.lib.petri.hlpn

import hydrozoa.lib.number.PositiveInt
import org.scalatest.funsuite.AnyFunSuite
import spire.algebra.Order
import spire.math.SafeLong

class BindingTest extends AnyFunSuite:

    private given Order[String] = Order.from((a, b) => a.compareTo(b))

    // A circular peer class {p0, p1, p2} with a static subclass of the even-indexed peers.
    private val peer = Sort.Class(
      "Peer",
      Set("p0", "p1", "p2"),
      Sort.Discipline.Circular,
      Map("evens" -> Set("p0", "p2"))
    )
    // A linear vote class {No, Yes} — no successor past the last element.
    private val vote =
        Sort.Class("Vote", Set("No", "Yes"), Sort.Discipline.Linear, Map.empty)

    private val x = Var("x", peer)
    private val y = Var("y", vote)

    // ----- evalColor -----

    test("evalColor: bound variable resolves to its value") {
        val b = Binding.bind(Binding.empty, x, "p1")
        assert(Binding.evalColor(ColorTerm.Ref(x), b) == Right("p1"))
    }

    test("evalColor: unbound variable fails") {
        assert(
          Binding.evalColor(ColorTerm.Ref(x), Binding.empty) ==
              Left(EvalError.UnboundVariable("x"))
        )
    }

    test("evalColor: successor wraps in a circular class") {
        val b = Binding.bind(Binding.empty, x, "p2")
        assert(Binding.evalColor(ColorTerm.Succ(ColorTerm.Ref(x)), b) == Right("p0"))
    }

    test("evalColor: successor past the end of a linear class fails") {
        val b = Binding.bind(Binding.empty, y, "Yes")
        assert(
          Binding.evalColor(ColorTerm.Succ(ColorTerm.Ref(y)), b) ==
              Left(EvalError.SuccessorUndefined("Vote"))
        )
    }

    test("evalColor: tuple builds a product color") {
        val b = Binding.bind(Binding.bind(Binding.empty, x, "p1"), y, "No")
        assert(
          Binding.evalColor(ColorTerm.Tuple(ColorTerm.Ref(x), ColorTerm.Ref(y)), b) ==
              Right(("p1", "No"))
        )
    }

    // ----- evalInscription -----

    test("evalInscription: weighted color is a scaled singleton bag") {
        val b = Binding.bind(Binding.empty, x, "p1")
        val term = Inscription.Weighted(PositiveInt.unsafeApply(2), ColorTerm.Ref(x))
        val bag = Binding.evalInscription(term, b).toOption.get
        val _ = assert(bag.get("p1") == SafeLong(2))
        assert(bag.multiplicityMap.size == 1)
    }

    test("evalInscription: all is one of every color in the class") {
        val bag = Binding.evalInscription(Inscription.All(peer), Binding.empty).toOption.get
        val _ = assert(bag.multiplicityMap.size == 3)
        assert(List("p0", "p1", "p2").forall(p => bag.get(p) == SafeLong.one))
    }

    test("evalInscription: subclass all is restricted to the partition") {
        val bag =
            Binding
                .evalInscription(Inscription.SubclassAll(peer, "evens"), Binding.empty)
                .toOption
                .get
        assert(bag.multiplicityMap.keySet == Set("p0", "p2"))
    }

    test("evalInscription: unknown subclass fails") {
        assert(
          Binding.evalInscription(Inscription.SubclassAll(peer, "odds"), Binding.empty) ==
              Left(EvalError.UnknownSubclass("Peer", "odds"))
        )
    }

    test("evalInscription: union sums multiplicities") {
        val b = Binding.bind(Binding.empty, x, "p0")
        val term = Inscription.Union(
          Inscription.Weighted(PositiveInt.unsafeApply(1), ColorTerm.Ref(x)),
          Inscription.Weighted(PositiveInt.unsafeApply(1), ColorTerm.Const("p0", peer))
        )
        val bag = Binding.evalInscription(term, b).toOption.get
        val _ = assert(bag.get("p0") == SafeLong(2))
        assert(bag.multiplicityMap.size == 1)
    }

    // ----- evalGuard -----

    test("evalGuard: equality") {
        val b = Binding.bind(Binding.empty, x, "p1")
        val _ = assert(
          Binding.evalGuard(Guard.Eq(ColorTerm.Ref(x), ColorTerm.Const("p1", peer)), b) == Right(
            true
          )
        )
        assert(
          Binding.evalGuard(Guard.Eq(ColorTerm.Ref(x), ColorTerm.Const("p0", peer)), b) == Right(
            false
          )
        )
    }

    test("evalGuard: less-than by carrier order") {
        assert(
          Binding.evalGuard(
            Guard.Lt(ColorTerm.Const("p0", peer), ColorTerm.Const("p1", peer)),
            Binding.empty
          ) == Right(true)
        )
    }

    test("evalGuard: subclass membership") {
        val _ = assert(
          Binding.evalGuard(
            Guard.InSubclass(ColorTerm.Ref(x), "evens"),
            Binding.bind(Binding.empty, x, "p0")
          ) == Right(true)
        )
        assert(
          Binding.evalGuard(
            Guard.InSubclass(ColorTerm.Ref(x), "evens"),
            Binding.bind(Binding.empty, x, "p1")
          ) == Right(false)
        )
    }

    test("evalGuard: boolean connectives") {
        val t = Guard.True
        val _ = assert(Binding.evalGuard(Guard.Not(t), Binding.empty) == Right(false))
        val _ = assert(Binding.evalGuard(Guard.And(t, Guard.Not(t)), Binding.empty) == Right(false))
        assert(Binding.evalGuard(Guard.Or(t, Guard.Not(t)), Binding.empty) == Right(true))
    }
