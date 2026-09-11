package hydrozoa.multisig.persistence

import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalatest.funsuite.AnyFunSuite

/** What a `transplantStackNumber` does to a freshly derived marker bundle.
  *
  * The rule this pins is that `Markers.adopt` and `Serve`'s boot gate must use the **same**
  * comparison. They did not: the gate accepted `hardConfirmed >= tag` while adoption required exact
  * equality, so a tag naming a stack below the store's tip passed the gate and was then silently
  * dropped — no adoption, no error, and an operator with no way to tell. The first test below is
  * that case, and it fails on the old `hardConfirmed.contains(tag)`.
  */
class MarkersAdoptTest extends AnyFunSuite {

    private def store(hardConfirmed: Int, hardAckedStack: Option[Int]): Markers =
        Markers.cold.copy(
          hardConfirmed = Some(StackNumber(hardConfirmed)),
          hardAckedStack = hardAckedStack.map(StackNumber(_))
        )

    test("a tag BELOW the store's tip is adopted, not silently dropped") {
        // The regression. The boot gate lets this through, so adoption must too — a tag is a
        // partition of the store chosen by the operator, and any stack the store holds is a
        // legitimate choice.
        val adopted =
            Markers.adopt(store(hardConfirmed = 100, hardAckedStack = None), Some(StackNumber(42)))
        assert(
          adopted.hardAckedStack.contains(StackNumber(42)),
          s"a below-tip tag must raise the floor; got ${adopted.hardAckedStack}"
        )
    }

    test("a tag EQUAL to the store's tip is adopted") {
        val adopted =
            Markers.adopt(store(hardConfirmed = 42, hardAckedStack = None), Some(StackNumber(42)))
        assert(adopted.hardAckedStack.contains(StackNumber(42)))
    }

    test("a tag ABOVE the store's tip is not adopted — the gate refuses that boot anyway") {
        val derived = store(hardConfirmed = 10, hardAckedStack = Some(7))
        assert(Markers.adopt(derived, Some(StackNumber(99))) == derived)
    }

    test("no tag leaves the bundle untouched") {
        val derived = store(hardConfirmed = 10, hardAckedStack = Some(7))
        assert(Markers.adopt(derived, None) == derived)
    }

    test("an empty store adopts nothing, whatever the tag") {
        assert(Markers.adopt(Markers.cold, Some(StackNumber(1))) == Markers.cold)
    }

    test("the floor is RAISED and never lowered") {
        // A peer mid-flight has acked one stack beyond its confirmation; clamping that down to the
        // tag would discard the in-flight handoff ReplayActor rebuilds from it.
        val ahead = store(hardConfirmed = 100, hardAckedStack = Some(101))
        assert(
          Markers.adopt(ahead, Some(StackNumber(42))).hardAckedStack.contains(StackNumber(101))
        )
    }

    test("adoption is idempotent — a tag left in the config after the fact changes nothing") {
        val derived = store(hardConfirmed = 100, hardAckedStack = Some(60))
        val once = Markers.adopt(derived, Some(StackNumber(42)))
        assert(Markers.adopt(once, Some(StackNumber(42))) == once)
    }
}
