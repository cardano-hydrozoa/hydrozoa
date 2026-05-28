package hydrozoa.multisig.persistence

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Round-trip + boundary tests for the lane-value framing (§5.5, §7.1). */
class LaneValueTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    test("frame then stamp + payload is identity") {
        forAll(Gen.long, Gen.containerOf[Array, Byte](Gen.choose(Byte.MinValue, Byte.MaxValue))) {
            (stamp: Long, payload: Array[Byte]) =>
                val framed = LaneValue.frame(stamp, payload)
                assert(LaneValue.stamp(framed) == stamp)
                assert(LaneValue.payload(framed).sameElements(payload))
        }
    }

    test("framed length == 8 + payload length") {
        forAll(Gen.long, Gen.containerOf[Array, Byte](Gen.choose(Byte.MinValue, Byte.MaxValue))) {
            (stamp: Long, payload: Array[Byte]) =>
                assert(
                  LaneValue.frame(stamp, payload).length == LaneValue.stampWidth + payload.length
                )
        }
    }

    test("an empty payload still frames + unframes cleanly") {
        val framed = LaneValue.frame(42L, Array.emptyByteArray)
        assert(framed.length == LaneValue.stampWidth)
        assert(LaneValue.stamp(framed) == 42L)
        assert(LaneValue.payload(framed).isEmpty)
    }

    test("stamp / payload throw on a too-short framed value") {
        intercept[IllegalArgumentException](LaneValue.stamp(Array.emptyByteArray))
        intercept[IllegalArgumentException](LaneValue.payload(new Array[Byte](7)))
    }

    test("the stamp prefix is big-endian (lex order = numeric order)") {
        val a = LaneValue.frame(0L, Array.emptyByteArray)
        val b = LaneValue.frame(1L, Array.emptyByteArray)
        val c = LaneValue.frame(Long.MaxValue, Array.emptyByteArray)
        assert(
          java.util.Arrays.compareUnsigned(a, b) < 0 &&
              java.util.Arrays.compareUnsigned(b, c) < 0
        )
    }
