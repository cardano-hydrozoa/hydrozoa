package hydrozoa.multisig.persistence

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Round-trip + boundary tests for the lane-value framing (§5.5, §7.1). */
class LaneValueTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    private val genStamp: Gen[ArrivalStamp] =
        for
            generation <- Gen.choose(0, Int.MaxValue)
            mono <- Gen.long
        yield ArrivalStamp(generation, mono)

    private val genPayload: Gen[Array[Byte]] =
        Gen.containerOf[Array, Byte](Gen.choose(Byte.MinValue, Byte.MaxValue))

    test("frame then stamp + payload is identity") {
        forAll(genStamp, genPayload) { (stamp: ArrivalStamp, payload: Array[Byte]) =>
            val framed = LaneValue.frame(stamp, payload)
            val _ = assert(LaneValue.stamp(framed) == stamp)
            assert(LaneValue.payload(framed).sameElements(payload))
        }
    }

    test("framed length == stampWidth + payload length") {
        forAll(genStamp, genPayload) { (stamp: ArrivalStamp, payload: Array[Byte]) =>
            assert(LaneValue.frame(stamp, payload).length == LaneValue.stampWidth + payload.length)
        }
    }

    test("an empty payload still frames + unframes cleanly") {
        val stamp = ArrivalStamp(3, 42L)
        val framed = LaneValue.frame(stamp, Array.emptyByteArray)
        val _ = assert(framed.length == LaneValue.stampWidth)
        val _ = assert(LaneValue.stamp(framed) == stamp)
        assert(LaneValue.payload(framed).isEmpty)
    }

    test("stamp / payload throw on a too-short framed value") {
        val _ = intercept[IllegalArgumentException](LaneValue.stamp(Array.emptyByteArray))
        intercept[IllegalArgumentException](
          LaneValue.payload(new Array[Byte](LaneValue.stampWidth - 1))
        )
    }

    test("the stamp prefix is big-endian — lex order == (generation, monotonic) order") {
        // monotonicNanos is non-negative in practice (IO.monotonic), so unsigned byte order
        // matches the signed (generation, monotonic) ordering.
        val a = LaneValue.frame(ArrivalStamp(0, 0L), Array.emptyByteArray)
        val b = LaneValue.frame(ArrivalStamp(0, 1L), Array.emptyByteArray)
        val c = LaneValue.frame(ArrivalStamp(1, 0L), Array.emptyByteArray)
        assert(
          java.util.Arrays.compareUnsigned(a, b) < 0 &&
              java.util.Arrays.compareUnsigned(b, c) < 0
        )
    }
