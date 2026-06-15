package hydrozoa.multisig.persistence

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Round-trip + boundary tests for the family-value framing (§5.5, §7.1). */
class FamilyValueTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    private val genStamp: Gen[ArrivalStamp] =
        for
            generation <- Gen.choose(0, Int.MaxValue)
            mono <- Gen.long
        yield ArrivalStamp(generation, mono)

    private val genPayload: Gen[Array[Byte]] =
        Gen.containerOf[Array, Byte](Gen.choose(Byte.MinValue, Byte.MaxValue))

    test("frame then stamp + payload is identity") {
        forAll(genStamp, genPayload) { (stamp: ArrivalStamp, payload: Array[Byte]) =>
            val framed = FamilyValue.frame(stamp, payload)
            val _ = assert(FamilyValue.stamp(framed) == stamp)
            assert(FamilyValue.payload(framed).sameElements(payload))
        }
    }

    test("framed length == stampWidth + payload length") {
        forAll(genStamp, genPayload) { (stamp: ArrivalStamp, payload: Array[Byte]) =>
            assert(
              FamilyValue.frame(stamp, payload).length == FamilyValue.stampWidth + payload.length
            )
        }
    }

    test("an empty payload still frames + unframes cleanly") {
        val stamp = ArrivalStamp(3, 42L)
        val framed = FamilyValue.frame(stamp, Array.emptyByteArray)
        val _ = assert(framed.length == FamilyValue.stampWidth)
        val _ = assert(FamilyValue.stamp(framed) == stamp)
        assert(FamilyValue.payload(framed).isEmpty)
    }

    test("stamp / payload throw on a too-short framed value") {
        val _ = intercept[IllegalArgumentException](FamilyValue.stamp(Array.emptyByteArray))
        intercept[IllegalArgumentException](
          FamilyValue.payload(new Array[Byte](FamilyValue.stampWidth - 1))
        )
    }

    test("the stamp prefix is big-endian — lex order == (generation, monotonic) order") {
        // monotonicNanos is non-negative in practice (IO.monotonic), so unsigned byte order
        // matches the signed (generation, monotonic) ordering.
        val a = FamilyValue.frame(ArrivalStamp(0, 0L), Array.emptyByteArray)
        val b = FamilyValue.frame(ArrivalStamp(0, 1L), Array.emptyByteArray)
        val c = FamilyValue.frame(ArrivalStamp(1, 0L), Array.emptyByteArray)
        assert(
          java.util.Arrays.compareUnsigned(a, b) < 0 &&
              java.util.Arrays.compareUnsigned(b, c) < 0
        )
    }
