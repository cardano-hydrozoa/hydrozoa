package hydrozoa.l1.rulebased.onchain

import hydrozoa.l1.rulebased.onchain.scalar.Scalar
import hydrozoa.l1.rulebased.onchain.scalar.Scalar.{
    fieldPrime,
    fromByteStringBigEndian,
    fromByteStringLittleEndian
}
import munit.FunSuite
import scalus.builtin.ByteString
import scalus.prelude.Option
import scalus.prelude.Option.{None, Some}

class ScalarTest extends FunSuite:

    private inline def s834 = new Scalar(834884848)

    test("apply") {
        assertEquals(Scalar(1), Some(new Scalar(1)))
        assertEquals(Scalar(Scalar.fieldPrime), None)
        assertEquals(Scalar(834884848), Some(s834))
        assertEquals(Scalar(BigInt(834884848)), Some(s834))
        assertEquals(Scalar("834884848"), Some(s834))
    }

    test("fromByteStringBigEndian") {
        assertEquals(
          fromByteStringBigEndian(ByteString.fromHex("ffff00")),
          Scalar(16776960)
        )
    }

    test("fromByteStringLittleEndian") {
        assertEquals(
          fromByteStringLittleEndian(ByteString.fromHex("ffff00")),
          Scalar(65535)
        )
    }

    test("add") {
        assertEquals(s834 + s834, new Scalar(1669769696))
        assertEquals(new Scalar(fieldPrime - 1) + new Scalar(1), Scalar.zero)
        assertEquals(new Scalar(3) + new Scalar(fieldPrime), new Scalar(3))
    }

    test("mul") {
        val s = s834
        assertEquals(s * s, Scalar("697032709419983104").get)
        assertEquals(Scalar.zero * s834, Scalar.zero)
        assertEquals(
          new Scalar(fieldPrime - 1) * new Scalar(2),
          Scalar(
            "52435875175126190479447740508185965837690552500527637822603658699938581184511"
          ).get
        )
    }

    test("scale") {
        val s = s834
        assertEquals(s.scale(-1), Scalar.zero)
        assertEquals(s.scale(0), Scalar.one)
        assertEquals(s.scale(1), s)
        assertEquals(s.scale(2), Scalar("697032709419983104").get)
        assertEquals(s.scale(3), Scalar("581942047655130761945608192").get)
        assertEquals(
          new Scalar(fieldPrime - 4).scale(200),
          Scalar(
            "12843927705572658539565969578937286576443167978938369866871449552629978143484"
          ).get
        )
    }

    test("scale2") {
        val s = s834
        assertEquals(s.scale2(-1), Scalar.zero)
        assertEquals(s.scale2(0), s)
        assertEquals(s.scale2(1), s.scale(2))
        assertEquals(s.scale2(2), s.scale(4))
        assertEquals(s.scale2(3), s.scale(8))
        assertEquals(s.scale2(4), s.scale(16))
    }

    test("div") {
        val s = s834
        assertEquals(s / s, Some(Scalar.one))
        assertEquals(s / Scalar.zero, None)
        assertEquals(
          new Scalar(fieldPrime - 1) / new Scalar(2),
          Scalar("26217937587563095239723870254092982918845276250263818911301829349969290592256")
        )
    }

    test("neg") {
        assertEquals(
          -s834,
          Scalar(
            "52435875175126190479447740508185965837690552500527637822603658699937746299665"
          ).get
        )
        assertEquals(-Scalar.zero, Scalar.zero)
        assertEquals(-Scalar.one, new Scalar(fieldPrime - 1))
    }

    test("recip") {
        assertEquals(
          s834.recip,
          Scalar("35891248691642227249400403463796410930702563777316955162085759263735363466421")
        )
        assertEquals(Scalar.zero.recip, None)
    }

    test("sub") {
        val s = s834
        assertEquals(s - s, Scalar.zero)
        assertEquals(s - new Scalar(834884847), Scalar.one)
        assertEquals(Scalar.zero - new Scalar(fieldPrime), Scalar.zero)
        assertEquals(Scalar.zero - new Scalar(5), new Scalar(fieldPrime - 5))
    }
