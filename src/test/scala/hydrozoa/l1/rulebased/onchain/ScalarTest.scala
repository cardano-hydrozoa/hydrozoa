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
    test("apply") {
        assertEquals(Scalar(BigInt(1)), Some(new Scalar(1)))
        assertEquals(Scalar(Scalar.fieldPrime), None)
        assertEquals(Scalar(BigInt(834884848)), Some(new Scalar(834884848)))
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
        val s = new Scalar(834884848)
        assertEquals(s + s, new Scalar(1669769696))
        assertEquals(new Scalar(fieldPrime - 1) + new Scalar(1), Scalar.zero)
        assertEquals(new Scalar(3) + new Scalar(fieldPrime), new Scalar(3))
    }

    test("mul") {
        val s = new Scalar(834884848)
        assertEquals(s * s, new Scalar(BigInt("697032709419983104")))
        assertEquals(Scalar.zero * new Scalar(834884848), Scalar.zero)
        assertEquals(
          new Scalar(fieldPrime - 1) * new Scalar(2),
          new Scalar(
            BigInt("52435875175126190479447740508185965837690552500527637822603658699938581184511")
          )
        )
    }

    test("scale") {
        val s = new Scalar(834884848)
        assertEquals(s.scale(-1), Scalar.zero)
        assertEquals(s.scale(0), Scalar.one)
        assertEquals(s.scale(1), s)
        assertEquals(s.scale(2), new Scalar(BigInt("697032709419983104")))
        assertEquals(s.scale(3), new Scalar(BigInt("581942047655130761945608192")))
        assertEquals(
          new Scalar(fieldPrime - 4).scale(200),
          new Scalar(
            BigInt("12843927705572658539565969578937286576443167978938369866871449552629978143484")
          )
        )
    }

    test("scale2") {
        val s = new Scalar(834884848)
        assertEquals(s.scale2(-1), Scalar.zero)
        assertEquals(s.scale2(0), s)
        assertEquals(s.scale2(1), s.scale(2))
        assertEquals(s.scale2(2), s.scale(4))
        assertEquals(s.scale2(3), s.scale(8))
        assertEquals(s.scale2(4), s.scale(16))
    }

    test("div") {
        val s = new Scalar(834884848)
        assertEquals(s / s, Some(Scalar.one))
        assertEquals(s / Scalar.zero, None)
        assertEquals(
          new Scalar(fieldPrime - 1) / new Scalar(2),
          Some(
            new Scalar(
              BigInt(
                "26217937587563095239723870254092982918845276250263818911301829349969290592256"
              )
            )
          )
        )
    }

    test("neg") {
        assertEquals(
          new Scalar(834884848).neg,
          new Scalar(
            BigInt("52435875175126190479447740508185965837690552500527637822603658699937746299665")
          )
        )
        assertEquals(Scalar.zero.neg, Scalar.zero)
        assertEquals(Scalar.one.neg, new Scalar(fieldPrime - 1))
    }

    test("recip") {
        assertEquals(
          new Scalar(834884848).recip,
          Some(
            new Scalar(
              BigInt(
                "35891248691642227249400403463796410930702563777316955162085759263735363466421"
              )
            )
          )
        )
        assertEquals(Scalar.zero.recip, None)
    }

    test("sub") {
        val s = new Scalar(834884848)
        assertEquals(s - s, Scalar.zero)
        assertEquals(s - new Scalar(834884847), Scalar.one)
        assertEquals(Scalar.zero - new Scalar(fieldPrime), Scalar.zero)
        assertEquals(Scalar.zero - new Scalar(5), new Scalar(fieldPrime - 5))
    }
