package hydrozoa.l1.rulebased.onchain

import hydrozoa.infra.encodeHex
import hydrozoa.l1.rulebased.onchain.scalar.Scalar as SScalar
import hydrozoa.l2.commitment.TrustedSetup
import hydrozoa.l2.ledger.simple.getG1Commitment
import munit.FunSuite
import scalus.builtin.{BLS12_381_G1_Element, ByteString, Data}
import scalus.ledger.api.v3.ScriptContext
import scalus.prelude.List
import scalus.prelude.crypto.bls12_381.G1
import supranational.blst.Scalar

import scala.io.Source

class TreasuryValidatorTest extends FunSuite:

    test("check_membership_empty_test") {

        // Pre-calculated powers of tau
        val crs_g2 = TrustedSetup.g2Monomials

        // Accumulator:
        val accumulator = TrustedSetup.g1Monomials1(1).head

        // Empty subset
        val subset: List[SScalar] = List()

        // Proof
        val proof = accumulator

        assertEquals(TreasuryValidator.checkMembership(crs_g2, accumulator, subset, proof), true)
    }

    test("commitment_calculation_test") {

        // Hashes of utxos
        val subset = List(
          SScalar("5088390254917556218676958430080367916099549701669885042964937592926").get,
          SScalar("10660627058191719947148018507418106787651292500144510379470545154509").get
        )

//        val ss = subset.map(ss => Scalar().from_bendian(ss._1.toByteArray))
//        println(s"utxos active hashes: ${ss.map(e => BigInt.apply(e.to_bendian()))}")

        val setup = TrustedSetup.g1Monomials(subset.size.toInt + 1)

        val commitmentPoint =
            getG1Commitment(setup, subset.map(ss => Scalar().from_bendian(ss._1.toByteArray)))

        val commitment = encodeHex(IArray.unsafeFromArray(commitmentPoint.compress()))

        assertEquals(
          commitment,
          "8ec51973adde24a8b6a05f62843f1c2949d01bdc642091f85a9d1803abc074616b545fd6fa25fbc467af2ef112cda832"
        )
    }

    /** This test is based on real (i.e. test) Hydrozoa data.
      */
    test("check_membership_test") {

        // Pre-calculated powers of tau
        val crs_g2 = TrustedSetup.g2Monomials

        // Accumulator:
        val accumulator = BLS12_381_G1_Element(
          ByteString.fromHex(
            "8ec51973adde24a8b6a05f62843f1c2949d01bdc642091f85a9d1803abc074616b545fd6fa25fbc467af2ef112cda832"
          )
        )

        // Hashes of utxos
        val subset = List(
          SScalar("5088390254917556218676958430080367916099549701669885042964937592926").get,
          SScalar("10660627058191719947148018507418106787651292500144510379470545154509").get
        )

        // Proof
        val proof = G1.generator

        assertEquals(TreasuryValidator.checkMembership(crs_g2, accumulator, subset, proof), true)
    }

    test("Withdraw redeemer") {
        val ctxData = Data.fromJson(
          Source
              .fromResource("withdraw-ctx-01.json")
              .getLines()
              .foldLeft("")((acc, line) => acc + line)
        )
        val ctx: ScriptContext = Data.fromData[ScriptContext](ctxData)
        println(ctx)
        TreasuryValidator.validate(ctxData)
    }
