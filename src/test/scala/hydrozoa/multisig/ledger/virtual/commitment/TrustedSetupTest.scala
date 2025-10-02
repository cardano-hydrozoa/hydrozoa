package hydrozoa.multisig.ledger.virtual.commitment

import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element}
import scalus.prelude.crypto.bls12_381.{G1, G2}
import scalus.|>

import munit.FunSuite
import supranational.blst.{P1, P2}

class TreasuryValidatorTest extends FunSuite:

    test("check trusted setup size") {
        assertEquals(TrustedSetup.takeSrsG1(Integer.MAX_VALUE).length, BigInt(32768))
        assertEquals(TrustedSetup.takeSrsG2(Integer.MAX_VALUE).length, BigInt(65))
    }

    test("load trusted setup as P1/P2") {
        assert(TrustedSetup.takeSrsG1(1).head.is_equal(P1.generator()))
        assert(TrustedSetup.takeSrsG2(1).head.is_equal(P2.generator()))
    }

    test("load trusted setup as G1/G2") {
        assertEquals(TrustedSetup.takeSrsG1(1).head |> BLS12_381_G1_Element.apply, G1.generator)
        assertEquals(TrustedSetup.takeSrsG2(1).head |> BLS12_381_G2_Element.apply, G2.generator)
    }
