package hydrozoa.multisig.ledger.virtual.commitment

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import scala.annotation.nowarn
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element}
import scalus.prelude.crypto.bls12_381.{G1, G2}
import scalus.|>
import supranational.blst.{P1, P2}

@nowarn("msg=unused value")
class TrustedSetupTest extends AnyFunSuite:

    test("check trusted setup size") {
        assertResult(32768, "G1 elements in setup")(
          TrustedSetup.takeSrsG1(Integer.MAX_VALUE).length
        )

        assertResult(65, "G2 elements in setup")(
          TrustedSetup.takeSrsG2(Integer.MAX_VALUE).length
        )
    }

    test("load trusted setup as P1/P2") {
        assertResult(P1.generator().compress(), "first G1 element")(
          TrustedSetup.takeSrsG1(1).head.compress()
        )

        assertResult(P2.generator().compress(), "first G2 element")(
          TrustedSetup.takeSrsG2(1).head.compress()
        )
    }

    test("load trusted setup as G1/G2") {
        assertResult(G1.generator) {
            TrustedSetup.takeSrsG1(1).head |> BLS12_381_G1_Element.apply
        }

        assertResult(G2.generator) {
            TrustedSetup.takeSrsG2(1).head |> BLS12_381_G2_Element.apply
        }
    }
