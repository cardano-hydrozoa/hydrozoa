package hydrozoa.l1.rulebased.onchain

import hydrozoa.l1.rulebased.onchain.scalar.Scalar
import munit.FunSuite
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString}
import scalus.prelude.List

class TreasuryValidatorTest extends FunSuite:

    test("check_membership_one_elements") {

        // Pre-calculated powers of tau
        val crs_g1 = List(
          BLS12_381_G1_Element(
            ByteString.fromHex(
              "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
            )
          ),
          BLS12_381_G1_Element(
            ByteString.fromHex(
              "b0e7791fb972fe014159aa33a98622da3cdc98ff707965e536d8636b5fcc5ac7a91a8c46e59a00dca575af0f18fb13dc"
            )
          )
        )

        // Accumulator:
        val accumulator = BLS12_381_G2_Element(
          ByteString.fromHex(
            "b0f15b32629d02514af939e5b660d27a4db9f84cde5eecfef7db87c056163a9f21925653519cf9972f4b6c115e195baf1439203af99d121fce39ec8eed3fa72a0a31dd537642ab7cb1da52dfbacab1a032c5579aa702a59f1991e9aefae1d9c5"
          )
        )

        // Subset
        val subset = List(
          Scalar("22401154959170154123134540742828377934364533580409315286338474307961").get
        )

        // Proof
        val proof = BLS12_381_G2_Element(
          ByteString.fromHex(
            "809875352e4cd02184ecd07429198ca87364ca0c4bf895482fb8364662bce1945d33c599b47b7d2b34724f45fa17fab8141afa380d192a9134d7f1238e17475af8f6c862c1eecf9666bb5e00b17461ad33112ef2f8dd9580c178b36300cb6dd8"
          )
        )

        assertEquals(TreasuryValidator.checkMembership(crs_g1, accumulator, subset, proof), true)
    }
