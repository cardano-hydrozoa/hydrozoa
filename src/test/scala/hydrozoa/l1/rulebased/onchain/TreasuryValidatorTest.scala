package hydrozoa.l1.rulebased.onchain

import hydrozoa.infra.encodeHex
import hydrozoa.l1.rulebased.onchain.scalar.Scalar as SScalar
import hydrozoa.l2.ledger.SimpleL2Ledger.mkDummySetupG2
import hydrozoa.l2.ledger.getG2Commitment
import munit.FunSuite
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString, Data}
import scalus.ledger.api.v3.ScriptContext
import scalus.prelude.List
import scalus.prelude.crypto.bls12_381.G2
import supranational.blst.Scalar

import scala.io.Source

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
          SScalar("22401154959170154123134540742828377934364533580409315286338474307961").get
        )

        // Proof
        val proof = BLS12_381_G2_Element(
          ByteString.fromHex(
            "809875352e4cd02184ecd07429198ca87364ca0c4bf895482fb8364662bce1945d33c599b47b7d2b34724f45fa17fab8141afa380d192a9134d7f1238e17475af8f6c862c1eecf9666bb5e00b17461ad33112ef2f8dd9580c178b36300cb6dd8"
          )
        )

        assertEquals(TreasuryValidator.checkMembership(crs_g1, accumulator, subset, proof), true)
    }

    test("check_membership_dummy") {

        // Pre-calculated powers of tau
        val crs_g1 = TreasuryValidator.setup

        // Accumulator:
        val accumulator = G2.generator

        // Empty subset
        val subset: List[SScalar] = List()

        // Proof
        val proof = G2.generator

        assertEquals(TreasuryValidator.checkMembership(crs_g1, accumulator, subset, proof), true)
    }

    /** This test is based on real (i.e. test) Hydrozoa data.
      */
    test("check_membership_hydrozoa_test") {

        // Pre-calculated powers of tau
        val crs_g1 = TreasuryValidator.setup

        // Accumulator:
        val accumulator = BLS12_381_G2_Element(
          ByteString.fromHex(
            // log
            "931cc290dd0dd5a3d9edf9b865f967dd60341ee525b1216a973274897a146c3e11e43b8b41635ecdbd30a2899218f5ed11039ad70a0eb043489820a6f65e6d62a0357e3a9b97f25cb18766fdb28c808475e242b2a37ab3febdf7e7fbb1b8d4a4"
          )
        )

        // Subset
        val subset = List(
          SScalar("4792973633413630736518528537479107674611990690430790408815911322150").get,
          SScalar("14938298949569724933183763973017050336046564539387946588209698254937").get
        )

        val ss = subset.map(ss => Scalar().from_bendian(ss._1.toByteArray))
        println(s"utxos active hashes: ${ss.map(e => BigInt.apply(e.to_bendian()))}")

        val setup = mkDummySetupG2(subset.size.toInt)

        val setupBS = setup.map(e => BLS12_381_G2_Element.apply(e).toCompressedByteString)
        setupBS.foreach(println)

        // TODO: Make separate test for that
        val commitmentPoint =
            getG2Commitment(setup, subset.map(ss => Scalar().from_bendian(ss._1.toByteArray)))
        val commitment = encodeHex(IArray.unsafeFromArray(commitmentPoint.compress()))

        assertEquals(
          commitment,
          "931cc290dd0dd5a3d9edf9b865f967dd60341ee525b1216a973274897a146c3e11e43b8b41635ecdbd30a2899218f5ed11039ad70a0eb043489820a6f65e6d62a0357e3a9b97f25cb18766fdb28c808475e242b2a37ab3febdf7e7fbb1b8d4a4"
        )

        // Proof
        val proof = G2.generator

        assertEquals(TreasuryValidator.checkMembership(crs_g1, accumulator, subset, proof), true)
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
