package hydrozoa.rulebased.ledger.dapp.script.plutus

import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.multisig.ledger.virtual.commitment.{KzgCommitment, TrustedSetup}
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scala.io.Source
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString, Data}
import scalus.cardano.ledger.ScriptHash
import scalus.ledger.api.v3.ScriptContext
import scalus.prelude.List
import scalus.prelude.crypto.bls12_381.G1
import scalus.|>
import supranational.blst.Scalar

@nowarn("msg=unused value")
class RuleBasedTreasuryScriptTest extends AnyFunSuite {

    test("RuleBasedTreasuryScript object exists and can be referenced") {
        assert(RuleBasedTreasuryScript.toString != null)
    }

    // TODO: restore once hash issue is fixed in Scalus
    ignore("Script compiles producing expected hash") {
        assertResult(
          ScriptHash.fromHex("d5cc516d835e1abfdb448f02fdeb7d168c1bd48ec56a4ed3e2f70c7a"),
          "Script hash should be stable. In case the script is modified or Scalus is bumped please update the test."
        ) {
            RuleBasedTreasuryScript.compiledScriptHash
        }
    }

    ignore("Script compiles producing expected size") {
        assertResult(
          11244,
          "Script size should be stable. In case the script is modified por Scalus is bumped lease update the test."
        ) {
            RuleBasedTreasuryScript.flatEncoded.length
        }
    }

    test("Membership check: empty accumulator / subset") {

        val crs_g2 = TrustedSetup.takeSrsG2(1).map(BLS12_381_G2_Element.apply)
        val accumulator = TrustedSetup.takeSrsG1(1).head |> BLS12_381_G1_Element.apply
        val subset: List[ScalusScalar] = List()
        val proof = accumulator

        assert(
          RuleBasedTreasuryValidator.checkMembership(crs_g2, accumulator, subset, proof)
        )
    }

    test("Commitment calculation: empty subset") {

        // Hashes of utxos
        val subsetScalus: List[ScalusScalar] = List.empty

        val subsetBlst = subsetScalus.map(ss => Scalar().from_bendian(ss._1.toByteArray))
        println(s"blst utxos active hashes: ${subsetBlst.map(e => BigInt.apply(e.to_bendian()))}")

        val commitmentPoint1 = KzgCommitment.calculateKzgCommitment(subsetBlst)
        val commitmentPoint2 = KzgCommitment.calculateKzgCommitment(subsetBlst)

        assert(
          HexUtil.encodeHexString(IArray.genericWrapArray(commitmentPoint1).toArray) ==
              HexUtil.encodeHexString(IArray.genericWrapArray(commitmentPoint2).toArray)
        )

        assert(
          HexUtil.encodeHexString(IArray.genericWrapArray(commitmentPoint1).toArray) ==
              "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
        )
    }

    test("Commitment calculation smoke-test") {

        // Hashes of utxos
        val subsetScalus = List(
          ScalusScalar("5088390254917556218676958430080367916099549701669885042964937592926").get,
          ScalusScalar("10660627058191719947148018507418106787651292500144510379470545154509").get
        )

        val subsetBlst = subsetScalus.map(ss => Scalar().from_bendian(ss._1.toByteArray))
        println(s"blst utxos active hashes: ${subsetBlst.map(e => BigInt.apply(e.to_bendian()))}")

        val commitmentPoint = KzgCommitment.calculateKzgCommitment(subsetBlst)

        assert(
          HexUtil.encodeHexString(IArray.genericWrapArray(commitmentPoint).toArray) ==
              "8ec51973adde24a8b6a05f62843f1c2949d01bdc642091f85a9d1803abc074616b545fd6fa25fbc467af2ef112cda832"
        )
    }

    test("Membership check smoke-test") {

        // Accumulator:
        val accumulator = BLS12_381_G1_Element(
          ByteString.fromHex(
            "8ec51973adde24a8b6a05f62843f1c2949d01bdc642091f85a9d1803abc074616b545fd6fa25fbc467af2ef112cda832"
          )
        )

        // Hashes of utxos
        val subset = List(
          ScalusScalar("5088390254917556218676958430080367916099549701669885042964937592926").get,
          ScalusScalar("10660627058191719947148018507418106787651292500144510379470545154509").get
        )

        // Proof
        val proof = G1.generator

        // Pre-calculated powers of tau
        val crsG2 = TrustedSetup.takeSrsG2(subset.length.toInt + 1).map(BLS12_381_G2_Element.apply)

        assert(
          RuleBasedTreasuryValidator.checkMembership(crsG2, accumulator, subset, proof)
        )
    }

    test("Deinit redeemer") {
        val ctxData = Data.fromJson(
          Source
              .fromResource("deinit-ctx-01.json")
              .getLines()
              .foldLeft("")((acc, line) => acc + line)
        )
        Data.fromData[ScriptContext](ctxData)
        // println(ctx)

        RuleBasedTreasuryValidator.validate(ctxData)
    }

}
