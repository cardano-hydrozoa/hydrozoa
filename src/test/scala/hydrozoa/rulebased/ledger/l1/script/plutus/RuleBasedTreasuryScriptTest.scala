package hydrozoa.rulebased.ledger.l1.script.plutus

import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.multisig.ledger.commitment.{KzgCommitment, TrustedSetup}
import hydrozoa.multisig.ledger.joint.EvacuationKey
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatumOnchain
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.given
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scala.io.Source
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.G1
import scalus.cardano.onchain.plutus.prelude.{List, Option}
import scalus.cardano.onchain.plutus.v1.Value.+
import scalus.cardano.onchain.plutus.v1.{Address, Credential, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.cardano.onchain.plutus.v3.{PubKeyHash, ScriptContext, TxId, TxInInfo, TxInfo, TxOutRef}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.bls12_381.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.|>
import supranational.blst.Scalar

@nowarn("msg=unused value")
class RuleBasedTreasuryScriptTest extends AnyFunSuite {

    test("Membership check: empty accumulator / subset") {

        val crs_g2 = TrustedSetup.takeSrsG2(1).map(G2Element.apply)
        val accumulator = TrustedSetup.takeSrsG1(1).head |> G1Element.apply
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
          commitmentPoint1 == commitmentPoint2
        )

        assert(
          commitmentPoint1.toHex ==
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
          commitmentPoint.toHex ==
              "8ec51973adde24a8b6a05f62843f1c2949d01bdc642091f85a9d1803abc074616b545fd6fa25fbc467af2ef112cda832"
        )
    }

    test("Membership check smoke-test") {

        // Accumulator:
        val accumulator = G1Element(
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
        val crsG2 = TrustedSetup.takeSrsG2(subset.length.toInt + 1).map(G2Element.apply)

        assert(
          RuleBasedTreasuryValidator.checkMembership(crsG2, accumulator, subset, proof)
        )
    }

    // ---- C-01: Evacuate redirect — regression guard (see security review) ------------------
    //
    // `Evacuate` must keep the continuing treasury output at the treasury script address. The
    // other checks only constrain that output's beacon, datum and total value — not where it
    // goes. Without the address check, an empty-subset evacuation (proof = the current
    // accumulator) is forced by value-preservation to put the ENTIRE treasury into the continuing
    // output, which an attacker can simply address to themselves. This test pins the fix:
    // identical txs, the only difference being the treasury output address.

    private val headMp: ByteString = ByteString.fromHex("a0" * 28)
    // CIP-67 HYDR (label 4937 -> prefix 0x01349900) beacon prefix + 28-byte head suffix.
    private val beaconName: ByteString = ByteString.fromHex("01349900" + "bb" * 28)

    private val treasuryValue: Value =
        Value.lovelace(BigInt(100_000_000)) + Value(headMp, beaconName, BigInt(1))

    // Compressed BLS12-381 G1 generator — a valid G1 point, reused as accumulator AND proof so
    // the empty-subset membership check e(acc, g2) == e(proof, g2) holds (it reduces to acc == proof).
    private val g1Point: ByteString = ByteString.fromHex(
      "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
    )
    private val setupG2: List[ByteString] =
        TrustedSetup.takeSrsG2(1).map(p2 => G2Element(p2).toCompressedByteString)

    private val resolvedDatum: RuleBasedTreasuryDatumOnchain =
        RuleBasedTreasuryDatumOnchain.ResolvedOnchain(
          headMp = headMp,
          evacuationActive = g1Point,
          version = (BigInt(1), BigInt(0)),
          setupG2 = setupG2
        )

    private val treasuryScriptAddr: Address =
        Address.fromScriptHash(ByteString.fromHex("11" * 28))
    private val attackerAddr: Address =
        Address.fromCredential(
          Credential.PubKeyCredential(PubKeyHash(ByteString.fromHex("ee" * 28)))
        )

    private val treasuryRef: TxOutRef = TxOutRef(TxId(ByteString.fromHex("ab" * 32)), BigInt(0))
    private val treasuryInput: TxInInfo = TxInInfo(
      treasuryRef,
      TxOut(treasuryScriptAddr, treasuryValue, OutputDatum.OutputDatum(resolvedDatum.toData))
    )

    /** Runs the real on-chain `Evacuate` logic for an empty-subset evacuation whose continuing
      * treasury output is addressed to `treasuryOutputAddr`. Returns Success iff the validator
      * accepts the transaction (the BLS pairing check runs on the JVM via blst).
      */
    private def runEmptyEvacuate(treasuryOutputAddr: Address): scala.util.Try[Unit] = {
        val changeOutput = TxOut(attackerAddr, Value.lovelace(BigInt(2_000_000)))
        val treasuryOutput =
            TxOut(treasuryOutputAddr, treasuryValue, OutputDatum.OutputDatum(resolvedDatum.toData))
        val txInfo = TxInfo(
          inputs = List(treasuryInput),
          outputs = List(changeOutput, treasuryOutput),
          id = TxId(ByteString.fromHex("cd" * 32))
        )
        val redeemer: TreasuryRedeemer = TreasuryRedeemer.Evacuate(
          EvacuateRedeemer(evacuationKeys = List.empty[EvacuationKey], proof = g1Point)
        )
        scala.util.Try(
          RuleBasedTreasuryValidator.spend(
            Option.Some(resolvedDatum.toData),
            redeemer.toData,
            txInfo,
            treasuryRef
          )
        )
    }

    test("C-01: Evacuate lets the resolved treasury be redirected to an attacker address") {
        // Baseline: returning the treasury to its own script address is accepted — proving the
        // transaction is otherwise valid and the ONLY thing varied below is the output address.
        assert(
          runEmptyEvacuate(treasuryScriptAddr).isSuccess,
          "baseline honest no-op evacuation (treasury -> script address) should be accepted"
        )

        // ATTACK: identical tx, but the continuing treasury output (beacon + the ENTIRE treasury
        // value) is addressed to the attacker's wallet. The address check must reject it.
        // (Before the fix this was a Success — the live C-01 vulnerability.)
        val attack = runEmptyEvacuate(attackerAddr)
        assert(
          attack.isFailure,
          s"C-01 regression: a treasury output at a non-script address must be rejected, got: $attack"
        )
    }

    // Ignoring since deinit-ctx-01.json is outdated
    ignore("Deinit redeemer") {
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
