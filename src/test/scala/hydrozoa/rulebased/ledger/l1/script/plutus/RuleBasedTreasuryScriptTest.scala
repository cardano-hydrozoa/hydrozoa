package hydrozoa.rulebased.ledger.l1.script.plutus

import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.multisig.ledger.commitment.{KzgCommitment, TrustedSetup}
import hydrozoa.multisig.ledger.joint.EvacuationKey
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer}
import hydrozoa.rulebased.ledger.l1.state.RegimeState.RuleBasedRegimeDatum
import hydrozoa.rulebased.ledger.l1.state.RegimeState.given
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.given
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scala.io.Source
import scalus.cardano.onchain.plutus.prelude.bls12_381.G1
import scalus.cardano.onchain.plutus.prelude.{List, Option}
import scalus.cardano.onchain.plutus.v1.Value.+
import scalus.cardano.onchain.plutus.v1.{Address, Credential, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.cardano.onchain.plutus.v3.{PubKeyHash, ScriptContext, TxId, TxInInfo, TxInfo, TxOutRef}
import scalus.uplc.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.bls12_381.*
import scalus.uplc.builtin.{ByteString, Data, ToData}
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

    // ---- C-01 / L-01: Evacuate output-address pinning + no-progress guard (security review) ----
    //
    // C-01: `Evacuate` must keep the continuing treasury output at the treasury script address —
    //       otherwise the beacon + the whole treasury value can be redirected to an attacker.
    // L-01: `Evacuate` must remove at least one utxo — a zero-evacuatee no-op just re-creates the
    //       treasury (permissionlessly) and is a UTxO-contention DoS vector against real evacuations.
    //
    // Both tests drive the real on-chain `Evacuate` (the BLS membership pairing runs on the JVM via
    // blst), using a genuine single-utxo evacuation: the active set is {evacuee}, so evacuating it
    // leaves an empty residual whose KZG commitment is the G1 generator.

    private val headMp: ByteString = ByteString.fromHex("a0" * 28)
    // CIP-67 HYDR (label 4937 -> prefix 0x01349900) beacon prefix + 28-byte head suffix.
    private val beaconName: ByteString = ByteString.fromHex("01349900" + "bb" * 28)

    // Compressed BLS12-381 G1 generator == KZG commitment of the empty set (see the empty-subset
    // commitment test) — i.e. the residual accumulator once the whole active set is evacuated.
    private val g1Generator: ByteString = ByteString.fromHex(
      "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
    )
    private val setupG2: List[ByteString] =
        TrustedSetup.takeSrsG2(4).map(p2 => G2Element(p2).toCompressedByteString)

    private val treasuryScriptAddr: Address =
        Address.fromScriptHash(ByteString.fromHex("11" * 28))
    private val attackerAddr: Address =
        Address.fromCredential(
          Credential.PubKeyCredential(PubKeyHash(ByteString.fromHex("ee" * 28)))
        )
    private val evacueeAddr: Address =
        Address.fromCredential(
          Credential.PubKeyCredential(PubKeyHash(ByteString.fromHex("cc" * 28)))
        )

    private val treasuryRef: TxOutRef = TxOutRef(TxId(ByteString.fromHex("ab" * 32)), BigInt(0))

    // The single active evacuee: its committed output, plus the active-set accumulator over the
    // (key, output) hash computed exactly as the validator recomputes it.
    private val evacueeKey: EvacuationKey = EvacuationKey(ByteString.fromHex("dd" * 32)).get
    private val evacueeOutput: TxOut = TxOut(evacueeAddr, Value.lovelace(BigInt(30_000_000)))
    private val activeSetAccumulator: ByteString = {
        val scalar = (evacueeKey, evacueeOutput)
            |> ToData.tupleToData |> serialiseData |> blake2b_224
            |> ScalusScalar.fromByteStringBigEndianUnsafe
        KzgCommitment.calculateKzgCommitment(
          List.single(Scalar().from_bendian(scalar._1.toByteArray))
        )
    }

    private val treasuryInValue: Value =
        Value.lovelace(BigInt(100_000_000)) + Value(headMp, beaconName, BigInt(1))
    // After evacuating the 30-ADA evacuee, the beacon + remaining 70 ADA stay in the treasury.
    private val treasuryResidualValue: Value =
        Value.lovelace(BigInt(70_000_000)) + Value(headMp, beaconName, BigInt(1))

    /** Drives the real on-chain `Evacuate`. Returns Success iff the validator accepts the tx. */
    private def runEvacuate(
        inAccumulator: ByteString,
        outAccumulator: ByteString,
        proof: ByteString,
        evacuationKeys: List[EvacuationKey],
        evacuationOutputs: List[TxOut],
        treasuryOutputAddr: Address,
        treasuryOutputValue: Value
    ): scala.util.Try[Unit] = {
        def datum(acc: ByteString): RuleBasedTreasuryDatum =
            RuleBasedTreasuryDatum.Resolved(
              headMp = headMp,
              evacuationActive = acc,
              version = (BigInt(1), BigInt(0))
            )
        val treasuryInput = TxInInfo(
          treasuryRef,
          TxOut(
            treasuryScriptAddr,
            treasuryInValue,
            OutputDatum.OutputDatum(datum(inAccumulator).toData)
          )
        )
        // Setup-ladder reference input: inline List[ByteString] datum with the G2 setup prefix.
        val ladderOutRef = TxOutRef(TxId(ByteString.fromHex("ef" * 32)), BigInt(0))
        val ladderRefInput = TxInInfo(
          ladderOutRef,
          TxOut(
            Address.fromScriptHash(ByteString.fromHex("22" * 28)),
            Value.lovelace(BigInt(5_000_000)),
            OutputDatum.OutputDatum(setupG2.toData)
          )
        )
        // Regime reference input: HRWT beacon under headMp + head-identity datum whose
        // setupLadder anchor is the ladder ref input's outRef (authenticating the rung).
        val regimeDatum = RuleBasedRegimeDatum(
          disputeId = ByteString.fromHex("01ff" + "aa" * 30),
          headPeers = List.empty,
          headPeersN = BigInt(0),
          coilPeers = List.empty,
          coilQuorum = BigInt(0),
          setupLadder = ladderOutRef
        )
        val regimeRefInput = TxInInfo(
          TxOutRef(TxId(ByteString.fromHex("ee" * 32)), BigInt(0)),
          TxOut(
            Address.fromScriptHash(ByteString.fromHex("33" * 28)),
            Value.lovelace(BigInt(2_000_000))
                + Value(headMp, ByteString.fromHex("012be4e0" + "bb" * 28), BigInt(1)),
            OutputDatum.OutputDatum(regimeDatum.toData)
          )
        )
        val changeOutput = TxOut(attackerAddr, Value.lovelace(BigInt(2_000_000)))
        val treasuryOutput = TxOut(
          treasuryOutputAddr,
          treasuryOutputValue,
          OutputDatum.OutputDatum(datum(outAccumulator).toData)
        )
        val txInfo = TxInfo(
          inputs = List(treasuryInput),
          referenceInputs = List.Cons(ladderRefInput, List.single(regimeRefInput)),
          outputs = List.Cons(changeOutput, List.Cons(treasuryOutput, evacuationOutputs)),
          id = TxId(ByteString.fromHex("cd" * 32))
        )
        val redeemer: TreasuryRedeemer =
            TreasuryRedeemer.Evacuate(
              EvacuateRedeemer(
                evacuationKeys = evacuationKeys,
                proof = proof,
                setupRefInputIdx = BigInt(0)
              )
            )
        scala.util.Try(
          RuleBasedTreasuryValidator.spend(
            Option.Some(datum(inAccumulator).toData),
            redeemer.toData,
            txInfo,
            treasuryRef
          )
        )
    }

    // A valid single-utxo evacuation: active set {evacuee} -> residual empty (proof = generator).
    private def runSingleEvacuate(treasuryOutputAddr: Address): scala.util.Try[Unit] =
        runEvacuate(
          inAccumulator = activeSetAccumulator,
          outAccumulator = g1Generator,
          proof = g1Generator,
          evacuationKeys = List.single(evacueeKey),
          evacuationOutputs = List.single(evacueeOutput),
          treasuryOutputAddr = treasuryOutputAddr,
          treasuryOutputValue = treasuryResidualValue
        )

    test("C-01: Evacuate pins the treasury output address (redirect rejected)") {
        // Baseline: a real single-utxo evacuation returning the residual treasury to the script
        // address is accepted — proving the tx is otherwise valid.
        assert(
          runSingleEvacuate(treasuryScriptAddr).isSuccess,
          "honest evacuation (residual treasury -> script address) should be accepted"
        )
        // ATTACK: identical tx, but the residual treasury output (beacon + remaining funds) is
        // addressed to the attacker. The address check must reject it.
        val attack = runSingleEvacuate(attackerAddr)
        assert(
          attack.isFailure,
          s"C-01: residual treasury output at a non-script address must be rejected, got: $attack"
        )
    }

    test("L-01: Evacuate rejects a zero-evacuatee no-op") {
        // The empty evacuation is otherwise valid (proof = unchanged accumulator, full value kept
        // at the script address); only the no-progress guard rejects it. Before the L-01 guard this
        // was accepted and was a permissionless treasury-UTxO-contention DoS vector.
        val noop = runEvacuate(
          inAccumulator = g1Generator,
          outAccumulator = g1Generator,
          proof = g1Generator,
          evacuationKeys = List.empty[EvacuationKey],
          evacuationOutputs = List.empty[TxOut],
          treasuryOutputAddr = treasuryScriptAddr,
          treasuryOutputValue = treasuryInValue
        )
        assert(
          noop.isFailure,
          s"L-01: empty (no-progress) evacuation must be rejected, got: $noop"
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
