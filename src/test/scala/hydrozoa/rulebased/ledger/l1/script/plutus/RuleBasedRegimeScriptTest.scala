package hydrozoa.rulebased.ledger.l1.script.plutus

import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedRegimeValidator.RegimeRedeemer
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedRegimeValidator.given
import hydrozoa.rulebased.ledger.l1.state.RegimeState.RuleBasedRegimeDatum
import hydrozoa.rulebased.ledger.l1.state.RegimeState.given
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{List, Option}
import scalus.cardano.onchain.plutus.v1.Value.+
import scalus.cardano.onchain.plutus.v1.{Address, Credential, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.cardano.onchain.plutus.v3.{PubKeyHash, TxId, TxInInfo, TxInfo, TxOutRef}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData

/** GUM-305: the regime utxo used to sit at the head multisig address, where any sweep of that
  * address could spend it and strand the rule-based treasury. It now sits at
  * [[RuleBasedRegimeValidator]], which accepts a spend only when the tx burns both the HRWT the
  * utxo holds and the treasury's beacon token — i.e. only in a deinit tx.
  *
  * Each test drives the real on-chain validator on the JVM and varies only the tx's mint field.
  */
class RuleBasedRegimeScriptTest extends AnyFunSuite {

    private val headMp: ByteString = ByteString.fromHex("a0" * 28)
    // CIP-67 HRWT (label 4798 -> prefix 0x012be4e0) + 28-byte head suffix.
    private val regimeTokenName: ByteString = ByteString.fromHex("012be4e0" + "bb" * 28)
    // CIP-67 HYDR (label 4937 -> prefix 0x01349900) treasury beacon + the same head suffix.
    private val beaconName: ByteString = ByteString.fromHex("01349900" + "bb" * 28)

    private val regimeScriptAddr: Address =
        Address.fromScriptHash(ByteString.fromHex("33" * 28))
    private val sweeperAddr: Address =
        Address.fromCredential(
          Credential.PubKeyCredential(PubKeyHash(ByteString.fromHex("ee" * 28)))
        )

    private val regimeRef: TxOutRef = TxOutRef(TxId(ByteString.fromHex("ab" * 32)), BigInt(0))

    private val regimeDatum: RuleBasedRegimeDatum = RuleBasedRegimeDatum(
      disputeId = ByteString.fromHex("01ff" + "aa" * 30),
      headPeers = List.empty,
      headPeersN = BigInt(0),
      coilPeers = List.empty,
      coilQuorum = BigInt(0),
      setupG2Ladder = TxOutRef(TxId(ByteString.fromHex("ef" * 32)), BigInt(0))
    )

    private val regimeInput: TxInInfo = TxInInfo(
      regimeRef,
      TxOut(
        regimeScriptAddr,
        Value.lovelace(BigInt(2_000_000)) + Value(headMp, regimeTokenName, BigInt(1)),
        OutputDatum.OutputDatum(regimeDatum.toData)
      )
    )

    private val burnRegimeToken: Value = Value(headMp, regimeTokenName, BigInt(-1))
    private val burnTreasuryBeacon: Value = Value(headMp, beaconName, BigInt(-1))

    /** Drives the real on-chain spend against a tx that consumes the regime utxo and mints `mint`.
      * Returns Success iff the validator accepts the tx.
      */
    private def runSpend(mint: Value): scala.util.Try[Unit] = {
        val txInfo = TxInfo(
          inputs = List.single(regimeInput),
          outputs = List.single(TxOut(sweeperAddr, Value.lovelace(BigInt(2_000_000)))),
          mint = mint,
          id = TxId(ByteString.fromHex("cd" * 32))
        )
        scala.util.Try(
          RuleBasedRegimeValidator.spend(
            Option.Some(regimeDatum.toData),
            RegimeRedeemer.Deinit.toData,
            txInfo,
            regimeRef
          )
        )
    }

    test("deinit burning the HRWT and the treasury beacon is accepted") {
        // Baseline: the deinit tx shape, proving the rejections below turn on the mint alone.
        val deinit = runSpend(burnRegimeToken + burnTreasuryBeacon)
        assert(deinit.isSuccess, s"a deinit-shaped burn should be accepted, got: $deinit")
    }

    test("a sweep that burns nothing is rejected") {
        // The bug this validator closes: an automated sweep moving the utxo's contents elsewhere.
        val sweep = runSpend(Value.zero)
        assert(sweep.isFailure, s"a spend burning nothing must be rejected, got: $sweep")
    }

    test("burning the HRWT alone is rejected") {
        // Would leave a treasury that can never resolve or evacuate: the rule-based validators read
        // the head identity from the regime utxo as a reference input.
        val orphaned = runSpend(burnRegimeToken)
        assert(
          orphaned.isFailure,
          s"burning the HRWT without the treasury beacon must be rejected, got: $orphaned"
        )
    }

    test("burning the treasury beacon alone is rejected") {
        val survivingToken = runSpend(burnTreasuryBeacon)
        assert(
          survivingToken.isFailure,
          s"spending without burning the HRWT must be rejected, got: $survivingToken"
        )
    }

    test("burning more than one treasury beacon is rejected") {
        // A head has exactly one beacon, so a burn of any other shape is not a deinit tx.
        val otherBeaconName = ByteString.fromHex("01349900" + "cc" * 28)
        val twoNames = runSpend(
          burnRegimeToken + burnTreasuryBeacon + Value(headMp, otherBeaconName, BigInt(-1))
        )
        assert(
          twoNames.isFailure,
          s"burning two beacon-prefixed tokens must be rejected, got: $twoNames"
        )
        val twoUnits = runSpend(burnRegimeToken + Value(headMp, beaconName, BigInt(-2)))
        assert(
          twoUnits.isFailure,
          s"burning two units of the beacon must be rejected, got: $twoUnits"
        )
    }

    test("burning look-alike tokens under another policy is rejected") {
        // The head multisig policy is the only authentication the validator has: burning under it
        // takes the head's unanimous witness, so a foreign policy must not satisfy the check.
        val otherMp = ByteString.fromHex("b1" * 28)
        val impostor = runSpend(
          Value(otherMp, regimeTokenName, BigInt(-1)) + Value(otherMp, beaconName, BigInt(-1))
        )
        assert(
          impostor.isFailure,
          s"burns under a foreign policy must be rejected, got: $impostor"
        )
    }
}
