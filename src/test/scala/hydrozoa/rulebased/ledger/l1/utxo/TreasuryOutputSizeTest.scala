package hydrozoa.rulebased.ledger.l1.utxo

import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.ledger.{AssetName, Coin, Sized, TransactionOutput, Value}
import scalus.uplc.builtin.bls12_381.G2Element
import test.TestPeersSpec

/** Prints a table of (setupSize, cborBytes, minAda lovelace) for Unresolved treasury outputs.
  *
  * Used to determine the minimum ADA that must be reserved in the treasury at resolution time so
  * that every subsequent EvacuationTx output respects minAda without the balancer needing to add
  * ADA (which would break the value-conservation invariant).
  */
class TreasuryOutputSizeTest extends AnyFunSuite:

    test("Unresolved treasury output CBOR size and minAda vs setup size (0 to 65)"):
        val env = MultiNodeConfig.generate(TestPeersSpec.default)().sample.get
        val config = env.headConfig

        // Convert to a Scala List so we can use .take(n) cleanly
        val allG2Points: List[scalus.uplc.builtin.ByteString] =
            TrustedSetup
                .takeSrsG2(EvacuationTx.Assumptions.maxEvacuationsPerTx + 1)
                .toScalaList
                .map(p2 => G2Element(p2).toCompressedByteString)

        val beaconToken = Value.asset(
          config.headMultisigScript.policyId,
          AssetName(config.headTokenNames.treasuryTokenName.bytes),
          1
        )
        // Minimal realistic value: 2 ADA + treasury beacon token.
        val baseValue = Value(Coin.ada(2)) + beaconToken

        println(f"\n${"setupSize"}%10s  ${"cborBytes"}%10s  ${"minAda (lovelace)"}%20s")
        println("-" * 46)

        val rows: IndexedSeq[(Int, Int, Coin)] =
            (0 to (EvacuationTx.Assumptions.maxEvacuationsPerTx + 1)).map { n =>
                val setup = scalus.cardano.onchain.plutus.prelude.List.from(allG2Points.take(n))
                val datum = Unresolved(
                  deadlineVoting = BigInt(2_000_000_000L),
                  versionMajor = BigInt(1),
                  setup = setup
                )
                // Upcast to TransactionOutput so the scalus Encoder[TransactionOutput] is resolved
                val output: TransactionOutput =
                    RuleBasedTreasuryOutput(datum, baseValue).toOutput(using config)
                val sized = Sized(output)
                val minAda =
                    MinCoinSizedTransactionOutput.ensureMinAda(sized, config.cardanoProtocolParams)
                println(f"$n%10d  ${sized.size}%10d  ${minAda.value}%20d")
                (n, sized.size, minAda)
            }

        assert(rows(0)._2 < rows(65)._2, "CBOR size should grow with setup length")
        assert(rows.forall(_._3.value > 0), "minAda must be positive for all setup sizes")
