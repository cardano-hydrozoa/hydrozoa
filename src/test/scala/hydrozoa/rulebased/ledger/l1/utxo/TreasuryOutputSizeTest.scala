package hydrozoa.rulebased.ledger.l1.utxo

import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.{Resolved, Unresolved}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.ledger.{AssetName, Coin, Sized, TransactionOutput, Value}
import scalus.uplc.builtin.ByteString
import test.TestPeersSpec

/** Measures the CBOR size and minAda of no-liabilities treasury outputs.
  *
  * The treasury datum is constant-size (the G2 setup and head-identity fields live in the setup
  * ladder / regime utxos), so a single measurement per datum shape suffices. The measured size
  * informs `FallbackContingency.Assumptions.maxNoLiabilitiesTreasuryUtxoBytes`.
  */
class TreasuryOutputSizeTest extends AnyFunSuite:

    test("no-liabilities treasury output CBOR size and minAda (Unresolved and Resolved)"):
        val env = MultiNodeConfig
            .generate(TestPeersSpec.default)()
            .pureApply(Gen.Parameters.default, Seed(0L))
        val config = env.headConfig
        val headMp = config.headMultisigScript.policyId

        val beaconToken = Value.asset(
          headMp,
          AssetName(config.headTokenNames.treasuryTokenName.bytes),
          1
        )
        // Minimal realistic value: 2 ADA + treasury beacon token.
        val baseValue = Value(Coin.ada(2)) + beaconToken

        val unresolved = Unresolved(
          headMp = headMp,
          deadlineVoting = BigInt(2_000_000_000L),
          versionMajor = BigInt(1)
        )
        // A KZG membership proof is a compressed G1 point (48 bytes).
        val resolved = Resolved(
          headMp = headMp,
          evacuationActive = ByteString.fromArray(Array.fill(48)(0xff.toByte)),
          version = (BigInt(1), BigInt(2))
        )

        val rows = List("Unresolved" -> unresolved, "Resolved" -> resolved).map { (name, datum) =>
            val output: TransactionOutput =
                RuleBasedTreasuryOutput(datum, baseValue).toOutput(using config)
            val sized = Sized(output)
            val minAda =
                MinCoinSizedTransactionOutput.ensureMinAda(sized, config.cardanoProtocolParams)
            println(f"$name%12s  cborBytes=${sized.size}%5d  minAda=${minAda.value}%9d lovelace")
            (name, sized.size, minAda)
        }

        // The slim datum makes the treasury output small and constant-size; assert a generous
        // upper bound well under FallbackContingency's 6960-byte assumption.
        rows.foreach { (name, size, minAda) =>
            val _ = assert(
              size <= 400,
              s"$name treasury output measured $size CBOR bytes; expected <= 400"
            )
            assert(minAda.value > 0, s"$name minAda must be positive (was ${minAda.value})")
        }
