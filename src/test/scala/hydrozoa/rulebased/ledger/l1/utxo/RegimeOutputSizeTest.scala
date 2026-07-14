package hydrozoa.rulebased.ledger.l1.utxo

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.node.MultiNodeConfig
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Sized, TransactionOutput}
import test.{PeersNumberSpec, TestPeerName, TestPeersSpec}

/** Measures the CBOR size of the rule-based regime utxo, validating
  * `FallbackContingency.Assumptions.maxRegimeUtxoBytes` (which sizes the regime utxo's minAda).
  *
  * The regime datum is dominated by the two peer vkey lists (32 bytes each), so the size grows with
  * the peer count; measure at the maximum test head size and assert headroom under the assumption.
  */
class RegimeOutputSizeTest extends AnyFunSuite:

    test("regime output CBOR size is within maxRegimeUtxoBytes"):
        // Measure at the maximum head size so the assumption is validated as a ceiling, not just for
        // small heads: the datum grows ~one compressed vkey (32 bytes) per head/coil peer.
        val spec = TestPeersSpec.default
            .withPeersNumberSpec(PeersNumberSpec.Exact(TestPeerName.maxPeers))
        val env = MultiNodeConfig
            .generate(spec)()
            .pureApply(Gen.Parameters.default, Seed(0L))
        val config = env.headConfig

        val sized = Sized[TransactionOutput](RuleBasedRegimeOutput.toOutput(using config))
        val bound: Int = FallbackContingency.Assumptions.maxRegimeUtxoBytes
        println(
          f"regime output  peers=${config.nHeadPeers.toInt}%2d  cborBytes=${sized.size}%5d  bound=$bound%5d"
        )

        assert(
          sized.size <= bound,
          s"regime output measured ${sized.size} CBOR bytes; exceeds maxRegimeUtxoBytes $bound"
        )
