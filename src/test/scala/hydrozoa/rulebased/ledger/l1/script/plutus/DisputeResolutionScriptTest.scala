package hydrozoa.rulebased.ledger.l1.script.plutus

import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scalus.cardano.ledger.ScriptHash

@nowarn("msg=unused value")
class DisputeResolutionScriptTest extends AnyFunSuite {

    test("DisputeResolutionScript object exists and can be referenced") {
        assert(DisputeResolutionScript.toString != null)
    }

    test("Script compiles producing expected hash") {
        val goldenHash =
            ScriptHash.fromHex("af2157ca1fdaa92369aa586370b22a105419e3ef4e2a86a82de95f9f")
        if DisputeResolutionScript.compiledScriptHash != goldenHash then {
            DisputeResolutionScript.writePlutusFile("disputeResolution.plutus")
        }
        assertResult(
          goldenHash,
          "Script hash should be stable. In case the script is modified or Scalus is bumped please update the test."
        ) {
            DisputeResolutionScript.compiledScriptHash
        }
    }

    test("Script compiles producing expected size") {
        assertResult(
          9322,
          "Script size should be stable. In case the script is modified please update the test."
        ) {
            DisputeResolutionScript.flatEncoded.length
        }
    }
}
