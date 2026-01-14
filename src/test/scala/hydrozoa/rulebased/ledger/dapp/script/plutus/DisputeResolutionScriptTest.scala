package hydrozoa.rulebased.ledger.dapp.script.plutus

import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scalus.cardano.ledger.ScriptHash

@nowarn("msg=unused value")
class DisputeResolutionScriptTest extends AnyFunSuite {

    test("DisputeResolutionScript object exists and can be referenced") {
        assert(DisputeResolutionScript.toString != null)
    }

    // TODO: restore once hash issue is fixed in Scalus
    ignore("Script compiles producing expected hash") {
        assertResult(
          ScriptHash.fromHex("d43241de0248c4204f950fe3ab2e0f95b0ce399b7864ed2a4c1f3f08"),
          "Script hash should be stable. In case the script is modified or Scalus is bumped please update the test."
        ) {
            DisputeResolutionScript.compiledScriptHash
        }
    }

    ignore("Script compiles producing expected size") {
        assertResult(
          13694,
          "Script size should be stable. In case the script is modified please update the test."
        ) {
            DisputeResolutionScript.flatEncoded.length
        }
    }
}
