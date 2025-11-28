package hydrozoa.rulebased.ledger.dapp.script.plutus

import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scalus.cardano.ledger.ScriptHash

@nowarn("msg=unused value")
class DisputeResolutionScriptTest extends AnyFunSuite {

    test("DisputeResolutionScript object exists and can be referenced") {
        assert(DisputeResolutionScript.toString != null)
    }

    test("Script compiles, size and hash is still the same") {

        assertResult(
          ScriptHash.fromHex("5ec6726ee1200efbb5ea9d49f25bbefb86e5ef82d5ddb4acc2f35b36"),
          "Script hash should be stable. In case the script is modified or Scalus is bumped please update the test."
        ) {
            DisputeResolutionScript.compiledScriptHash
        }

        assertResult(
            13669,
          "Script size should be stable. In case the script is modified please update the test."
        ) {
            DisputeResolutionScript.flatEncoded.length
        }
    }
}
