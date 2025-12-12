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
          ScriptHash.fromHex("46af0a55b5c6e1d01f6c5ee0804757a676b03bc8f9d50087c528c3d8"),
          "Script hash should be stable. In case the script is modified or Scalus is bumped please update the test."
        ) {
            DisputeResolutionScript.compiledScriptHash
        }

        assertResult(
          13688,
          "Script size should be stable. In case the script is modified please update the test."
        ) {
            DisputeResolutionScript.flatEncoded.length
        }
    }
}
