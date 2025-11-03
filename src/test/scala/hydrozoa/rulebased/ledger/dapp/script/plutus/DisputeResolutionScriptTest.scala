package hydrozoa.rulebased.ledger.dapp.script.plutus

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ScriptHash

import scala.annotation.nowarn

@nowarn("msg=unused value")
class DisputeResolutionScriptTest extends AnyFunSuite {

    test("DisputeResolutionScript object exists and can be referenced") {
        assert(DisputeResolutionScript.toString != null)
    }

    test("Script compiles, size and hash is still the same") {

        assertResult(
          ScriptHash.fromHex("4d09665e6f52af8ce9b3ff38b3d59ea58218b3e652c867fe1c90e63e"),
          "Script hash should be stable. In case the script is modified or Scalus is bumped please update the test."
        ) {
            DisputeResolutionScript.compiledScriptHash
        }

        assertResult(
          13383,
          "Script size should be stable. In case the script is modified please update the test."
        ) {
            DisputeResolutionScript.flatEncoded.length
        }
    }
}
