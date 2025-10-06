package hydrozoa.rulebased.ledger.dapp.script.plutus

import munit.FunSuite
import scalus.cardano.ledger.ScriptHash

class DisputeResolutionScriptTest extends FunSuite {

    test("DisputeResolutionScript object exists and can be referenced") {
        assertNotEquals(DisputeResolutionScript.toString, null)
    }

    test("Script compiles, size and hash is still the same") {
        assertEquals(
          DisputeResolutionScript.compiledScriptHash,
          ScriptHash.fromHex("1c547f989c9bea54e25ca36c938d51b15b7b88b46d0e6432a5bf1890"),
          "Script hash should be stable. In case the script is modified please update the test."
        )

        assertEquals(
          DisputeResolutionScript.flatEncoded.length,
          10169,
          "Script size should be stable. In case the script is modified please update the test."
        )

    }
}
