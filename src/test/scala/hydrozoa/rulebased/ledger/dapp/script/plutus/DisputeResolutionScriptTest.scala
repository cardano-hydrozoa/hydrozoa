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
          ScriptHash.fromHex("7f9ad6889b55ce62629f442aa0589251c0560972d149aa97f8495fe3"),
          "Script hash should be stable. In case the script is modified or Scalus is bumped please update the test."
        )

        assertEquals(
          DisputeResolutionScript.flatEncoded.length,
          10398,
          "Script size should be stable. In case the script is modified please update the test."
        )

    }
}
