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
          ScriptHash.fromHex("0cf0fb737c29c5c5b6eaed22f378e5d9e68974b3ff6429441870aaf3"),
          "Script hash should be stable. In case the script is modified or Scalus is bumped please update the test."
        )

        assertEquals(
          DisputeResolutionScript.flatEncoded.length,
          10507,
          "Script size should be stable. In case the script is modified please update the test."
        )

    }
}
