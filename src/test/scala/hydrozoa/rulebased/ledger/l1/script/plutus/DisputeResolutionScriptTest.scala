package hydrozoa.rulebased.ledger.l1.script.plutus

import munit.FunSuite
import scalus.cardano.ledger.ScriptHash

class DisputeResolutionScriptTest extends FunSuite {

    test("DisputeResolutionScript object exists and can be referenced") {
        assertNotEquals(DisputeResolutionScript.toString, null)
    }

    // FIXME: Script compilation currently has issues with FromData/ToData circular dependencies
    // This test is disabled until the underlying Scalus compilation issues are resolved
    //
    // The error involves infinite recursion in flat encoding during script compilation:
    // - Module linking errors for VoteDatum and related types
    // - Stack overflow in scalus.serialization.flat.package$.w7l
    // - Circular dependencies in FromData/ToData derivation
    //
    test("Script compiles, size and hash is still the same") {
        assertEquals(
          DisputeResolutionScript.compiledScriptHash,
          ScriptHash.fromHex("deedbeef"),
          "Script hash should be stable. In case the script is modified please update the test."
        )
    }
}
