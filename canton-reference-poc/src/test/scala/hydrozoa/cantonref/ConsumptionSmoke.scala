package hydrozoa.cantonref

import daml.splice.api.token.holdingv2.InstrumentId
import org.scalatest.funsuite.AnyFunSuite
import tokenstandard.PartyId
import tokenstandard.engine.DamlEngine
import tokenstandard.engine.EngineLedger
import tokenstandard.engine.EngineStore

/** Proves Hydrozoa can point at the canton-reference-registry library: consume its fast engine
  * reference ledger (ProjectRef to the `engine` module, DARs loaded from the bundled classpath),
  * deploy a registry and seed a holding, entirely in-process. Foundation for the two-step
  * allocation PoC.
  */
class ConsumptionSmoke extends AnyFunSuite:

    test("consume the engine reference ledger: create rules, seed, read balance"):
        val reg = PartyId("reg")
        val alice = PartyId("alice")
        val xId = new InstrumentId(reg.value, "X")

        val engine = DamlEngine.load()
        val ledger = new EngineLedger(engine)

        val program = for
            _ <- ledger.createTokenRules(reg)
            _ <- ledger.seedHolding(reg, alice, xId, BigDecimal(1000))
            bal <- ledger.unlockedBalance(alice, alice, xId)
        yield bal

        val result = program.run(EngineStore.empty)
        assert(result.map(_._2) == Right(BigDecimal(1000)), s"got $result")
