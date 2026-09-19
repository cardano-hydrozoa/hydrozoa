package hydrozoa.rulebased.ledger.l1.utxo

import hydrozoa.config.node.MultiNodeConfig
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import test.TestPeersSpec

/** GUM-305: the regime output must sit at the rule-based regime validator's address. It used to sit
  * at the head multisig address, where a sweep of that address could spend it and leave the
  * rule-based treasury unable to resolve or evacuate.
  */
class RuleBasedRegimeOutputTest extends AnyFunSuite:

    private val config = MultiNodeConfig
        .generate(TestPeersSpec.default)()
        .pureApply(Gen.Parameters.default, Seed(0L))
        .headConfig

    private val output = RuleBasedRegimeOutput.toOutput(using config)

    test("the regime output sits at the rule-based regime script address"):
        assert(output.address == config.ruleBasedRegimeAddress)

    test("the regime output is not at the head multisig address"):
        assert(output.address != config.headMultisigAddress)

    test("the regime output parses back as this head's regime output"):
        assert(RuleBasedRegimeOutput.validate(output)(using config).isRight)
