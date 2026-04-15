package hydrozoa.rulebased.ledger.l1.script.plutus

import hydrozoa.config.HydrozoaBlueprint
import java.io.File
import java.nio.file.Files
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scalus.cardano.blueprint.Blueprint

@nowarn("msg=unused value")
class ExportTest extends AnyFunSuite {

    private val blueprintPath = HydrozoaBlueprint.blueprintFilePath

    test("Blueprint file exists") {
        val file = new File(blueprintPath)
        assert(file.exists(), s"Blueprint file should exist at $blueprintPath")
        assert(file.isFile, "Blueprint path should be a file, not a directory")
    }

    test("Blueprint file is valid CIP-57 JSON") {
        val file = new File(blueprintPath)
        val json = Files.readString(file.toPath)

        // Should parse without throwing an exception
        val blueprint = Blueprint.fromJson(json)

        // Basic structure checks
        assert(blueprint.preamble.title == "Hydrozoa Rule-Based Regime Validators")
        assert(blueprint.preamble.version.contains("1.0.0"))
        assert(blueprint.preamble.license.contains("Apache-2.0"))
        assert(blueprint.validators.length == 2)
    }

    test("Blueprint is up-to-date with compiled scripts") {
        // Generate fresh blueprint from source
        val freshBlueprint = Export.createBlueprint()

        // Load existing blueprint from file
        val file = new File(blueprintPath)
        val existingJson = Files.readString(file.toPath)
        val existingBlueprint = Blueprint.fromJson(existingJson)

        // Compare validators
        assert(
          freshBlueprint.validators.length == existingBlueprint.validators.length,
          "Number of validators should match"
        )

        // Check Dispute Resolution Validator
        val freshDispute = freshBlueprint.validators.head
        val existingDispute = existingBlueprint.validators.head

        assertResult(
          existingDispute.hash,
          "Dispute Resolution script hash mismatch. " +
              s"Expected: ${existingDispute.hash}, " +
              s"Got: ${freshDispute.hash}. " +
              "Please run: nix develop --command sbtn 'runMain hydrozoa.rulebased.ledger.l1.script.plutus.Export'"
        ) {
            freshDispute.hash
        }

        assertResult(
          existingDispute.compiledCode,
          "Dispute Resolution compiled code should match. " +
              "Please run: nix develop --command sbtn 'runMain hydrozoa.rulebased.ledger.l1.script.plutus.Export'"
        ) {
            freshDispute.compiledCode
        }

        // Check Rule-Based Treasury Validator
        val freshTreasury = freshBlueprint.validators(1)
        val existingTreasury = existingBlueprint.validators(1)

        assertResult(
          existingTreasury.hash,
          "Rule-Based Treasury script hash mismatch. " +
              s"Expected: ${existingTreasury.hash}, " +
              s"Got: ${freshTreasury.hash}. " +
              "Please run: nix develop --command sbtn 'runMain hydrozoa.rulebased.ledger.l1.script.plutus.Export'"
        ) {
            freshTreasury.hash
        }

        assertResult(
          existingTreasury.compiledCode,
          "Rule-Based Treasury compiled code should match. " +
              "Please run: nix develop --command sbtn 'runMain hydrozoa.rulebased.ledger.l1.script.plutus.Export'"
        ) {
            freshTreasury.compiledCode
        }
    }

    test("Blueprint contains correct validator metadata") {
        val file = new File(blueprintPath)
        val json = Files.readString(file.toPath)
        val blueprint = Blueprint.fromJson(json)

        // Check Dispute Resolution Validator
        val disputeValidator = blueprint.validators.head
        assert(disputeValidator.title == "Dispute Resolution Validator")
        assert(disputeValidator.datum.isDefined, "Dispute validator should have datum schema")
        assert(disputeValidator.redeemer.isDefined, "Dispute validator should have redeemer schema")
        assert(
          disputeValidator.compiledCode.isDefined,
          "Dispute validator should have compiled code"
        )
        assert(disputeValidator.hash.isDefined, "Dispute validator should have script hash")

        // Check Rule-Based Treasury Validator
        val treasuryValidator = blueprint.validators(1)
        assert(treasuryValidator.datum.isDefined, "Treasury validator should have datum schema")
        assert(
          treasuryValidator.redeemer.isDefined,
          "Treasury validator should have redeemer schema"
        )
        assert(
          treasuryValidator.compiledCode.isDefined,
          "Treasury validator should have compiled code"
        )
        assert(treasuryValidator.hash.isDefined, "Treasury validator should have script hash")
    }

    test("Blueprint datum and redeemer schemas are present") {
        val file = new File(blueprintPath)
        val json = Files.readString(file.toPath)
        val blueprint = Blueprint.fromJson(json)

        // Check Dispute Resolution schemas
        val disputeValidator = blueprint.validators.head
        val disputeDatum = disputeValidator.datum.get
        assert(disputeDatum.schema.title.contains("VoteDatum"))
        assert(disputeDatum.schema.fields.isDefined, "VoteDatum should have fields")

        val disputeRedeemer = disputeValidator.redeemer.get
        assert(disputeRedeemer.schema.title.contains("DisputeRedeemer"))
        assert(
          disputeRedeemer.schema.anyOf.isDefined,
          "DisputeRedeemer should be an enum with anyOf"
        )

        // Check Rule-Based Treasury schemas
        val treasuryValidator = blueprint.validators(1)
        val treasuryDatum = treasuryValidator.datum.get
        assert(treasuryDatum.schema.title.contains("RuleBasedTreasuryDatumOnchain"))
        assert(
          treasuryDatum.schema.anyOf.isDefined,
          "RuleBasedTreasuryDatumOnchain should be an enum"
        )

        val treasuryRedeemer = treasuryValidator.redeemer.get
        assert(treasuryRedeemer.schema.title.contains("TreasuryRedeemer"))
        assert(treasuryRedeemer.schema.anyOf.isDefined, "TreasuryRedeemer should be an enum")
    }
}
