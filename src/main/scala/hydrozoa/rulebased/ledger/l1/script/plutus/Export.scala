package hydrozoa.rulebased.ledger.l1.script.plutus

import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.DisputeRedeemer
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatumOnchain
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteDatum
import java.io.File
import java.nio.file.Files
import scalus.cardano.address.Network
import scalus.cardano.blueprint.Blueprint

object Export {

    /** Creates a CIP-57 compliant Blueprint describing both Hydrozoa rule-based validators:
      *   - DisputeResolutionScript: Manages voting and tallying during dispute resolution
      *   - RuleBasedTreasuryScript: Manages treasury state transitions and evacuations
      */
    def createBlueprint(): Blueprint = {
        // Create DisputeResolution validator blueprint
        val disputeValidator = Blueprint.plutusV3[VoteDatum, DisputeRedeemer](
          title = "Dispute Resolution Validator",
          description =
              "Manages voting, tallying, and resolution of disputes in Hydrozoa rule-based regime. " +
                  "Allows peers to vote on block headers, tally votes pairwise, and resolve disputes when consensus is reached.",
          version = "1.0.0",
          license = Some("Apache-2.0"),
          compiled = DisputeResolutionScript.compiledPlutusV3Program
        )

        // Create RuleBasedTreasury validator blueprint
        val treasuryValidator = Blueprint.plutusV3[RuleBasedTreasuryDatumOnchain, TreasuryRedeemer](
          title = "Rule-Based Treasury Validator",
          description = "Manages the Hydrozoa treasury during rule-based regime. " +
              "Handles dispute resolution, UTXO evacuation with KZG membership proofs, and head deinitialization.",
          version = "1.0.0",
          license = Some("Apache-2.0"),
          compiled = RuleBasedTreasuryScript.compiledPlutusV3Program
        )

        // Combine both validators into a single blueprint
        val preamble = scalus.cardano.blueprint.Preamble(
          title = "Hydrozoa Rule-Based Regime Validators",
          description = Some(
            "CIP-57 compliant blueprint for Hydrozoa's rule-based state channel validators. " +
                "These validators manage dispute resolution and treasury operations when multi-party consensus is no longer held."
          ),
          version = Some("1.0.0"),
          compiler = Some(scalus.cardano.blueprint.CompilerInfo.currentScalus),
          plutusVersion = Some(scalus.cardano.ledger.Language.PlutusV3),
          license = Some("Apache-2.0")
        )

        Blueprint(
          preamble = preamble,
          validators = Seq(
            disputeValidator.validators.head,
            treasuryValidator.validators.head
          )
        )
    }

    /** Writes the blueprint to the resources' directory.
      *
      * The output file will be at: src/main/resources/hydrozoa/scripts/plutus.json
      */
    def exportBlueprint(): Unit = {
        val blueprint = createBlueprint()
        val outputPath = HydrozoaBlueprint.blueprintFilePath

        // Create parent directories if they don't exist
        val outputFile = new File(outputPath)
        outputFile.getParentFile.mkdirs()

        // Write blueprint to file
        blueprint.writeToFile(outputFile)

        println(s"Blueprint exported to: $outputPath")
        // Extract script hashes from addresses (network doesn't matter for script hash)
        val disputeScriptHash =
            DisputeResolutionScript.address(Network.Mainnet).scriptHashOption.get
        val treasuryScriptHash =
            RuleBasedTreasuryScript.address(Network.Mainnet).scriptHashOption.get
        println(s"- Dispute Resolution Script Hash: ${disputeScriptHash.toHex}")
        println(s"- Rule-Based Treasury Script Hash: ${treasuryScriptHash.toHex}")
    }

    /** Path to the flat-encoded Dispute Resolution script in the source tree. */
    val disputeResolutionFlatFilePath: String =
        "src/main/resources/dispute_resolution.flat"

    /** Path to the flat-encoded Rule-Based Treasury script in the source tree. */
    val ruleBasedTreasuryFlatFilePath: String =
        "src/main/resources/rule_based_treasury.flat"

    /** Writes a single flat-encoded UPLC program to the given path. */
    private def writeFlatScript(flatBytes: Array[Byte], outputPath: String): Unit = {
        val outputFile = new File(outputPath)
        outputFile.getParentFile.mkdirs()
        Files.write(outputFile.toPath, flatBytes)
        println(s"Flat-encoded script exported to: $outputPath (${flatBytes.length} bytes)")
    }

    /** Writes the flat-encoded UPLC programs for both validators to the resources directory.
      *
      * The flat format is the raw UPLC binary encoding consumed by formal-verification tooling
      * (e.g. plutus-core / aiken-style toolchains). It is NOT the CBOR-wrapped script bytes that
      * appear in the CIP-57 blueprint.
      */
    def exportFlatScripts(): Unit = {
        writeFlatScript(
          DisputeResolutionScript.compiledPlutusV3Program.program.flatEncoded,
          disputeResolutionFlatFilePath
        )
        writeFlatScript(
          RuleBasedTreasuryScript.compiledPlutusV3Program.program.flatEncoded,
          ruleBasedTreasuryFlatFilePath
        )
    }

    /** Main method for standalone execution.
      *
      * Run with: nix develop --command sbtn "runMain
      * hydrozoa.rulebased.ledger.l1.script.plutus.Export"
      */
    def main(args: Array[String]): Unit = {
        exportBlueprint()
        exportFlatScripts()
    }
}
