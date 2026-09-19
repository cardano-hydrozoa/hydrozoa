package hydrozoa.rulebased.ledger.l1.script.plutus

import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.DisputeRedeemer
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedRegimeValidator.RegimeRedeemer
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.l1.state.RegimeState.RuleBasedRegimeDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteDatum
import java.io.File
import scalus.cardano.address.Network
import scalus.cardano.blueprint.Blueprint

object Export {

    /** Creates a CIP-57 compliant Blueprint describing the Hydrozoa rule-based validators:
      *   - DisputeResolutionScript: Manages voting and tallying during dispute resolution
      *   - RuleBasedTreasuryScript: Manages treasury state transitions and evacuations
      *   - RuleBasedRegimeScript: Guards the regime utxo, spendable only by the deinit tx
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
        val treasuryValidator = Blueprint.plutusV3[RuleBasedTreasuryDatum, TreasuryRedeemer](
          title = "Rule-Based Treasury Validator",
          description = "Manages the Hydrozoa treasury during rule-based regime. " +
              "Handles dispute resolution, UTXO evacuation with KZG membership proofs, and head deinitialization.",
          version = "1.0.0",
          license = Some("Apache-2.0"),
          compiled = RuleBasedTreasuryScript.compiledPlutusV3Program
        )

        // Create RuleBasedRegime validator blueprint
        val regimeValidator = Blueprint.plutusV3[RuleBasedRegimeDatum, RegimeRedeemer](
          title = "Rule-Based Regime Validator",
          description = "Guards the Hydrozoa rule-based regime utxo. " +
              "Allows spending only by a deinit transaction, which burns the regime utxo's HRWT " +
              "beacon together with the treasury's beacon token.",
          version = "1.0.0",
          license = Some("Apache-2.0"),
          compiled = RuleBasedRegimeScript.compiledPlutusV3Program
        )

        // Combine all validators into a single blueprint
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
            treasuryValidator.validators.head,
            regimeValidator.validators.head
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
        val regimeScriptHash =
            RuleBasedRegimeScript.address(Network.Mainnet).scriptHashOption.get
        println(s"- Dispute Resolution Script Hash: ${disputeScriptHash.toHex}")
        println(s"- Rule-Based Treasury Script Hash: ${treasuryScriptHash.toHex}")
        println(s"- Rule-Based Regime Script Hash: ${regimeScriptHash.toHex}")
    }

    /** Main method for standalone execution.
      *
      * Run with: nix develop --command sbt "runMain
      * hydrozoa.rulebased.ledger.l1.script.plutus.Export"
      */
    def main(args: Array[String]): Unit = {
        exportBlueprint()
    }
}
