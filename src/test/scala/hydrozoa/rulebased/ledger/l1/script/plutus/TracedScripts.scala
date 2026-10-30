package hydrozoa.rulebased.ledger.l1.script.plutus

import hydrozoa.config.HydrozoaBlueprint
import scalus.cardano.ledger.rules.{Context, STS, State}
import scalus.cardano.ledger.{ScriptHash, Transaction, TransactionException}
import scalus.cardano.node.SubmitError
import scalus.testing.ImmutableEmulator
import scalus.uplc.DebugScript

/** Traced compiles of the rule-based validators, for tests that assert why a script failed.
  *
  * The validators are compiled without error traces ([[ScriptCompilerOptions]]), so a failing
  * script reports no reason. A ledger evaluator given [[debugScripts]] replays a failure that
  * logged nothing against the traced compile of the same validator, and the validator's message
  * lands in the failure's logs. The replay runs only when a script fails.
  */
object TracedScripts {

    /** Each validator's traced compile, keyed by the hash of the pinned script it stands in for:
      * transactions carry the blueprint's scripts ([[HydrozoaBlueprint]]), so a key computed from a
      * fresh compile would never match one once the compiler drifts from the pin.
      */
    lazy val debugScripts: Map[ScriptHash, DebugScript] =
        Map(
          HydrozoaBlueprint.disputeScriptHash ->
              DebugScript.fromCompiled(DisputeResolutionScript.compiledPlutusV3Program),
          HydrozoaBlueprint.treasuryScriptHash ->
              DebugScript.fromCompiled(RuleBasedTreasuryScript.compiledPlutusV3Program),
          HydrozoaBlueprint.regimeScriptHash ->
              DebugScript.fromCompiled(RuleBasedRegimeScript.compiledPlutusV3Program)
        )

    /** [[ImmutableEmulator.submit]], with [[debugScripts]] in the ledger context. Returns the state
      * the transaction leads to, not a new emulator, since callers only inspect rejections.
      */
    def submit(emulator: ImmutableEmulator, tx: Transaction): Either[SubmitError, State] = {
        val context = Context(
          env = emulator.env,
          slotConfig = emulator.slotConfig,
          evaluatorMode = emulator.evaluatorMode,
          debugScripts = debugScripts
        )
        STS.Mutator
            .transit[TransactionException](
              emulator.validators,
              emulator.mutators,
              context,
              emulator.state,
              tx
            )
            .left
            .map(SubmitError.fromException)
    }
}
