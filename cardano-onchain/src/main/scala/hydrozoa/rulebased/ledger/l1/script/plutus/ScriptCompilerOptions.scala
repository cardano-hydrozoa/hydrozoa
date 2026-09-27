package hydrozoa.rulebased.ledger.l1.script.plutus

import scalus.compiler.Options

/** The Scalus compiler options every rule-based validator is compiled with.
  *
  * `releaseUntagged` optimises the UPLC and compiles out error messages and `log` calls, so a
  * failing script reports no reason. To see the reason, evaluate the same script context against a
  * traced compile: `DebugScript.fromCompiled(compiledPlutusV3Program)` recompiles it with error
  * traces, and the ledger evaluator replays a failure against it when given it in `debugScripts`.
  *
  * Any change here changes every script hash: regenerate the blueprint with `Export`.
  */
object ScriptCompilerOptions {
    val options: Options = Options.releaseUntagged
}
