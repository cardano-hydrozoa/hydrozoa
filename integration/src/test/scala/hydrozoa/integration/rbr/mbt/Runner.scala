package hydrozoa.integration.rbr.mbt

import org.scalacheck.{Test, YetAnotherProperties}
import scala.concurrent.duration.*

/** Registers the RBR fallback→evacuation MBT. One slow real-clock WS run per property, so cap at a
  * single successful evaluation (like the other multi-peer dispute suites).
  */
object RbrMbtProperties extends YetAnotherProperties("RBR MBT"):

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withWorkers(1).withMinSuccessfulTests(1)

    val _ = property("ws: autonomous evacuation matches the RBRHlNet terminal") =
        RbrMbtSuite(nHeadPeers = 3, nCoilPeers = 3, maxVersionMinor = 2, nCommands = 12).property()

/** Yaci-backed variant of [[RbrMbtProperties]]. Same shape, same shrink cap, but runs the SUT
  * against a real Testcontainers-managed Yaci devnet — one JVM-wide container reset + redeployed
  * per iteration. Requires Docker; excluded from the default test run (see build.sbt); run via
  * `just integration-yaci-docker` or explicitly with
  * `integration/testOnly hydrozoa.integration.rbr.mbt.RbrMbtPropertiesYaci`.
  */
object RbrMbtPropertiesYaci extends YetAnotherProperties("RBR MBT (Yaci)"):

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withWorkers(1).withMinSuccessfulTests(1)

    val _ = property("ws: autonomous evacuation matches the RBRHlNet terminal (Yaci)") =
        RbrMbtSuite(
          nHeadPeers = 3,
          nCoilPeers = 3,
          maxVersionMinor = 2,
          backendSpec = RbrMbtSuite.BackendSpec.Yaci(),
        ).property()

/** Public-testnet variant of [[RbrMbtProperties]]. Same shape, same single-iteration cap, but runs
  * the SUT against the real **Preview** testnet over Blockfrost: the 6 peers are funded from a
  * pre-funded master wallet and the reference scripts are deployed on-chain per run (see
  * [[hydrozoa.integration.preview.PublicSetup]]). Requires a valid Blockfrost key + a funded master
  * wallet; excluded from the default test run (see build.sbt). Run explicitly with
  * `integration/testOnly hydrozoa.integration.rbr.mbt.RbrMbtPropertiesPublic` (or
  * `just integration-rbr-preview`), after exporting `RBR_MBT_PREVIEW_MASTER_SIGNING_KEY`.
  */
object RbrMbtPropertiesPublic extends YetAnotherProperties("RBR MBT (Preview)"):

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withWorkers(1).withMinSuccessfulTests(1)

    /** Preview Blockfrost project id. Sourced from `$BLOCKFROST_API_KEY` (the project convention —
      * see `DeployScriptsAndG2Setup` / `Bootstrap`), falling back to the id
      * [[hydrozoa.integration.stage1]]'s public run hardcodes. NB: that fallback is currently a
      * dead token (403), so a live run needs `BLOCKFROST_API_KEY` exported (e.g. in
      * `.envrc.local`).
      */
    private val previewBlockfrostKey =
        sys.env.getOrElse("BLOCKFROST_API_KEY", "previewQQFamFAznFQgz0uRG9OntxgqJczreq9z")

    /** Ed25519 signing key (hex) of the pre-funded Preview master wallet the 6 peers are funded
      * from — a real wallet secret, so it's read from the environment rather than committed.
      * Generate one with the `keygen` CLI (`hydrozoa keygen` prints the signing key + its Testnet
      * address), fund that address from the Preview faucet with enough tPreview-ADA (≈ 6 × peer
      * funding + the script-deploy ladder + fees), and export the signing key as
      * `RBR_MBT_PREVIEW_MASTER_SIGNING_KEY`.
      */
    private val masterSigningKeyHex = sys.env.getOrElse("RBR_MBT_PREVIEW_MASTER_SIGNING_KEY", "")

    val _ = property("ws: autonomous evacuation matches the RBRHlNet terminal (Preview)") =
        RbrMbtSuite(
          nHeadPeers = 3,
          nCoilPeers = 3,
          maxVersionMinor = 2,
          backendSpec = RbrMbtSuite.BackendSpec.Public(previewBlockfrostKey, masterSigningKeyHex),
          // Real ~20s block cadence: give fallback + dispute + evacuation a much wider budget than
          // the fast Mock/Yaci backends, and wait several blocks for deposits to commit.
          scenarioTimeout = 30.minutes,
          depositCommitWindow = 5.minutes,
        ).property()
