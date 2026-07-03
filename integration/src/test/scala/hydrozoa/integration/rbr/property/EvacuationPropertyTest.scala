package hydrozoa.integration.rbr.property

import hydrozoa.*
import hydrozoa.config.node.operation.evacuation.{NodeOperationEvacuationConfig, NodeOperationEvacuationConfigGen}
import hydrozoa.multisig.consensus.peer.PeerWallet
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, Properties}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/*
CURRENT STATUS:
- The test only tests "happy path" behavior, where every peer votes for the same commitment
- We start with a mocked fallback tx
- We go through dispute + resolution + evacuation successfully
- Full classification of the utxo state matches
- Next steps (in order):
  - Instead of starting the actors individually, start them with the rule-based regime manager
  - Add in a cardano backend proxy to drop transactions from certain peers and regain some determinism ("rig the races")
  - Vote for different commitments
  - start with an actual fallback
  - add deinit
  - Domain-based logging (rather than stringly typed)
  - Model based testing of intermediary states according to full classification
 */
object EvacuationPropertyTest extends Properties("RBR Evacuation Property"):

    given ppIDU: (InitialDisputeUtxos => Pretty) = _ =>
        Pretty(_ => "InitialDisputeUtxos (too long to print)")

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters =
        p.withMinSuccessfulTests(3)

    // These might need to be tuned. Basically we don't want to end up in a spin loop.
    //  TODO: How can we detect this in the actors themselves?
    val votingDuration: FiniteDuration = 5.seconds
    val evacuationDuration: FiniteDuration = 10.minute
    // After the terminal "no more evacuations" signal fires, keep the actors running for this
    // buffer to catch any post-completion crashes (e.g. building an EvacuationTx with an empty
    // map; treating any such error as Recoverable is what keeps the actors alive for rollbacks).
    val postCompletionBuffer: FiniteDuration = 30.seconds
    val actorRunDuration: FiniteDuration =
        votingDuration + evacuationDuration + postCompletionBuffer

    // 100ms polling period so actors poll on every ~1s cats-actors ping loop tick
    val fastEvacConfig: NodeOperationEvacuationConfigGen =
        (wallet: PeerWallet) => Gen.const(NodeOperationEvacuationConfig(100.millis, wallet))


    // TODO(rbr-loader-move): both properties are temporarily disabled while the RBA's loaders were
    // moved into the actor and now read from `Persistence`. The vote / abstain scenarios need to
    // seed HardConfirmation(N) + UnsignedStack(N) + EvacuationMap(...) fixtures with a real
    // `FallbackTx` (heavy) instead of injecting `EvacuationInputs` directly. Reintroduce once the
    // stage4-style handoff harness (see `VoteVersionMismatchTest`) is generalised or a fixture
    // builder for `StackEffects.HardConfirmed.Regular` with a synthetic FallbackTx lands.

    /** Shared happy-path scenario: synthesize the post-fallback UTxO set, spawn the
      * [[DisputeActor]] + [[EvacuationActor]] pair per peer (with the [[DisputeAction]] under test),
      * and assert the terminal UTxO classification.
      *
      * Both Vote and Abstain produce the same terminal state — the default vote utxo already
      * commits to `evacMap.kzgCommitment`, so peers either join the default's vote or step aside
      * and let it carry the tally; in either case the resolution evacuates against `evacMap`.
      *
      * monadicIO (real time): `setReceiveTimeout` in cats-actors uses
      * `System.currentTimeMillis()`, which is NOT controlled by TestControl. Real wall-clock time
      * is required for actors to poll. `fastEvacConfig` overrides the default 1–10 min polling
      * period so actors poll every ~1s.
      */
    // NOTE: the `scenario` / `actorsFor` helpers that previously drove the two properties above
    // have been removed. They constructed a `RuleBasedActor` with mocked `loadAction` /
    // `loadEvacuationInputs` lambdas — no longer part of RBA's ctor signature. The re-work will
    // seed a `Persistence` with a real `StackEffects.HardConfirmed.Regular` (fallback tx +
    // multi-signed SEC + refunds), or bring the whole scenario under the stage4-style
    // MultiPeerHeadHarness like `VoteVersionMismatchTest`. Git history preserves the original.
