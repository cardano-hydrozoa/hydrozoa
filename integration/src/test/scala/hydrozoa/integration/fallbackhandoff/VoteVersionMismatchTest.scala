package hydrozoa.integration.fallbackhandoff

import cats.effect.{Deferred, IO, Ref, Resource}
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.{CardanoBackend as L1Backend, FirewalledCardanoBackend, FirewalledCardanoBackendEvent}
import hydrozoa.multisig.consensus.CardanoLiaisonEvent
import hydrozoa.multisig.consensus.SlowConsensusActorEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockVersion.Major.given_Conversion_Major_Int
import hydrozoa.multisig.ledger.l1.tx.FallbackTx
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects}
import hydrozoa.multisig.{CommonChildEvent, RuleBasedOnlyChildEvent}
import hydrozoa.rulebased.RuleBasedActorEvent
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.tx.VoteTx
import scala.concurrent.duration.*
import scalus.cardano.ledger.TransactionHash

// WIP scaffold: step1_setup + harnessInputs are still `???`, so several helpers below are
// currently unreferenced from the compiled tree. Suppress the unused-symbol errors under
// -Werror while the scaffold lands piece by piece; drop the annotation once step1 wires them
// together.
@scala.annotation.nowarn("msg=unused private member")
/** Demonstrates the vote-version mismatch bug in [[RuleBasedRegimeManager.loadAction]].
  *
  * Scenario:
  *   1. Set up a 2-peer head. Per-peer [[FirewalledCardanoBackend]] uses a static predicate:
  *      "drop any [[FallbackTx]] whose produced treasury datum has `versionMajor == 2`". Major-1's
  *      fallback still lands normally.
  *   2. Let both peers hard-confirm through major-2 off-chain.
  *   3. When CL dispatches `Action.FallbackToRuleBased` for major-2, the firewall drops the submit
  *      (returns `Right(())`), so `FallbackToRuleBasedDispatched` still fires and HMRM auto-spawns
  *      [[RuleBasedRegimeManager]].
  *   4. RRM's `loadAction` reads the latest hard-confirmed stack (major-2) and constructs a Vote
  *      whose SEC references major-2. The Vote submission reaches the mock (not a FallbackTx, not
  *      dropped); the mock rejects it because on-chain treasury still reflects major-1.
  *
  * Body is a straight for-yield of `step1..step4` — read it top-down.
  */
object VoteVersionMismatchTest:

    private val nHeadPeers: Int = 2
    private val scenarioTimeout: FiniteDuration = 60.seconds

    // ------------------------------------------------------------------
    // Runners
    // ------------------------------------------------------------------

    private def runDirect: IO[Unit] = runScenario(TransportMode.Direct)
    private def runWs: IO[Unit] = runScenario(TransportMode.WebSocket)

    /** The whole test, top to bottom. */
    private def runScenario(transportMode: TransportMode): IO[Unit] =
        for
            ctx <- step1_setup(transportMode)
            _   <- step2_awaitBothPeersHardConfirmMajor2(ctx)
            _   <- step3_awaitFallbackToRuleBasedHandoff(ctx)
            _   <- step4_assertVoteRejected(ctx)
        yield ()

    // ------------------------------------------------------------------
    // Shared scenario context
    // ------------------------------------------------------------------

    /** State + handles threaded between steps. */
    private final case class Ctx(
        transportMode: TransportMode,
        firewall: FirewallState,
        harness: MultiPeerHeadHarness.Harness[Unit, Unit],
        // Fires once any peer observes `CardanoLiaisonEvent.FallbackToRuleBasedDispatched`.
        fallbackDispatched: Deferred[IO, Unit],
        // Fires once both peers hard-confirm a stack whose last major produced version 2.
        bothPeersConfirmedMajor2: Deferred[IO, Unit],
        // Fires once RBA logs `RuleBasedActorEvent.Backend.ErrorSubmittingTx`.
        voteSubmitFailed: Deferred[IO, Unit],
        // Per-peer Vote tx ids as recorded from RBA's `Tx.Submitting(vote: VoteTx)` trace.
        voteTxIds: Ref[IO, Map[HeadPeerNumber, TransactionHash]],
    )

    /** Firewall observability state: per-peer submitted / dropped log accumulated from the
      * [[FirewalledCardanoBackendEvent]] stream.
      */
    private final case class FirewallState(
        dropped: Ref[
          IO,
          Map[HeadPeerNumber, List[FirewalledCardanoBackendEvent.DroppedOutboundTx]],
        ],
        submitted: Ref[
          IO,
          Map[HeadPeerNumber, List[FirewalledCardanoBackendEvent.SubmittedTx]],
        ],
    )

    // ------------------------------------------------------------------
    // Steps
    // ------------------------------------------------------------------

    /** Allocate firewall + signals, build [[MultiNodeConfig]], and stand up the harness with the
      * per-peer [[FirewalledCardanoBackend]] + an observer tracer wired to the signals.
      */
    private def step1_setup(transportMode: TransportMode): IO[Ctx] = ???

    /** Wait until both peers hard-confirm a stack whose last major produced version 2. Consensus
      * is off-chain so the fallback drop doesn't block this.
      */
    private def step2_awaitBothPeersHardConfirmMajor2(ctx: Ctx): IO[Unit] =
        ctx.bothPeersConfirmedMajor2.get.timeout(scenarioTimeout)

    /** Wait for `CardanoLiaisonEvent.FallbackToRuleBasedDispatched` from either peer. HMRM will
      * have auto-spawned RRM by the time this fires; nothing for the test to do here besides
      * synchronise.
      */
    private def step3_awaitFallbackToRuleBasedHandoff(ctx: Ctx): IO[Unit] =
        ctx.fallbackDispatched.get.timeout(scenarioTimeout)

    /** Belt-and-suspenders assertion:
      *   (a) confirm `voteSubmitFailed` fired (RBA's own `Backend.ErrorSubmittingTx` trace).
      *   (b) look up each peer's Vote tx id (recorded from RBA's `Tx.Submitting` trace) in the
      *       firewall's `SubmittedTx` log; assert the recorded result is `Left(InvalidTx)`.
      */
    private def step4_assertVoteRejected(ctx: Ctx): IO[Unit] =
        for
            _ <- ctx.voteSubmitFailed.get.timeout(scenarioTimeout)
            voteIds <- ctx.voteTxIds.get
            submitted <- ctx.firewall.submitted.get
            _ <- IO {
                assert(
                  voteIds.nonEmpty,
                  "expected at least one peer to have submitted a Vote tx",
                )
                voteIds.foreach { case (peer, voteId) =>
                    val peerSubs = submitted.getOrElse(peer, Nil)
                    val voteSub = peerSubs.find(_.txHash == voteId)
                    assert(
                      voteSub.isDefined,
                      s"peer $peer: no SubmittedTx event for Vote id $voteId",
                    )
                    voteSub.foreach { sub =>
                        assert(
                          sub.result.left.exists(_.isInstanceOf[L1Backend.Error.InvalidTx]),
                          s"peer $peer: expected Left(InvalidTx) on Vote submission, " +
                              s"got ${sub.result}",
                        )
                    }
                }
            }
        yield ()

    // ------------------------------------------------------------------
    // Wiring helpers (used by step1)
    // ------------------------------------------------------------------

    /** Allocate empty per-peer firewall log Refs. */
    private def newFirewallState: IO[FirewallState] =
        for
            dropped <- Ref[IO].of(
              Map.empty[HeadPeerNumber, List[FirewalledCardanoBackendEvent.DroppedOutboundTx]]
            )
            submitted <- Ref[IO].of(
              Map.empty[HeadPeerNumber, List[FirewalledCardanoBackendEvent.SubmittedTx]]
            )
        yield FirewallState(dropped, submitted)

    /** Per-peer backend wrapper installed via `MultiPeerHeadHarness.Hooks.wrapPeerBackend`.
      * Static predicate: drop iff the tx is a [[FallbackTx]] whose produced treasury datum is
      * `Unresolved(versionMajor == 2)`. Everything else (including RBA's Vote tx) passes through
      * to the mock. Each firewall event lands in `state.dropped` / `state.submitted` for
      * post-run inspection.
      */
    private def wrapPeerBackend(state: FirewallState)(
        peerNum: HeadPeerNumber,
        underlying: L1Backend[IO],
    ): L1Backend[IO] =
        val perPeerTracer: ContraTracer[IO, FirewalledCardanoBackendEvent] =
            ContraTracer[IO, FirewalledCardanoBackendEvent] {
                case e: FirewalledCardanoBackendEvent.DroppedOutboundTx =>
                    state.dropped.update(m => m.updated(peerNum, m.getOrElse(peerNum, Nil) :+ e))
                case e: FirewalledCardanoBackendEvent.SubmittedTx =>
                    state.submitted.update(m => m.updated(peerNum, m.getOrElse(peerNum, Nil) :+ e))
            }
        FirewalledCardanoBackend(
          underlying = underlying,
          shouldDrop = etx =>
              IO.pure(etx match {
                  case fb: FallbackTx =>
                      fb.treasuryProduced.treasuryOutput.datum match {
                          case RuleBasedTreasuryDatum.Unresolved(_, versionMajor, _) =>
                              versionMajor == BigInt(2)
                          case _ => false
                      }
                  case _ => false
              }),
          firewallTracer = perPeerTracer,
        )

    /** Attach to `MultiPeerHeadHarness.Hooks.tracer`:
      *   - Complete `bothPeersConfirmedMajor2` once both peers emit a hard-confirmation whose last
      *     major partition produced version 2.
      *   - Complete `fallbackDispatched` on the first `FallbackToRuleBasedDispatched`.
      *   - Record `voteTxIds(peer) = voteTx.tx.id` on RBA's `Tx.Submitting(vote: VoteTx)`.
      *   - Complete `voteSubmitFailed` on the first `RuleBasedActorEvent.Backend.ErrorSubmittingTx`.
      */
    private def observerTracer(
        bothPeersConfirmedMajor2: Deferred[IO, Unit],
        fallbackDispatched: Deferred[IO, Unit],
        voteTxIds: Ref[IO, Map[HeadPeerNumber, TransactionHash]],
        voteSubmitFailed: Deferred[IO, Unit],
    ): ContraTracer[IO, MultiPeerHeadHarness.Event] =
        // Accumulates the set of peers that have hard-confirmed a stack whose last major produced
        // version 2. When the set reaches nHeadPeers, we release the deferred.
        val peersAtMajor2: Ref[IO, Set[HeadPeerNumber]] = Ref.unsafe(Set.empty)

        def lastMajorVersion(effects: StackEffects.HardConfirmed): Option[Int] =
            effects match {
                case _: StackEffects.HardConfirmed.Initial => None
                case r: StackEffects.HardConfirmed.Regular =>
                    r.partitions.toList.reverseIterator.collectFirst {
                        case m: PartitionEffects.Major[?] =>
                            m.settlement.majorVersionProduced.convert
                        case f: PartitionEffects.Final =>
                            f.finalization.majorVersionProduced.convert
                    }
            }

        ContraTracer[IO, MultiPeerHeadHarness.Event] {
            case MultiPeerHeadHarness.Event.Head(peerNum, evt) =>
                evt match {
                    case CommonChildEvent.SlowConsensusActor(
                          SlowConsensusActorEvent.StackHardConfirmed(stack)
                        ) =>
                        if lastMajorVersion(stack.effects).contains(2) then
                            peersAtMajor2.updateAndGet(_ + peerNum).flatMap { s =>
                                if s.size >= nHeadPeers
                                then bothPeersConfirmedMajor2.complete(()).void
                                else IO.unit
                            }
                        else IO.unit

                    case CommonChildEvent.CardanoLiaison(
                          _: CardanoLiaisonEvent.FallbackToRuleBasedDispatched
                        ) =>
                        fallbackDispatched.complete(()).void

                    case RuleBasedOnlyChildEvent.RuleBasedActor(
                          RuleBasedActorEvent.Tx.Submitting(vote: VoteTx)
                        ) =>
                        voteTxIds.update(_.updated(peerNum, vote.tx.id))

                    case RuleBasedOnlyChildEvent.RuleBasedActor(
                          _: RuleBasedActorEvent.Backend.ErrorSubmittingTx
                        ) =>
                        voteSubmitFailed.complete(()).void

                    case _ => IO.unit
                }

            case MultiPeerHeadHarness.Event.Coil(_, _) => IO.unit
        }

    /** Build MultiNodeConfig + pre-init UTxOs + start epoch for the 2-peer scenario. */
    private def harnessInputs(
        transportMode: TransportMode
    ): Resource[IO, MultiPeerHeadHarness.Inputs] = ???
