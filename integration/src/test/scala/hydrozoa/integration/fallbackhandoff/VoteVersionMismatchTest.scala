package hydrozoa.integration.fallbackhandoff

import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.integration.harness.{MultiPeerDisputeProperties, MultiPeerHeadHarness}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.{CardanoBackend as L1Backend, FirewalledCardanoBackend, FirewalledCardanoBackendEvent, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, RequestSequencer, SlowConsensusActorEvent}
import hydrozoa.multisig.ledger.block.BlockVersion.Major.given_Conversion_Major_Int
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects}
import hydrozoa.multisig.{CommonChildEvent, RuleBasedOnlyChildEvent}
import hydrozoa.rulebased.RuleBasedActorEvent
import org.scalacheck.Prop
import scala.concurrent.duration.*
import test.{SeedPhrase, TestPeers}

/** Regression test for [[hydrozoa.rulebased.RuleBasedActor.loadAction]]: when the on-chain treasury
  * lags behind the latest hard-confirmed stack, the RBA must vote with the SEC whose `versionMajor`
  * matches the on-chain treasury (not the newest SEC in persistence), so the built Vote passes the
  * Plutus dispute-script's `versionMajor field must match` check.
  *
  * Scenario:
  *   1. 2-peer head. Per-peer [[FirewalledCardanoBackend]] drops any [[SettlementTx]] whose
  *      `majorVersionProduced == 2` so the on-chain treasury stays pinned at major-1 while the
  *      off-chain view advances.
  *   2. Both peers hard-confirm through major-2 off-chain.
  *   3. CL notices the on-chain treasury is stale and dispatches `Action.FallbackToRuleBased`; HMRM
  *      auto-spawns [[hydrozoa.rulebased.RuleBasedRegimeManager]], which spawns
  *      [[hydrozoa.rulebased.RuleBasedActor]].
  *   4. `loadAction(treasuryVersionMajor)` walks backward through hard-confirmed stacks and picks
  *      the last SEC matching `versionMajor = 1` (the on-chain treasury's). The Vote it builds and
  *      submits passes Plutus and lands on chain.
  */
object VoteVersionMismatchTest extends MultiPeerDisputeProperties("Vote Version Mismatch"):

    private val nHeadPeers: Int = 2
    private val nCoilPeers: Int = 1
    // Per-step budget. Must absorb the 60s takeoff offset plus the ~30s-per-major deadman
    // cadence (`forcedMajorBlockWakeupTime` = bcet + inactivityMargin) up to major 2.
    private val scenarioTimeout: FiniteDuration = 5.minutes

    // ------------------------------------------------------------------
    // Test properties
    // ------------------------------------------------------------------

    val _ = property("ws: RRM votes at latest hard-confirmed major even when on-chain lags") =
        testProperty(TransportMode.WebSocket)

    private def testProperty(transportMode: TransportMode): Prop =
        val testPeers = TestPeers(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers, nCoilPeers)
        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)

        val resource = MultiPeerHeadHarness.mkResource(
          transportMode = transportMode,
          testPeers = testPeers,
          testPeerToUtxos = testPeerToUtxos,
          takeoffOffset = 60.seconds,
          coilPeers = testPeers.coilPeersConfig(hub = HeadPeerNumber(0)),
        ) { (takeoffTime, mnc) =>
            buildCtxResource(transportMode, mnc, testPeers, takeoffTime)
        }

        test.TestM.run[Ctx, Boolean](scenarioTestM, resource)

    // ------------------------------------------------------------------
    // Scenario body (in Ctx-fixed TestM)
    // ------------------------------------------------------------------

    private val ctxTestM = test.TestMFixedEnv[Ctx]()
    import ctxTestM.*

    private def scenarioTestM: test.TestM[Ctx, Boolean] =
        for
            ctx <- ask

            // 1. Background fiber submitting one minimal `TransactionRequest` to peer 0's
            // `RequestSequencer` every second. Once block 1 lands via the first request, the
            // `forcedMajorBlockWakeupTime` deadman on each header takes over and force-completes
            // empty major blocks until we reach major 2; the trailing requests keep minor blocks
            // flowing.
            _ <- lift(
              (MultiPeerHeadHarness.submitKickRequest(ctx.harness)
                  >> IO.sleep(1.second)).foreverM.start.void
            )

            // 2. Both head peers hard-confirm through major-2 off-chain.
            _ <- lift(ctx.bothPeersConfirmedMajor2.get.timeout(scenarioTimeout))

            // 3. CL notices the on-chain treasury is stale and dispatches
            // `Action.FallbackToRuleBased`; HMRM auto-spawns RRM/RBA.
            _ <- lift(ctx.fallbackDispatched.get.timeout(scenarioTimeout))

            // 4. Every head peer's RBA finds the SEC matching the on-chain treasury's
            // versionMajor and submits a Vote that passes Plutus.
            _ <- lift(ctx.allHeadsBuildingVote.get.timeout(scenarioTimeout))
            _ <- lift(ctx.allHeadsVoteSubmittedOk.get.timeout(scenarioTimeout))
            errs <- lift(ctx.harness.sutErrors.get)
            _ <- assertWith(
              !errs.exists(_.contains("versionMajor field must match")),
              "regression: sutErrors contains 'versionMajor field must match' — RBA's " +
                  "loadAction is voting with an SEC ahead of the on-chain treasury version",
            )

            // 5. Every coil peer runs `handleCoil` (proves the CL handoff routed each coil into
            // the rule-based regime). Skipped when `nCoilPeers == 0`.
            _ <-
                if nCoilPeers > 0
                then lift(ctx.allCoilsHandledDispute.get.timeout(scenarioTimeout))
                else pure(())
        yield true

    // ------------------------------------------------------------------
    // Shared scenario context
    // ------------------------------------------------------------------

    /** State + handles threaded between steps. */
    private final case class Ctx(
        firewall: FirewallState,
        harness: MultiPeerHeadHarness.Harness[Option[RequestSequencer.Handle]],
        fallbackDispatched: Deferred[IO, Unit],
        bothPeersConfirmedMajor2: Deferred[IO, Unit],
        allHeadsBuildingVote: Deferred[IO, Unit],
        allHeadsVoteSubmittedOk: Deferred[IO, Unit],
        allCoilsHandledDispute: Deferred[IO, Unit],
    )

    private final case class FirewallState(
        dropped: Ref[
          IO,
          Map[PeerId, List[FirewalledCardanoBackendEvent.DroppedOutboundTx]],
        ],
        submitted: Ref[
          IO,
          Map[PeerId, List[FirewalledCardanoBackendEvent.SubmittedTx]],
        ],
    )

    // ------------------------------------------------------------------
    // Ctx bring-up (Resource[IO, Ctx])
    // ------------------------------------------------------------------

    /** Allocate observability state, compute pre-init UTxOs from `testPeers`, stand up the
      * [[MultiPeerHeadHarness]] with per-peer [[FirewalledCardanoBackend]] and the observer tracer.
      */
    private def buildCtxResource(
        transportMode: TransportMode,
        multiNodeConfig: MultiNodeConfig,
        testPeers: TestPeers,
        takeoffTime: Option[java.time.Instant],
    ): Resource[IO, Ctx] =
        for
            firewall <- Resource.eval(newFirewallState)
            fallbackDispatched <- Resource.eval(Deferred[IO, Unit])
            bothPeersConfirmedMajor2 <- Resource.eval(Deferred[IO, Unit])
            allHeadsBuildingVote <- Resource.eval(Deferred[IO, Unit])
            allHeadsVoteSubmittedOk <- Resource.eval(Deferred[IO, Unit])
            allCoilsHandledDispute <- Resource.eval(Deferred[IO, Unit])

            observer <- Resource.eval(
              observerTracer(
                bothPeersConfirmedMajor2,
                fallbackDispatched,
                allHeadsBuildingVote,
                allHeadsVoteSubmittedOk,
                allCoilsHandledDispute,
              )
            )

            harness <- MultiPeerHeadHarness.disputeHarnessResource(
              label = s"VoteVersionMismatch-${transportMode.toString.toLowerCase}",
              transportMode = transportMode,
              multiNodeConfig = multiNodeConfig,
              testPeers = testPeers,
              takeoffTime = takeoffTime,
              tracer = MultiPeerHeadHarness.humanFormatTracer(nHeadPeers) |+| observer,
              // Firewall every node — coil CL would otherwise submit the head-dropped v2
              // settlement out-of-band, suppressing the fallback path.
              wrapBackend = (peerId, backend) => wrapNodeBackend(firewall)(peerId, backend),
            )
        yield Ctx(
          firewall = firewall,
          harness = harness,
          fallbackDispatched = fallbackDispatched,
          bothPeersConfirmedMajor2 = bothPeersConfirmedMajor2,
          allHeadsBuildingVote = allHeadsBuildingVote,
          allHeadsVoteSubmittedOk = allHeadsVoteSubmittedOk,
          allCoilsHandledDispute = allCoilsHandledDispute,
        )

    // ------------------------------------------------------------------
    // Wiring helpers
    // ------------------------------------------------------------------

    private def newFirewallState: IO[FirewallState] =
        for
            dropped <- Ref[IO].of(
              Map.empty[PeerId, List[FirewalledCardanoBackendEvent.DroppedOutboundTx]]
            )
            submitted <- Ref[IO].of(
              Map.empty[PeerId, List[FirewalledCardanoBackendEvent.SubmittedTx]]
            )
        yield FirewallState(dropped, submitted)

    /** Shared drop rule + slf4j sink, plus per-node event capture into [[FirewallState]]. */
    private def wrapNodeBackend(state: FirewallState)(
        peerId: PeerId,
        underlying: L1Backend[IO],
    ): L1Backend[IO] =
        val capture: ContraTracer[IO, FirewalledCardanoBackendEvent] =
            ContraTracer[IO, FirewalledCardanoBackendEvent] {
                case e: FirewalledCardanoBackendEvent.DroppedOutboundTx =>
                    state.dropped.update(m => m.updated(peerId, m.getOrElse(peerId, Nil) :+ e))
                case e: FirewalledCardanoBackendEvent.SubmittedTx =>
                    state.submitted.update(m => m.updated(peerId, m.getOrElse(peerId, Nil) :+ e))
            }
        FirewalledCardanoBackend(
          underlying = underlying,
          shouldDrop = MultiPeerHeadHarness.DropRule.settlementProducingMajor(2).toGate,
          firewallTracer = MultiPeerHeadHarness.firewallSlf4jSink(peerId) |+| capture,
        )

    /** Observer tracer wiring — vote/build signals fire when **every** head peer hits the milestone
      * (not just the first), so a regression in the CL handoff would surface.
      */
    private def observerTracer(
        bothPeersConfirmedMajor2: Deferred[IO, Unit],
        fallbackDispatched: Deferred[IO, Unit],
        allHeadsBuildingVote: Deferred[IO, Unit],
        allHeadsVoteSubmittedOk: Deferred[IO, Unit],
        allCoilsHandledDispute: Deferred[IO, Unit],
    ): IO[ContraTracer[IO, MultiPeerHeadHarness.Event]] =
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

        for
            peersAtMajor2 <- Ref[IO].of(Set.empty[HeadPeerNumber])
            headsBuildingVote <- Ref[IO].of(Set.empty[HeadPeerNumber])
            headsVoteSubmittedOk <- Ref[IO].of(Set.empty[HeadPeerNumber])
            coilsHandledDispute <- Ref[IO].of(Set.empty[Int])
        yield ContraTracer[IO, MultiPeerHeadHarness.Event] {
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
                          RuleBasedActorEvent.Tx.Building("VoteTx")
                        ) =>
                        headsBuildingVote.updateAndGet(_ + peerNum).flatMap { s =>
                            if s.size >= nHeadPeers
                            then allHeadsBuildingVote.complete(()).void
                            else IO.unit
                        }

                    case RuleBasedOnlyChildEvent.RuleBasedActor(
                          RuleBasedActorEvent.Tx.SubmitSuccess(tx)
                        ) if tx.transactionFamily == "VoteTx" =>
                        headsVoteSubmittedOk.updateAndGet(_ + peerNum).flatMap { s =>
                            if s.size >= nHeadPeers
                            then allHeadsVoteSubmittedOk.complete(()).void
                            else IO.unit
                        }

                    case _ => IO.unit
                }

            case MultiPeerHeadHarness.Event.Coil(coilNum, evt) =>
                evt match {
                    // Any of the three coil-side terminal events proves `handleCoil` ran.
                    case RuleBasedOnlyChildEvent.RuleBasedActor(
                          _: RuleBasedActorEvent.Dispute.Coil.ParsingRatchet.type |
                          _: RuleBasedActorEvent.Dispute.Coil.AlreadyAtTarget.type |
                          _: RuleBasedActorEvent.Dispute.Coil.NoRatchetTarget.type
                        ) =>
                        coilsHandledDispute.updateAndGet(_ + coilNum.convert).flatMap { s =>
                            if s.size >= nCoilPeers
                            then allCoilsHandledDispute.complete(()).void
                            else IO.unit
                        }
                    case _ => IO.unit
                }
        }
