package hydrozoa.integration.fallbackhandoff

import cats.data.ReaderT
import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.lib.logging.{ContraTracer, Slf4jTracer}
import hydrozoa.multisig.backend.cardano.{CardanoBackend as L1Backend, FirewalledCardanoBackend, FirewalledCardanoBackendEvent, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, RequestSequencer, SlowConsensusActorEvent, UserRequest, UserRequestBody, UserRequestHeader}
import hydrozoa.multisig.ledger.block.BlockVersion.Major.given_Conversion_Major_Int
import hydrozoa.multisig.ledger.l1.tx.SettlementTx
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects}
import hydrozoa.multisig.{CoilMultisigRegimeManagerEventFormat, CommonChildEvent, HeadMultisigRegimeManagerEventFormat, RuleBasedOnlyChildEvent}
import hydrozoa.rulebased.RuleBasedActorEvent
import org.scalacheck.{Gen, Prop, Properties}
import scala.concurrent.duration.*
import scalus.uplc.builtin.ByteString
import test.{SeedPhrase, TestPeers}

/** Regression test for [[hydrozoa.rulebased.RuleBasedActor.loadAction]]: when the on-chain
  * treasury lags behind the latest hard-confirmed stack, the RBA must vote with the SEC whose
  * `versionMajor` matches the on-chain treasury (not the newest SEC in persistence), so the
  * built Vote passes the Plutus dispute-script's `versionMajor field must match` check.
  *
  * Scenario:
  *   1. 2-peer head. Per-peer [[FirewalledCardanoBackend]] drops any [[SettlementTx]] whose
  *      `majorVersionProduced == 2` so the on-chain treasury stays pinned at major-1 while the
  *      off-chain view advances.
  *   2. Both peers hard-confirm through major-2 off-chain.
  *   3. CL notices the on-chain treasury is stale and dispatches
  *      `Action.FallbackToRuleBased`; HMRM auto-spawns
  *      [[hydrozoa.rulebased.RuleBasedRegimeManager]], which spawns
  *      [[hydrozoa.rulebased.RuleBasedActor]].
  *   4. `loadAction(treasuryVersionMajor)` walks backward through hard-confirmed stacks and
  *      picks the last SEC matching `versionMajor = 1` (the on-chain treasury's). The Vote it
  *      builds and submits passes Plutus and lands on chain.
  */
object VoteVersionMismatchTest extends Properties("Vote Version Mismatch"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = p.withMinSuccessfulTests(1)

    private val nHeadPeers: Int = 2
    private val nCoilPeers: Int = 1
    private val scenarioTimeout: FiniteDuration = 90.seconds
    private val cardanoNetwork: CardanoNetwork = CardanoNetwork.Preprod

    // ------------------------------------------------------------------
    // Test properties
    // ------------------------------------------------------------------
    
    val _ = property("ws: RRM votes at latest hard-confirmed major even when on-chain lags") =
        testProperty(TransportMode.WebSocket)

    private def testProperty(transportMode: TransportMode): Prop =
        val testPeers = TestPeers(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers, nCoilPeers)
        val coilWallets = testPeers.coilWallets
        val coilPeersConfig = testPeers.coilPeersConfig(hub = HeadPeerNumber(0))
        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)

        // Fast voting deadline so the deadline-gated tally path in RuleBasedActor unblocks well
        // inside `scenarioTimeout`. The default generator picks 1h..5d; the coil peer's RBA
        // would sit on `TallyBlockedByAwaitingVote` for that entire window and time the scenario
        // out. Same pattern as `EvacuationPropertyTest.fastDisputeResolutionConfig`.
        val fastDisputeResolutionConfig: test.GenWithTestPeers[DisputeResolutionConfig] =
            ReaderT { network =>
                Gen.const(
                  DisputeResolutionConfig(
                    votingDuration = QuantizedFiniteDuration(
                      slotConfig = network.slotConfig,
                      finiteDuration = 5.seconds,
                    )
                  )
                )
            }

        val resource = MultiPeerHeadHarness.mkResource(
          transportMode = transportMode,
          testPeers = testPeers,
          testPeerToUtxos = testPeerToUtxos,
          takeoffOffset = 10.seconds,
          disputeResolutionConfig = fastDisputeResolutionConfig,
          coilPeers = coilPeersConfig,
        ) { (takeoffTime, mnc) =>
            buildCtxResource(transportMode, mnc, testPeers, coilWallets, takeoffTime)
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

            // 1a. Submit one minimal `TransactionRequest` to peer 0's `RequestSequencer`. Once block 1
            // lands via this request, the `forcedMajorBlockWakeupTime` deadman on each header
            // takes over and force-completes empty major blocks until we reach major 2.
            _ <- lift(submitOneUserRequest(ctx))

            // 1b. Background fiber submitting requests at a slow cadence for minor block production.
            _ <- lift((IO.sleep(1.second) >> submitOneUserRequest(ctx)).foreverM.start.void)

            // 2. Both head peers hard-confirm through major-2 off-chain.
            _ <- lift(ctx.bothPeersConfirmedMajor2.get.timeout(scenarioTimeout))

            // 3. CL notices the on-chain treasury is stale and dispatches
            // `Action.FallbackToRuleBased`; HMRM auto-spawns RRM/RBA.
            _ <- lift(ctx.fallbackDispatched.get.timeout(scenarioTimeout))

            // 4. Every head peer's RBA finds the SEC matching the on-chain treasury's
            // versionMajor and submits a Vote that passes Plutus.
            _    <- lift(ctx.allHeadsBuildingVote.get.timeout(scenarioTimeout))
            _    <- lift(ctx.allHeadsVoteSubmittedOk.get.timeout(scenarioTimeout))
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
        transportMode: TransportMode,
        multiNodeConfig: MultiNodeConfig,
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
        coilWallets: List[hydrozoa.multisig.consensus.peer.PeerWallet],
        takeoffTime: Option[java.time.Instant],
    ): Resource[IO, Ctx] =
        for
            firewall <- Resource.eval(newFirewallState)
            fallbackDispatched <- Resource.eval(Deferred[IO, Unit])
            bothPeersConfirmedMajor2 <- Resource.eval(Deferred[IO, Unit])
            allHeadsBuildingVote <- Resource.eval(Deferred[IO, Unit])
            allHeadsVoteSubmittedOk    <- Resource.eval(Deferred[IO, Unit])
            allCoilsHandledDispute <- Resource.eval(Deferred[IO, Unit])

            preinitPeerUtxosL1 = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)
                .map { case (name, utxos) => name.headPeerNumber -> utxos }

            // Under TestControl the harness jumps virtual time to `startEpochMs` before any actor
            // exists (see MultiPeerHeadHarness.PreSystem.testControlPresleep). Anchor to the head's
            // configured initial block end-time so the model clock is coherent with the head config.
            startEpochMs = multiNodeConfig.headConfig.initialBlock.blockBrief.endTime
                .convert.instant.toEpochMilli

            coilNodeConfigs = multiNodeConfig.mkCoilNodeConfigs(coilWallets)

            observer <- Resource.eval(
              observerTracer(
                bothPeersConfirmedMajor2,
                fallbackDispatched,
                allHeadsBuildingVote,
                allHeadsVoteSubmittedOk,
                allCoilsHandledDispute,
              )
            )

            hooks = MultiPeerHeadHarness.Hooks[Option[RequestSequencer.Handle]](
              tracer = humanFormatTracer |+| observer,
              handle = {
                  case (PeerId.Head(peerNum), conns) =>
                      IO.fromOption(conns.requestSequencer)(
                        new NoSuchElementException(
                          s"peer $peerNum has no RequestSequencer.Handle in its Connections"
                        )
                      ).map(Some(_))
                  case (_: PeerId.Coil, _) => IO.pure(None)
              },
              // Firewall every node — coil CL would otherwise submit the head-dropped v2
              // settlement out-of-band, suppressing the fallback path.
              wrapBackend = (peerId, backend) => wrapNodeBackend(firewall)(peerId, backend),
            )

            label = s"VoteVersionMismatch-${transportMode.toString.toLowerCase}"

            harness <- MultiPeerHeadHarness.resource[Option[RequestSequencer.Handle]](
              MultiPeerHeadHarness.Inputs(
                config = MultiPeerHeadHarness.Config(
                  label = label,
                  backendMode = MultiPeerHeadHarness.StorageBackend.Mode.InMemory,
                  transportMode = transportMode,
                ),
                multiNodeConfig = multiNodeConfig,
                coilNodeConfigs = coilNodeConfigs,
                preinitPeerUtxosL1 = preinitPeerUtxosL1,
                takeoffTime = takeoffTime,
                startEpochMs = startEpochMs,
              ),
              hooks,
            )
        yield Ctx(
          transportMode = transportMode,
          multiNodeConfig = multiNodeConfig,
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

    /** Submit one minimal `UserRequest.TransactionRequest` to peer 0's `RequestSequencer` to kick
      * `BlockWeaver` past block 1's `Leader.AwaitingConfirmation` state so the deadman switch on
      * subsequent block headers can start force-producing major blocks. The l2 payload is
      * intentionally empty — `JointLedger` will mark the request `Invalid` but block 1 still
      * completes, which is all we need.
      */
    private def submitOneUserRequest(ctx: Ctx): IO[Unit] =
        val peerNum    = HeadPeerNumber(0)
        val slotConfig = ctx.multiNodeConfig.headConfig.cardanoNetwork.slotConfig
        val body: UserRequestBody.TransactionRequestBody =
            UserRequestBody.TransactionRequestBody(
              l2Payload = ByteString.fromArray(Array.empty[Byte])
            )
        for
            now    <- IO.realTimeInstant
            header = UserRequestHeader(
              headId = ctx.multiNodeConfig.headConfig.headId,
              validityStart = RequestValidityStartTime(
                QuantizedInstant.ofEpochSeconds(slotConfig, now.getEpochSecond - 5L)
              ),
              validityEnd = RequestValidityEndTime(
                QuantizedInstant.ofEpochSeconds(slotConfig, now.getEpochSecond + 300L)
              ),
              bodyHash = body.hash,
            )
            userRequest = UserRequest.TransactionRequest(header, body)
            sequencer <- IO.fromOption(ctx.harness.peers.get(peerNum).flatMap(_.handle))(
              new NoSuchElementException(s"peer $peerNum missing in harness")
            )
            _ <- sequencer ?: userRequest
        yield ()

    private def newFirewallState: IO[FirewallState] =
        for
            dropped <- Ref[IO].of(
              Map.empty[PeerId, List[FirewalledCardanoBackendEvent.DroppedOutboundTx]]
            )
            submitted <- Ref[IO].of(
              Map.empty[PeerId, List[FirewalledCardanoBackendEvent.SubmittedTx]]
            )
        yield FirewallState(dropped, submitted)

    /** Route every harness event through the existing per-cell human formatters into slf4j so
      * scenario runs are visible in the console/log without touching each MRM's internal tracer.
      */
    private def humanFormatTracer: ContraTracer[IO, MultiPeerHeadHarness.Event] =
        ContraTracer[IO, MultiPeerHeadHarness.Event] {
            case MultiPeerHeadHarness.Event.Head(peerNum, evt) =>
                Slf4jTracer.sink.traceWith(
                  HeadMultisigRegimeManagerEventFormat.humanFormat(peerNum)(evt)
                )
            case MultiPeerHeadHarness.Event.Coil(coilNum, evt) =>
                val syntheticLabel = HeadPeerNumber(nHeadPeers + coilNum.convert)
                Slf4jTracer.sink.traceWith(
                  CoilMultisigRegimeManagerEventFormat.humanFormat(syntheticLabel, coilNum)(evt)
                )
        }

    /** Static drop predicate + per-node event capture. */
    private def wrapNodeBackend(state: FirewallState)(
        peerId: PeerId,
        underlying: L1Backend[IO],
    ): L1Backend[IO] =
        val slf4jSink: ContraTracer[IO, FirewalledCardanoBackendEvent] =
            Slf4jTracer.sink.contramap {
                case FirewalledCardanoBackendEvent.DroppedOutboundTx(hash) =>
                    hydrozoa.lib.logging.LogEvent
                        .From(Map("peer" -> peerId.toString), "FirewalledCardanoBackend")
                        .warn(s"firewall DROPPED tx $hash")
                case FirewalledCardanoBackendEvent.SubmittedTx(hash, result) =>
                    hydrozoa.lib.logging.LogEvent
                        .From(Map("peer" -> peerId.toString), "FirewalledCardanoBackend")
                        .info(s"firewall passed tx $hash result=$result")
            }
        val perNodeTracer: ContraTracer[IO, FirewalledCardanoBackendEvent] =
            slf4jSink |+| ContraTracer[IO, FirewalledCardanoBackendEvent] {
                case e: FirewalledCardanoBackendEvent.DroppedOutboundTx =>
                    state.dropped.update(m => m.updated(peerId, m.getOrElse(peerId, Nil) :+ e))
                case e: FirewalledCardanoBackendEvent.SubmittedTx =>
                    state.submitted.update(m => m.updated(peerId, m.getOrElse(peerId, Nil) :+ e))
            }
        FirewalledCardanoBackend(
          underlying = underlying,
          shouldDrop = etx =>
              IO.pure(etx match {
                  case s: SettlementTx => s.majorVersionProduced.convert == 2
                  case _               => false
              }),
          firewallTracer = perNodeTracer,
        )

    /** Observer tracer wiring — vote/build signals fire when **every** head peer hits the
      * milestone (not just the first), so a regression in the CL handoff would surface.
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
            peersAtMajor2       <- Ref[IO].of(Set.empty[HeadPeerNumber])
            headsBuildingVote   <- Ref[IO].of(Set.empty[HeadPeerNumber])
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
