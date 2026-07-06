package hydrozoa.integration.fallbackhandoff

import cats.data.ReaderT
import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.head.initialization.{InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.TxTiming.Durations.*
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{
  InitParamsType,
  generateHeadConfig,
  generateHeadConfigBootstrap
}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.lib.cardano.scalus.QuantizedTime.{
  QuantizedInstant,
  quantize
}
import hydrozoa.lib.logging.{ContraTracer, Slf4jTracer}
import hydrozoa.multisig.backend.cardano.{CardanoBackend as L1Backend, FirewalledCardanoBackend, FirewalledCardanoBackendEvent, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{
  CardanoLiaisonEvent,
  RequestSequencer,
  SlowConsensusActorEvent,
  UserRequest,
  UserRequestBody,
  UserRequestHeader
}
import hydrozoa.multisig.ledger.block.BlockVersion.Major.given_Conversion_Major_Int
import hydrozoa.multisig.ledger.l1.tx.SettlementTx
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects}
import hydrozoa.multisig.{
  CoilMultisigRegimeManagerEventFormat,
  CommonChildEvent,
  HeadMultisigRegimeManagerEventFormat,
  RuleBasedOnlyChildEvent
}
import hydrozoa.rulebased.RuleBasedActorEvent
import org.scalacheck.{
  Gen,
  Prop,
  Properties
}
import scala.concurrent.duration.*
import scalus.uplc.builtin.ByteString
import test.{SeedPhrase, TestPeers, given}

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
    // Coil peer(s) run [[hydrozoa.multisig.CoilMultisigRegimeManager]] and, post-handoff, a
    // coil-mode [[hydrozoa.rulebased.RuleBasedActor]] via
    // [[hydrozoa.rulebased.RuleBasedRegimeManager]] (see
    // `CoilMultisigRegimeManager.onHandoffToRuleBased`). Flip to 1 to exercise step5's coil
    // ratchet assertion; the wiring (coil NodeConfig build, harness passthrough, observer)
    // is in place. Left at 0 today because the head major-2 window closes before the
    // scenario can observe `bothPeersConfirmedMajor2` when a coil peer is in the cluster —
    // a timing tune-up (`scenarioTimeout` or the fast-timing knobs) can unblock it as
    // follow-up.
    private val nCoilPeers: Int = 0
    // Real wall-clock budget. Head config's minSettlementDuration + our fast-timing knobs
    // determine major-block cadence; two majors + RRM handoff + accepted Vote submission fit
    // in ~60s.
    private val scenarioTimeout: FiniteDuration = 90.seconds
    private val cardanoNetwork: CardanoNetwork = CardanoNetwork.Preprod

    // ------------------------------------------------------------------
    // Test properties
    // ------------------------------------------------------------------

    // Direct (TestControl) property is not registered yet: cats-actors' `setReceiveTimeout` uses
    // the real wall clock (see EvacuationPropertyTest doc) so peers can't tick under TestControl
    // without an explicit tickAll driver. Skip until we have that story.
    val _ = property("ws: RRM votes at latest hard-confirmed major even when on-chain lags") =
        testProperty(TransportMode.WebSocket)

    private def testProperty(transportMode: TransportMode): Prop =
        import org.scalacheck.util.Pretty

        given (MultiNodeConfig => Pretty) = _ => Pretty(_ => "MultiNodeConfig (too long)")

        // Head + coil peer wallets — mirrors `stage4/Suite.scala:806-818`. `testPeers` covers the
        // head range [0, nHeadPeers); the extra wallets past that are used to spawn coil peers
        // (via `NodeConfig.mkCoilConfig`) and their yaci-genesis UTxOs stay available on the mock
        // backend as collateral for the coil RBA. `coilQuorum = 0` — the persistence-backed
        // `loadAction` returns `coilSignatures = Nil` (coil-side recovery is deferred), so any
        // non-zero quorum would trip the Plutus coil multisig check.
        val testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers)
        val coilWallets: List[hydrozoa.multisig.consensus.peer.PeerWallet] =
            if nCoilPeers == 0 then Nil
            else {
                val withCoils =
                    TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers + nCoilPeers)
                (0 until nCoilPeers).toList.map(i =>
                    withCoils.walletFor(HeadPeerNumber(nHeadPeers + i))
                )
            }
        val coilPeersConfig: hydrozoa.config.head.coil.CoilPeers =
            hydrozoa.config.head.coil.CoilPeers.indexed(
              coilWallets.map(w =>
                  hydrozoa.config.head.coil.CoilPeerData(
                    verificationKey = w.exportVerificationKey,
                    hubHeadPeerNumber = HeadPeerNumber(0),
                  )
              )
            )
        // Under WS (real wall-clock) mode we MUST anchor the initial block's creation-end-time to
        // `takeoffTime` — otherwise the default generator picks a random Jan-1-2026 + 100-day
        // offset that has already elapsed relative to `Instant.now()`, so the init tx validity
        // window closes before CL ever wakes up. Stage4 does the same trick.
        val takeoffTime: Option[java.time.Instant] =
            if transportMode.useTestControl then None
                // TODO: This should use IO.realtimeInstant
            else Some(java.time.Instant.now().plusSeconds(2))
        val generateHeadStartTime: test.GenWithTestPeers[BlockCreationEndTime] =
            ReaderT { tp =>
                takeoffTime match {
                    case Some(t) => Gen.const(BlockCreationEndTime(t.quantize(tp.slotConfig)))
                    case None    =>
                        val anchorTime = 1767225600L // Jan 1 2026, GMT
                        val range      = 86_400 * 100L
                        for offset <- Gen.choose(0L, range)
                        yield BlockCreationEndTime(
                          java.time.Instant.ofEpochSecond(anchorTime + offset).quantize(tp.slotConfig)
                        )
                }
            }
        // Fast timing knobs stolen from stage4/Suite.scala so blocks progress within our budget:
        // halve the CL polling period and shorten the two rate-limiter periods (see
        // docs/rate-limiter.md).
        // Static fast TxTiming so `Action.FallbackToRuleBased` (case 3 at CardanoLiaison.scala:940)
        // fires within our budget — settlement/fallback windows are seconds, not hours.
        val fastTxTiming: test.GenWithTestPeers[TxTiming] = ReaderT { network =>
            Gen.const(
              TxTiming(
                minSettlementDuration = MinSettlementDuration(2.seconds.quantize(network.slotConfig)),
                // Init tx window: initEndTime = bcet + minSettlementDuration +
                // inactivityMarginDuration = 5s. Actor bring-up + stack-0 hard-confirmation +
                // CL's first poll all have to fit inside that or `InitWindowElapsed` fires.
                // `inactivityMarginDuration` also gates the Minor→Major deadman: majors fire
                // every ~inactivityMarginDuration after the previous major's bcet.
                inactivityMarginDuration = InactivityMarginDuration(3.seconds.quantize(network.slotConfig)),
                silenceDuration = SilenceDuration(1.second.quantize(network.slotConfig)),
                depositSubmissionDuration = DepositSubmissionDuration(1.second.quantize(network.slotConfig)),
                depositMaturityDuration = DepositMaturityDuration(1.second.quantize(network.slotConfig)),
                depositAbsorptionDuration = DepositAbsorptionDuration(2.minutes.quantize(network.slotConfig)),
              )
            )
        }

        // Pin the head-bootstrap's genesis UTxOs to the SAME map the harness seeds into the mock
        // backend below (`preinitPeerUtxosL1`). The default BottomUp generator randomises them
        // independently, so the init tx tries to spend inputs the mock doesn't have → invalid tx
        // → InitWindowElapsed. Stage4 uses the same trick (Suite.scala:847-853).
        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)
        val genMultiNodeConfig =
            MultiNodeConfig
                .generateWith(testPeers)(
                  generateHeadConfig = generateHeadConfig(
                    genHeadConfigBootstrap = generateHeadConfigBootstrap(
                      generateHeadParams = generateHeadParameters(generateTxTiming = fastTxTiming)
                          .map(_.copy(coilQuorum = 0)),
                      generateInitializationParameters = InitParamsType.TopDown(
                        InitializationParametersGenTopDown.GenWithDeps(
                          generateGenesisUtxosL1 = ReaderT((_: TestPeers) =>
                              Gen.const(
                                testPeerToUtxos.map { case (k, v) => k.headPeerNumber -> v }
                              )
                          )
                        )
                      ),
                      coilPeers = coilPeersConfig,
                    ),
                    generateInitialBlock = bootstrap =>
                        generateInitialBlock(
                          genHeadConfigBootstrap =
                              ReaderT.pure[Gen, TestPeers, hydrozoa.config.head.HeadConfig.Bootstrap](
                                bootstrap
                              ),
                          generateBlockCreationEndTime = generateHeadStartTime,
                        ),
                  ),
                  // Default evacuationBotPollingPeriod is 1-10 MINUTES — RuleBasedActor's Tick
                  // would then never fire inside our budget after handoff. cats-actors pings the
                  // receive-timeout at 1 Hz anyway, so pin the config end low (100ms).
                  generateNodeOperationEvacuationConfig = w =>
                      Gen.const(
                        hydrozoa.config.node.operation.evacuation
                            .NodeOperationEvacuationConfig(
                              evacuationBotPollingPeriod = 100.millis,
                              ruleBasedWallet = w,
                            )
                      ),
                  generateNodeOperationMultisigConfig = hc =>
                      hydrozoa.config.node.operation.multisig
                          .generateNodeOperationMultisigConfig(
                            maxPollingPeriod = hc.maxCardanoLiaisonPollingPeriod / 2,
                            rateLimits = hydrozoa.config.node.operation.multisig.RateLimits(
                              softBlockMinPeriod = 500.millis,
                              hardStackMinPeriod = 250.millis,
                            ),
                          )
                )
                .label("MultiNodeConfig")

        val resource: org.scalacheck.PropertyM[IO, Resource[IO, Ctx]] =
            org.scalacheck.PropertyM
                .pick[IO, MultiNodeConfig](genMultiNodeConfig)
                .map(mnc =>
                    buildCtxResource(transportMode, mnc, testPeers, coilWallets, takeoffTime)
                )

        test.TestM.run[Ctx, Boolean](scenarioTestM, resource)

    // ------------------------------------------------------------------
    // Scenario body (in Ctx-fixed TestM)
    // ------------------------------------------------------------------

    private val ctxTestM = test.TestMFixedEnv[Ctx]()
    import ctxTestM.*

    private def scenarioTestM: test.TestM[Ctx, Boolean] =
        for
            _ <- step1a_submitBootstrapRequest
            _ <- step1b_startPeriodicRequestLoop
            _ <- step2_awaitBothPeersHardConfirmMajor2
            _ <- step3_awaitFallbackToRuleBasedHandoff
            _ <- step4_assertVoteAccepted
            _ <- if nCoilPeers > 0 then step5_assertCoilRatchetSubmitted else pure(())
        yield true

    // ------------------------------------------------------------------
    // Shared scenario context
    // ------------------------------------------------------------------

    /** State + handles threaded between steps. */
    private final case class Ctx(
        transportMode: TransportMode,
        multiNodeConfig: MultiNodeConfig,
        firewall: FirewallState,
        harness: MultiPeerHeadHarness.Harness[RequestSequencer.Handle, Unit],
        fallbackDispatched: Deferred[IO, Unit],
        bothPeersConfirmedMajor2: Deferred[IO, Unit],
        voteBuildAttempted: Deferred[IO, Unit],
        voteSubmittedOk: Deferred[IO, Unit],
        coilRatchetSubmitted: Deferred[IO, Unit],
    )

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

    /** Submit one minimal `TransactionRequest` to peer 0's `RequestSequencer`. The head has no
      * autonomous block-progress driver: `BlockWeaver.Leader.AwaitingConfirmation` waits for either
      * a user request or a soft-confirmation for the previous block, and the initial block never
      * gets soft-confirmed (it lives in config). Once block 1 lands via this single request, the
      * `forcedMajorBlockWakeupTime` deadman switch on each block header takes over and force-
      * completes empty major blocks until we reach major 2.
      */
    private def step1a_submitBootstrapRequest: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(submitOneUserRequest(ctx))
        yield ()

    /** Start a background fiber that keeps submitting `TransactionRequest`s at a slow cadence
      * (10s) for the rest of the scenario. Deadman-only progression yields all-Major partitions
      * (each stack = one Major, no SEC → RRM abstains). Feeding requests periodically fills the
      * `blockCanStayMinor` window after each Major with an extra Minor at the same major version,
      * so the LAST hard-confirmed stack's Major partition carries an SEC and `loadAction` returns
      * `Vote`. The fiber leaks harmlessly on scenario completion — the harness Resource cancels
      * the actor system, which terminates the RequestSequencer that would receive further sends.
      */
    private def step1b_startPeriodicRequestLoop: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _ <- lift(
              (IO.sleep(1.second) >> submitOneUserRequest(ctx)).foreverM.start.void
            )
        yield ()

    private def step2_awaitBothPeersHardConfirmMajor2: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(ctx.bothPeersConfirmedMajor2.get.timeout(scenarioTimeout))
        yield ()

    private def step3_awaitFallbackToRuleBasedHandoff: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(ctx.fallbackDispatched.get.timeout(scenarioTimeout))
        yield ()

    /** RBA's `loadAction` walks backward through hard-confirmed stacks and picks the SEC
      * matching the on-chain treasury's `versionMajor` (major-1 here), so the built Vote
      * passes the Plutus dispute-script check and submits successfully. Assert that the actor
      * attempted a Vote (proves handoff + persistence load), that the Vote submitted OK
      * (proves the version match worked), and that `sutErrors` does not contain
      * `"versionMajor field must match"` — presence would signal a regression where
      * `loadAction` picked the newest SEC instead of the version-matching one.
      */
    private def step4_assertVoteAccepted: test.TestM[Ctx, Unit] =
        for
            ctx  <- ask
            _    <- lift(ctx.voteBuildAttempted.get.timeout(scenarioTimeout))
            _    <- lift(ctx.voteSubmittedOk.get.timeout(scenarioTimeout))
            errs <- lift(ctx.harness.sutErrors.get)
            _ <- assertWith(
              !errs.exists(_.contains("versionMajor field must match")),
              "regression: sutErrors contains 'versionMajor field must match' — RBA's " +
                  "loadAction is voting with an SEC ahead of the on-chain treasury version",
            )
        yield ()

    /** Coil peer runs the same [[hydrozoa.rulebased.RuleBasedActor]] as head peers (spawned by
      * [[hydrozoa.multisig.CoilMultisigRegimeManager.onHandoffToRuleBased]]) but its
      * `Dispute.handle` routes into `handleCoil`: it picks an Open-phase ballot box and submits
      * a [[hydrozoa.rulebased.ledger.l1.tx.RatchetVoteTx]]. Assert one such submit lands within
      * the scenario budget.
      */
    private def step5_assertCoilRatchetSubmitted: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(ctx.coilRatchetSubmitted.get.timeout(scenarioTimeout))
        yield ()

    // ------------------------------------------------------------------
    // Ctx bring-up (step1_setup as a Resource[IO, Ctx])
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
            voteBuildAttempted <- Resource.eval(Deferred[IO, Unit])
            voteSubmittedOk    <- Resource.eval(Deferred[IO, Unit])
            coilRatchetSubmitted <- Resource.eval(Deferred[IO, Unit])

            preinitPeerUtxosL1 = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)
                .map { case (name, utxos) => name.headPeerNumber -> utxos }

            // Under TestControl the harness jumps virtual time to `startEpochMs` before any actor
            // exists (see MultiPeerHeadHarness.PreSystem.testControlPresleep). Anchor to the head's
            // configured initial block end-time so the model clock is coherent with the head config.
            startEpochMs = multiNodeConfig.headConfig.initialBlock.blockBrief.endTime
                .convert.instant.toEpochMilli

            // Coil NodeConfigs — mirrors `stage4/Suite.scala:900-916`. Reuses head 0's operational
            // sub-configs (evacuation polling period, cardano-liaison polling period, etc.) since
            // the coil path doesn't exercise their head-specific fields.
            head0Private = multiNodeConfig.nodePrivateConfigs(HeadPeerNumber(0))
            coilNodeConfigs = coilWallets.map { w =>
                hydrozoa.config.node.NodeConfig
                    .mkCoilConfig(
                      headConfig = multiNodeConfig.headConfig,
                      ownCoilWallet = w,
                      nodeOperationEvacuationConfig =
                          head0Private.nodeOperationEvacuationConfig.copy(ruleBasedWallet = w),
                      nodeOperationMultisigConfig = head0Private.nodeOperationMultisigConfig,
                      blockfrostApiKey = "not-a-real-key",
                      sugarRushUri = "ws://localhost:3001/ws",
                      adminUsername = "admin",
                      adminPassword = "welcome",
                      httpHost = "0.0.0.0",
                      httpPort = "8080",
                    )
                    .get
            }

            hooks = MultiPeerHeadHarness.Hooks[RequestSequencer.Handle, Unit](
              tracer = humanFormatTracer |+| observerTracer(
                bothPeersConfirmedMajor2,
                fallbackDispatched,
                voteBuildAttempted,
                voteSubmittedOk,
                coilRatchetSubmitted,
              ),
              peerHandle = (peerNum, conns) =>
                  IO.fromOption(conns.requestSequencer)(
                    new NoSuchElementException(
                      s"peer $peerNum has no RequestSequencer.Handle in its Connections"
                    )
                  ),
              coilHandle = (_, _) => IO.unit,
              wrapPeerBackend = wrapPeerBackend(firewall),
            )

            label = s"VoteVersionMismatch-${transportMode.toString.toLowerCase}"

            harness <- MultiPeerHeadHarness.resource[RequestSequencer.Handle, Unit](
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
          voteBuildAttempted = voteBuildAttempted,
          voteSubmittedOk = voteSubmittedOk,
          coilRatchetSubmitted = coilRatchetSubmitted,
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
        val userVk     =
            ctx.multiNodeConfig.nodeConfigs(peerNum).ownWallet.exportVerificationKey
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
            userRequest = UserRequest.TransactionRequest(header, body, userVk)
            sequencer <- IO.fromOption(ctx.harness.peers.get(peerNum).map(_.handle))(
              new NoSuchElementException(s"peer $peerNum missing in harness")
            )
            _ <- sequencer ?: userRequest
        yield ()

    private def newFirewallState: IO[FirewallState] =
        for
            dropped <- Ref[IO].of(
              Map.empty[HeadPeerNumber, List[FirewalledCardanoBackendEvent.DroppedOutboundTx]]
            )
            submitted <- Ref[IO].of(
              Map.empty[HeadPeerNumber, List[FirewalledCardanoBackendEvent.SubmittedTx]]
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

    /** Static drop predicate + per-peer event capture. */
    private def wrapPeerBackend(state: FirewallState)(
        peerNum: HeadPeerNumber,
        underlying: L1Backend[IO],
    ): L1Backend[IO] =
        val slf4jSink: ContraTracer[IO, FirewalledCardanoBackendEvent] =
            Slf4jTracer.sink.contramap {
                case FirewalledCardanoBackendEvent.DroppedOutboundTx(hash) =>
                    hydrozoa.lib.logging.LogEvent
                        .From(Map("peer" -> peerNum.toString), "FirewalledCardanoBackend")
                        .warn(s"firewall DROPPED tx $hash")
                case FirewalledCardanoBackendEvent.SubmittedTx(hash, result) =>
                    hydrozoa.lib.logging.LogEvent
                        .From(Map("peer" -> peerNum.toString), "FirewalledCardanoBackend")
                        .info(s"firewall passed tx $hash result=$result")
            }
        val perPeerTracer: ContraTracer[IO, FirewalledCardanoBackendEvent] =
            slf4jSink |+| ContraTracer[IO, FirewalledCardanoBackendEvent] {
                case e: FirewalledCardanoBackendEvent.DroppedOutboundTx =>
                    state.dropped.update(m => m.updated(peerNum, m.getOrElse(peerNum, Nil) :+ e))
                case e: FirewalledCardanoBackendEvent.SubmittedTx =>
                    state.submitted.update(m => m.updated(peerNum, m.getOrElse(peerNum, Nil) :+ e))
            }
        FirewalledCardanoBackend(
          underlying = underlying,
          shouldDrop = etx =>
              IO.pure(etx match {
                  case s: SettlementTx => s.majorVersionProduced.convert == 2
                  case _               => false
              }),
          firewallTracer = perPeerTracer,
        )

    /** Observer tracer wiring — see class-level Ctx doc for what each signal represents. */
    private def observerTracer(
        bothPeersConfirmedMajor2: Deferred[IO, Unit],
        fallbackDispatched: Deferred[IO, Unit],
        voteBuildAttempted: Deferred[IO, Unit],
        voteSubmittedOk: Deferred[IO, Unit],
        coilRatchetSubmitted: Deferred[IO, Unit],
    ): ContraTracer[IO, MultiPeerHeadHarness.Event] =
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
                          RuleBasedActorEvent.Tx.Building("VoteTx")
                        ) =>
                        voteBuildAttempted.complete(()).void

                    case RuleBasedOnlyChildEvent.RuleBasedActor(
                          RuleBasedActorEvent.Tx.SubmitSuccess(tx)
                        ) if tx.transactionFamily == "VoteTx" =>
                        voteSubmittedOk.complete(()).void

                    case _ => IO.unit
                }

            case MultiPeerHeadHarness.Event.Coil(_, evt) =>
                evt match {
                    case RuleBasedOnlyChildEvent.RuleBasedActor(
                          RuleBasedActorEvent.Tx.SubmitSuccess(tx)
                        ) if tx.transactionFamily == "RatchetVoteTx" =>
                        coilRatchetSubmitted.complete(()).void
                    case _ => IO.unit
                }
        }
