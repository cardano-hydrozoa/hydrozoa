package hydrozoa.integration.fallbackhandoff

import cats.data.ReaderT
import cats.effect.*
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.generateHeadConfig
import hydrozoa.config.head.InitParamsType
import hydrozoa.config.head.initialization.{InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.TxTiming.Durations.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{generateHeadConfigBootstrap}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import org.scalacheck.Gen
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import cats.syntax.all.*
import hydrozoa.lib.logging.{ContraTracer, Slf4jTracer}
import hydrozoa.multisig.backend.cardano.{CardanoBackend as L1Backend, FirewalledCardanoBackend, FirewalledCardanoBackendEvent, yaciTestSauceGenesis}
import hydrozoa.multisig.{CoilMultisigRegimeManagerEventFormat, HeadMultisigRegimeManagerEventFormat}
import hydrozoa.multisig.consensus.CardanoLiaisonEvent
import hydrozoa.multisig.consensus.RequestSequencer
import hydrozoa.multisig.consensus.SlowConsensusActorEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{UserRequest, UserRequestBody, UserRequestHeader}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import scalus.uplc.builtin.ByteString
import hydrozoa.multisig.ledger.block.BlockVersion.Major.given_Conversion_Major_Int
import hydrozoa.multisig.ledger.l1.tx.SettlementTx
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects}
import hydrozoa.multisig.{CommonChildEvent, RuleBasedOnlyChildEvent}
import hydrozoa.rulebased.RuleBasedActorEvent
import org.scalacheck.{Prop, Properties}
import scala.concurrent.duration.*
import test.{SeedPhrase, TestPeers, given}

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
  */
object VoteVersionMismatchTest extends Properties("Vote Version Mismatch"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = p.withMinSuccessfulTests(1)

    private val nHeadPeers: Int = 2
    // Real wall-clock budget. Head config's minSettlementDuration + our fast-timing knobs
    // determine major-block cadence; two majors + RRM handoff + Plutus rejection fit in ~60s.
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

        val testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers)
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
                      generateHeadParams = generateHeadParameters(generateTxTiming = fastTxTiming),
                      generateInitializationParameters = InitParamsType.TopDown(
                        InitializationParametersGenTopDown.GenWithDeps(
                          generateGenesisUtxosL1 = ReaderT((_: TestPeers) =>
                              Gen.const(
                                testPeerToUtxos.map { case (k, v) => k.headPeerNumber -> v }
                              )
                          )
                        )
                      ),
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
                .map(mnc => buildCtxResource(transportMode, mnc, testPeers, takeoffTime))

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
            _ <- step4_assertVoteRejected
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

    /** The vote-version-mismatch bug: RRM loads the SEC from the latest hard-confirmed stack
      * (major-2) and attempts to build a Vote for it, but the on-chain treasury still reflects
      * major-1. The Plutus dispute script's `versionMajor` check rejects the vote during client-
      * side tx finalize (before submission), surfacing as a `BuildError.Vote` that crashes the
      * `RuleBasedActor` — captured in `harness.sutErrors`. We assert both signals: RRM tried to
      * build a Vote (proves it found an SEC in a stack whose Major has already been dropped by
      * the firewall) AND the build failed with the specific script error.
      */
    private def step4_assertVoteRejected: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(ctx.voteBuildAttempted.get.timeout(scenarioTimeout))
            _ <- lift(
              (for
                  errs <- ctx.harness.sutErrors.get
                  hit = errs.exists(_.contains("versionMajor field must match"))
                  _ <- if hit then IO.unit else IO.sleep(500.millis)
              yield hit).iterateUntil(identity).timeout(scenarioTimeout)
            )
            errs <- lift(ctx.harness.sutErrors.get)
            _ <- assertWith(
              errs.exists(_.contains("versionMajor field must match")),
              "expected a RuleBasedActor BuildError.Vote whose message contains the " +
                  "'versionMajor field must match' plutus script rejection",
            )
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
        takeoffTime: Option[java.time.Instant],
    ): Resource[IO, Ctx] =
        for
            firewall <- Resource.eval(newFirewallState)
            fallbackDispatched <- Resource.eval(Deferred[IO, Unit])
            bothPeersConfirmedMajor2 <- Resource.eval(Deferred[IO, Unit])
            voteBuildAttempted <- Resource.eval(Deferred[IO, Unit])

            preinitPeerUtxosL1 = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)
                .map { case (name, utxos) => name.headPeerNumber -> utxos }

            // Under TestControl the harness jumps virtual time to `startEpochMs` before any actor
            // exists (see MultiPeerHeadHarness.PreSystem.testControlPresleep). Anchor to the head's
            // configured initial block end-time so the model clock is coherent with the head config.
            startEpochMs = multiNodeConfig.headConfig.initialBlock.blockBrief.endTime
                .convert.instant.toEpochMilli

            hooks = MultiPeerHeadHarness.Hooks[RequestSequencer.Handle, Unit](
              tracer = humanFormatTracer |+| observerTracer(
                bothPeersConfirmedMajor2,
                fallbackDispatched,
                voteBuildAttempted,
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
                coilNodeConfigs = Nil,
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

                    case _ => IO.unit
                }

            case MultiPeerHeadHarness.Event.Coil(_, _) => IO.unit
        }
