package hydrozoa.integration.rbr.property

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
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.config.head.{InitParamsType, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, quantize}
import hydrozoa.lib.logging.{ContraTracer, Slf4jTracer}
import hydrozoa.multisig.backend.cardano.{CardanoBackend as L1Backend, FirewalledCardanoBackend, FirewalledCardanoBackendEvent, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, RequestSequencer, UserRequest, UserRequestBody, UserRequestHeader}
import hydrozoa.multisig.ledger.l1.tx.SettlementTx
import hydrozoa.multisig.ledger.block.BlockVersion.Major.given_Conversion_Major_Int
import hydrozoa.multisig.{CoilMultisigRegimeManagerEventFormat, CommonChildEvent, HeadMultisigRegimeManagerEventFormat, RuleBasedOnlyChildEvent}
import hydrozoa.rulebased.RuleBasedActorEvent
import org.scalacheck.{Gen, Prop, Properties}
import scala.concurrent.duration.*
import scalus.uplc.builtin.ByteString
import test.{SeedPhrase, TestPeers, given}

/** Rule-based regime dispute flow through the [[MultiPeerHeadHarness]] — replaces the previous
  * version which spawned [[RuleBasedActor]]s directly with mocked loader lambdas (no longer
  * possible after the persistence-backed loader migration).
  *
  * Scenario:
  *   1. 2-peer head; per-peer [[FirewalledCardanoBackend]] drops any [[SettlementTx]] with
  *      `versionMajor == 2`, so on-chain lags at v1 while peers hard-confirm through v2 off-chain.
  *   2. Bootstrap L2 request + periodic requests give each Major stack a trailing Minor with SEC.
  *   3. When CL dispatches `Action.FallbackToRuleBased` for major-2, HMRM spawns
  *      [[RuleBasedRegimeManager]] which spawns [[RuleBasedActor]].
  *   4. RBA's persistence-backed `loadAction` walks backward, finds SEC-v1 matching the on-chain
  *      treasury, and submits a Vote that Plutus accepts (guarded by the version-major fix).
  *   5. Voting deadline elapses → TallyTx → ResolutionTx.
  *   6. Test asserts a ResolutionTx submitted successfully (i.e. the dispute resolved).
  *
  * Full evacuation (`RuleBasedActorEvent.Evacuation.NoMore`) is NOT asserted: the EvacuationTx
  * builder trips the treasury validator's `Trusted setup in the treasury is not big enough`
  * check because [[FallbackTx.scala]] currently sets `setupG2 = SList.empty` (marked TODO).
  * Wiring the KZG G2 trusted setup through fallback construction is a separate follow-up.
  */
object EvacuationPropertyTest extends Properties("RBR Evacuation Property"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = p.withMinSuccessfulTests(1)

    private val nHeadPeers: Int              = 2
    private val scenarioTimeout: FiniteDuration = 3.minutes
    private val cardanoNetwork: CardanoNetwork = CardanoNetwork.Preprod

    val _ = property("ws: fallback→RRM→vote→tally→resolve dispute completes") =
        testProperty(TransportMode.WebSocket)

    private def testProperty(transportMode: TransportMode): Prop =
        import org.scalacheck.util.Pretty

        given (MultiNodeConfig => Pretty) = _ => Pretty(_ => "MultiNodeConfig (too long)")

        val testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers)
        val takeoffTime: Option[java.time.Instant] =
            if transportMode.useTestControl then None
            else Some(java.time.Instant.now().plusSeconds(2))
        val generateHeadStartTime: test.GenWithTestPeers[BlockCreationEndTime] =
            ReaderT { tp =>
                takeoffTime match {
                    case Some(t) => Gen.const(BlockCreationEndTime(t.quantize(tp.slotConfig)))
                    case None    =>
                        val anchorTime = 1767225600L
                        val range      = 86_400 * 100L
                        for offset <- Gen.choose(0L, range)
                        yield BlockCreationEndTime(
                          java.time.Instant.ofEpochSecond(anchorTime + offset).quantize(tp.slotConfig)
                        )
                }
            }
        val fastTxTiming: test.GenWithTestPeers[TxTiming] = ReaderT { network =>
            Gen.const(
              TxTiming(
                minSettlementDuration = MinSettlementDuration(2.seconds.quantize(network.slotConfig)),
                inactivityMarginDuration = InactivityMarginDuration(3.seconds.quantize(network.slotConfig)),
                silenceDuration = SilenceDuration(1.second.quantize(network.slotConfig)),
                depositSubmissionDuration = DepositSubmissionDuration(1.second.quantize(network.slotConfig)),
                depositMaturityDuration = DepositMaturityDuration(1.second.quantize(network.slotConfig)),
                depositAbsorptionDuration = DepositAbsorptionDuration(2.minutes.quantize(network.slotConfig)),
              )
            )
        }

        // Fast voting deadline so TallyTx becomes valid within our scenario budget. The default
        // generator picks 1h..5d — way beyond our reach. `deadlineVoting` is
        // `fallbackValidityStart + votingDuration`, and TallyTx has `validityStart = deadlineVoting`.
        val fastDisputeResolutionConfig: test.GenWithTestPeers[DisputeResolutionConfig] =
            ReaderT { network =>
                Gen.const(
                  DisputeResolutionConfig(
                    votingDuration = QuantizedFiniteDuration(
                      slotConfig = network.slotConfig,
                      finiteDuration = 5.seconds
                    )
                  )
                )
            }

        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)
        val genMultiNodeConfig =
            MultiNodeConfig
                .generateWith(testPeers)(
                  generateHeadConfig = generateHeadConfig(
                    genHeadConfigBootstrap = generateHeadConfigBootstrap(
                      generateHeadParams = generateHeadParameters(
                        generateTxTiming = fastTxTiming,
                        generateDisputeResolutionConfig = fastDisputeResolutionConfig,
                      ),
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
    // Scenario body
    // ------------------------------------------------------------------

    private val ctxTestM = test.TestMFixedEnv[Ctx]()
    import ctxTestM.*

    private def scenarioTestM: test.TestM[Ctx, Boolean] =
        for
            _ <- step1a_submitBootstrapRequest
            _ <- step1b_startPeriodicRequestLoop
            _ <- step2_awaitFallbackToRuleBasedHandoff
            _ <- step3_awaitResolutionSubmitted
        yield true

    /** State + handles threaded between steps. */
    private final case class Ctx(
        transportMode: TransportMode,
        multiNodeConfig: MultiNodeConfig,
        harness: MultiPeerHeadHarness.Harness[RequestSequencer.Handle, Unit],
        fallbackDispatched: Deferred[IO, Unit],
        resolutionSubmitted: Deferred[IO, Unit],
    )

    private def step1a_submitBootstrapRequest: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(submitOneUserRequest(ctx))
        yield ()

    /** Keep feeding requests so each Major stack has a trailing Minor with an SEC — otherwise
      * `loadAction` walks backward past every Major-only stack and abstains, tally/resolve then
      * takes the default vote's kzg. Either terminal state is acceptable for this test, but the
      * shorter path via a real Vote exercises more of the flow.
      */
    private def step1b_startPeriodicRequestLoop: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _ <- lift(
              (IO.sleep(1.second) >> submitOneUserRequest(ctx)).foreverM.start.void
            )
        yield ()

    private def step2_awaitFallbackToRuleBasedHandoff: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(ctx.fallbackDispatched.get.timeout(scenarioTimeout))
        yield ()

    private def step3_awaitResolutionSubmitted: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(ctx.resolutionSubmitted.get.timeout(scenarioTimeout))
        yield ()

    // ------------------------------------------------------------------
    // Ctx bring-up
    // ------------------------------------------------------------------

    private def buildCtxResource(
        transportMode: TransportMode,
        multiNodeConfig: MultiNodeConfig,
        testPeers: TestPeers,
        takeoffTime: Option[java.time.Instant],
    ): Resource[IO, Ctx] =
        for
            fallbackDispatched  <- Resource.eval(Deferred[IO, Unit])
            resolutionSubmitted <- Resource.eval(Deferred[IO, Unit])

            preinitPeerUtxosL1 = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)
                .map { case (name, utxos) => name.headPeerNumber -> utxos }

            startEpochMs = multiNodeConfig.headConfig.initialBlock.blockBrief.endTime
                .convert.instant.toEpochMilli

            hooks = MultiPeerHeadHarness.Hooks[RequestSequencer.Handle, Unit](
              tracer = humanFormatTracer |+| observerTracer(
                fallbackDispatched,
                resolutionSubmitted,
              ),
              peerHandle = (peerNum, conns) =>
                  IO.fromOption(conns.requestSequencer)(
                    new NoSuchElementException(
                      s"peer $peerNum has no RequestSequencer.Handle in its Connections"
                    )
                  ),
              coilHandle = (_, _) => IO.unit,
              wrapPeerBackend = wrapPeerBackend,
            )

            label = s"RBREvacuation-${transportMode.toString.toLowerCase}"

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
          harness = harness,
          fallbackDispatched = fallbackDispatched,
          resolutionSubmitted = resolutionSubmitted,
        )

    // ------------------------------------------------------------------
    // Wiring
    // ------------------------------------------------------------------

    private def submitOneUserRequest(ctx: Ctx): IO[Unit] =
        val peerNum    = HeadPeerNumber(0)
        val slotConfig = ctx.multiNodeConfig.headConfig.cardanoNetwork.slotConfig
        val body: UserRequestBody.TransactionRequestBody =
            UserRequestBody.TransactionRequestBody(
              l2Payload = ByteString.fromArray(Array.empty[Byte])
            )
        val userVk =
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

    /** Drop any settlement whose produced treasury datum has `versionMajor == 2` — that's the
      * settlement that would take on-chain from v1 to v2. Keeping on-chain at v1 while peers
      * hard-confirm through v2 off-chain is what triggers `Action.FallbackToRuleBased`.
      */
    private def wrapPeerBackend(
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
        FirewalledCardanoBackend(
          underlying = underlying,
          shouldDrop = etx =>
              IO.pure(etx match {
                  case s: SettlementTx => s.majorVersionProduced.convert == 2
                  case _               => false
              }),
          firewallTracer = slf4jSink,
        )

    private def observerTracer(
        fallbackDispatched: Deferred[IO, Unit],
        resolutionSubmitted: Deferred[IO, Unit],
    ): ContraTracer[IO, MultiPeerHeadHarness.Event] =
        ContraTracer[IO, MultiPeerHeadHarness.Event] {
            case MultiPeerHeadHarness.Event.Head(_, evt) =>
                evt match {
                    case CommonChildEvent.CardanoLiaison(
                          _: CardanoLiaisonEvent.FallbackToRuleBasedDispatched
                        ) =>
                        fallbackDispatched.complete(()).void

                    case RuleBasedOnlyChildEvent.RuleBasedActor(
                          RuleBasedActorEvent.Tx.SubmitSuccess(tx)
                        ) if tx.transactionFamily == "Resolution" =>
                        resolutionSubmitted.complete(()).void

                    case _ => IO.unit
                }

            case MultiPeerHeadHarness.Event.Coil(_, _) => IO.unit
        }
