package hydrozoa.integration.rbr.property

import cats.data.ReaderT
import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
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
import test.{SeedPhrase, TestPeers}

/** Rule-based regime dispute flow through the [[MultiPeerHeadHarness]] — real MRM + persistence
  * + RBA against a mock L1, exercising the fallback → vote → tally → resolve sequence.
  *
  * Scenario:
  *   1. 2-peer head; per-peer [[FirewalledCardanoBackend]] drops any [[SettlementTx]] with
  *      `versionMajor == 2`, so on-chain lags at v1 while peers hard-confirm through v2 off-chain.
  *   2. Bootstrap L2 request + periodic requests give each Major stack a trailing Minor with SEC.
  *   3. When CL dispatches `Action.FallbackToRuleBased` for major-2, HMRM spawns
  *      [[RuleBasedRegimeManager]] which spawns [[RuleBasedActor]].
  *   4. RBA's persistence-backed `loadAction` walks backward, finds the SEC matching the
  *      on-chain treasury's `versionMajor = 1`, and submits a Vote that Plutus accepts.
  *   5. Voting deadline elapses → TallyTx → ResolutionTx.
  *   6. Test asserts a ResolutionTx submitted successfully (i.e. the dispute resolved).
  *
  * Full evacuation (`RuleBasedActorEvent.Evacuation.NoMore`) is not asserted: the EvacuationTx
  * builder trips the treasury validator's `Trusted setup in the treasury is not big enough`
  * check because [[FallbackTx.scala]] sets `setupG2 = SList.empty` (TODO there). Wiring the
  * KZG G2 trusted setup through fallback construction is separate follow-up work.
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
        val testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers)

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

        val resource = MultiPeerHeadHarness.mkResource(
          transportMode = transportMode,
          testPeers = testPeers,
          testPeerToUtxos = testPeerToUtxos,
          takeoffOffset = 2.seconds,
          disputeResolutionConfig = fastDisputeResolutionConfig,
        ) { (takeoffTime, mnc) =>
            buildCtxResource(transportMode, mnc, testPeers, takeoffTime)
        }

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
