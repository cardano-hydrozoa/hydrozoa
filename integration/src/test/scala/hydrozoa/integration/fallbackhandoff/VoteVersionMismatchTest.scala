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
import hydrozoa.multisig.consensus.SlowConsensusActorEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockVersion.Major.given_Conversion_Major_Int
import hydrozoa.multisig.ledger.l1.tx.SettlementTx
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects}
import hydrozoa.multisig.{CommonChildEvent, RuleBasedOnlyChildEvent}
import hydrozoa.rulebased.RuleBasedActorEvent
import hydrozoa.rulebased.ledger.l1.tx.VoteTx
import org.scalacheck.{Prop, Properties}
import scala.concurrent.duration.*
import scalus.cardano.ledger.TransactionHash
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
    // determine major-block cadence; two majors + hard-confirmations comfortably fit in 5 min.
    private val scenarioTimeout: FiniteDuration = 5.minutes
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
            else Some(java.time.Instant.now().plusSeconds(20))
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
                minSettlementDuration = MinSettlementDuration(30.seconds.quantize(network.slotConfig)),
                // Widen the init tx window: initEndTime = bcet + minSettlementDuration +
                // inactivityMarginDuration. Actor bring-up + stack-0 hard-confirmation + CL's
                // first poll all have to fit inside that window or `InitWindowElapsed` fires.
                inactivityMarginDuration = InactivityMarginDuration(60.seconds.quantize(network.slotConfig)),
                silenceDuration = SilenceDuration(5.seconds.quantize(network.slotConfig)),
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
                  generateNodeOperationMultisigConfig = hc =>
                      hydrozoa.config.node.operation.multisig
                          .generateNodeOperationMultisigConfig(
                            maxPollingPeriod = hc.maxCardanoLiaisonPollingPeriod / 2,
                            rateLimits = hydrozoa.config.node.operation.multisig.RateLimits(
                              softBlockMinPeriod = 5.seconds,
                              hardStackMinPeriod = 2.seconds,
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
        firewall: FirewallState,
        harness: MultiPeerHeadHarness.Harness[Unit, Unit],
        fallbackDispatched: Deferred[IO, Unit],
        bothPeersConfirmedMajor2: Deferred[IO, Unit],
        voteSubmitFailed: Deferred[IO, Unit],
        voteTxIds: Ref[IO, Map[HeadPeerNumber, TransactionHash]],
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

    private def step4_assertVoteRejected: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(ctx.voteSubmitFailed.get.timeout(scenarioTimeout))
            voteIds <- lift(ctx.voteTxIds.get)
            submitted <- lift(ctx.firewall.submitted.get)
            _ <- assertWith(
              voteIds.nonEmpty,
              "expected at least one peer to have submitted a Vote tx",
            )
            _ <- voteIds.toList.foldLeft(pure(())) { case (acc, (peer, voteId)) =>
                val peerSubs = submitted.getOrElse(peer, Nil)
                val voteSub  = peerSubs.find(_.txHash == voteId)
                for
                    _ <- acc
                    _ <- assertWith(
                      voteSub.isDefined,
                      s"peer $peer: no SubmittedTx event for Vote id $voteId",
                    )
                    _ <- voteSub match
                        case None => pure(())
                        case Some(sub) =>
                            assertWith(
                              sub.result.left
                                  .exists(_.isInstanceOf[L1Backend.Error.InvalidTx]),
                              s"peer $peer: expected Left(InvalidTx) on Vote submission, " +
                                  s"got ${sub.result}",
                            )
                yield ()
            }
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
            voteSubmitFailed <- Resource.eval(Deferred[IO, Unit])
            voteTxIds <- Resource.eval(
              Ref[IO].of(Map.empty[HeadPeerNumber, TransactionHash])
            )

            preinitPeerUtxosL1 = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)
                .map { case (name, utxos) => name.headPeerNumber -> utxos }

            // Under TestControl the harness jumps virtual time to `startEpochMs` before any actor
            // exists (see MultiPeerHeadHarness.PreSystem.testControlPresleep). Anchor to the head's
            // configured initial block end-time so the model clock is coherent with the head config.
            startEpochMs = multiNodeConfig.headConfig.initialBlock.blockBrief.endTime
                .convert.instant.toEpochMilli

            hooks = MultiPeerHeadHarness.Hooks[Unit, Unit](
              tracer = humanFormatTracer |+| observerTracer(
                bothPeersConfirmedMajor2,
                fallbackDispatched,
                voteTxIds,
                voteSubmitFailed,
              ),
              peerHandle = (_, _) => IO.unit,
              coilHandle = (_, _) => IO.unit,
              wrapPeerBackend = wrapPeerBackend(firewall),
            )

            label = s"VoteVersionMismatch-${transportMode.toString.toLowerCase}"

            harness <- MultiPeerHeadHarness.resource[Unit, Unit](
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
          firewall = firewall,
          harness = harness,
          fallbackDispatched = fallbackDispatched,
          bothPeersConfirmedMajor2 = bothPeersConfirmedMajor2,
          voteSubmitFailed = voteSubmitFailed,
          voteTxIds = voteTxIds,
        )

    // ------------------------------------------------------------------
    // Wiring helpers
    // ------------------------------------------------------------------

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
                  case s: SettlementTx => s.majorVersionProduced.convert == 2
                  case _               => false
              }),
          firewallTracer = perPeerTracer,
        )

    /** Observer tracer wiring — see class-level Ctx doc for what each signal represents. */
    private def observerTracer(
        bothPeersConfirmedMajor2: Deferred[IO, Unit],
        fallbackDispatched: Deferred[IO, Unit],
        voteTxIds: Ref[IO, Map[HeadPeerNumber, TransactionHash]],
        voteSubmitFailed: Deferred[IO, Unit],
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
