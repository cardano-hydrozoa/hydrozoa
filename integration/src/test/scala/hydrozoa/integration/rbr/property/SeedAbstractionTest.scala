package hydrozoa.integration.rbr.property

import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.integration.harness.{MultiPeerDisputeProperties, MultiPeerHeadHarness}
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.CommonChildEvent
import hydrozoa.multisig.backend.cardano.{FirewalledCardanoBackend, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, RequestSequencer}
import org.scalacheck.Prop
import scala.concurrent.duration.*
import test.{SeedPhrase, TestPeers}

/** De-risks the RBR bisimulation abstraction: drive a multi-peer head to fallback with the entire
  * dispute frozen (drop the v2 settlement to trigger fallback, drop every dispute-family tx so the
  * [[hydrozoa.rulebased.RuleBasedActor]] can't advance the state), then assert the pristine
  * post-fallback L1 snapshot projects to the same [[ObservableMarking]] as the `RBRHlNet` seed —
  * `alpha(seed) == beta(post-fallback)`. This pins the model↔SUT seeding mapping before the
  * per-step bisimulation loop is wired.
  */
object SeedAbstractionTest extends MultiPeerDisputeProperties("RBR Seed Abstraction"):

    private val nHeadPeers: Int                 = 3
    private val nCoilPeers: Int                 = 2
    private val maxVersionMinor: Int            = 2
    private val scenarioTimeout: FiniteDuration = 5.minutes
    private val quiescenceDelay: FiniteDuration = 2.seconds

    // Freeze the dispute: dropping the v2 settlement triggers fallback; dropping every dispute
    // family keeps the RBA from mutating the post-fallback state so the snapshot stays at the seed.
    private val disputeFamilies: Set[String] =
        Set("VoteTx", "AbstainTx", "RatchetVoteTx", "TallyTx", "Resolution", "EvacuationTx", "Deinit")

    private val freezeDispute: MultiPeerHeadHarness.DropRule =
        MultiPeerHeadHarness.DropRule(etx => disputeFamilies.contains(etx.transactionFamily))

    val _ = property("ws: post-fallback L1 snapshot matches the RBRHlNet seed") =
        testProperty(TransportMode.WebSocket)

    private def testProperty(transportMode: TransportMode): Prop =
        val testPeers       = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers, nCoilPeers)
        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)

        val resource = MultiPeerHeadHarness.mkResource(
          transportMode = transportMode,
          testPeers = testPeers,
          testPeerToUtxos = testPeerToUtxos,
          takeoffOffset = 60.seconds,
          coilPeers = testPeers.coilPeersConfig(hub = HeadPeerNumber(0)),
          coilQuorum = nCoilPeers,
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
            _ <- step3_assertSeedAbstraction
        yield true

    private final case class Ctx(
        harness: MultiPeerHeadHarness.Harness[Option[RequestSequencer.Handle]],
        fallbackDispatched: Deferred[IO, Unit],
        periodicRequestFiber: Ref[IO, Option[FiberIO[Nothing]]],
    )

    private def step1a_submitBootstrapRequest: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(MultiPeerHeadHarness.submitKickRequest(ctx.harness))
        yield ()

    private def step1b_startPeriodicRequestLoop: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            fiber <- lift(
              (IO.sleep(1.second) >> MultiPeerHeadHarness.submitKickRequest(ctx.harness)).foreverM.start
            )
            _ <- lift(ctx.periodicRequestFiber.set(Some(fiber)))
        yield ()

    private def step2_awaitFallbackToRuleBasedHandoff: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(ctx.fallbackDispatched.get.timeout(scenarioTimeout))
        yield ()

    /** Cancel the request loop, let the fallback tx settle (the dispute is frozen, so no dispute tx
      * can land in this window), snapshot the shared L1, and assert `alpha(seed) == beta(snapshot)`.
      */
    private def step3_assertSeedAbstraction: test.TestM[Ctx, Unit] =
        for
            ctx   <- ask
            _     <- lift(ctx.periodicRequestFiber.get.flatMap(_.traverse_(_.cancel)))
            _     <- lift(IO.sleep(quiescenceDelay))
            utxos <- lift(ctx.harness.l1Snapshot)
            beta <- lift(
              IO.fromEither(
                ObservableMarking
                    .beta(utxos)(using ctx.harness.multiNodeConfig)
                    .leftMap(new RuntimeException(_))
              )
            )
            alpha = ObservableMarking.alpha(RBRHlNet(nHeadPeers, maxVersionMinor).toOption.get)
            _ <- assertWith(
              alpha == beta,
              s"seed abstraction mismatch:\n  alpha (model): $alpha\n  beta  (L1):    $beta",
            )
        yield ()

    // ------------------------------------------------------------------
    // Wiring
    // ------------------------------------------------------------------

    private def buildCtxResource(
        transportMode: TransportMode,
        multiNodeConfig: MultiNodeConfig,
        testPeers: TestPeers,
        takeoffTime: Option[java.time.Instant],
    ): Resource[IO, Ctx] =
        for
            fallbackDispatched   <- Resource.eval(Deferred[IO, Unit])
            periodicRequestFiber <- Resource.eval(Ref[IO].of(Option.empty[FiberIO[Nothing]]))

            harness <- MultiPeerHeadHarness.disputeHarnessResource(
              label = s"RBRSeedAbstraction-${transportMode.toString.toLowerCase}",
              transportMode = transportMode,
              multiNodeConfig = multiNodeConfig,
              testPeers = testPeers,
              takeoffTime = takeoffTime,
              tracer = MultiPeerHeadHarness.humanFormatTracer(nHeadPeers) |+| observerTracer(
                fallbackDispatched
              ),
              wrapBackend = (peerId, backend) =>
                  FirewalledCardanoBackend(
                    underlying = backend,
                    shouldDrop =
                        (MultiPeerHeadHarness.DropRule.settlementProducingMajor(2) || freezeDispute).toGate,
                    firewallTracer = MultiPeerHeadHarness.firewallSlf4jSink(peerId),
                  ),
            )
        yield Ctx(harness, fallbackDispatched, periodicRequestFiber)

    private def observerTracer(
        fallbackDispatched: Deferred[IO, Unit]
    ): ContraTracer[IO, MultiPeerHeadHarness.Event] =
        val onEvent: Any => IO[Unit] = {
            case CommonChildEvent.CardanoLiaison(
                  _: CardanoLiaisonEvent.FallbackToRuleBasedDispatched
                ) =>
                fallbackDispatched.complete(()).void
            case _ => IO.unit
        }
        ContraTracer[IO, MultiPeerHeadHarness.Event] {
            case MultiPeerHeadHarness.Event.Head(_, evt) => onEvent(evt)
            case MultiPeerHeadHarness.Event.Coil(_, evt) => onEvent(evt)
        }
