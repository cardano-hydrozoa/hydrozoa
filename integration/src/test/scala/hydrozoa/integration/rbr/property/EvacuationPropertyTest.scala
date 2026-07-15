package hydrozoa.integration.rbr.property

import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.harness.{MultiPeerDisputeProperties, MultiPeerHeadHarness}
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.lib.logging.{ContraTracer, Slf4jTracer}
import scala.annotation.unused
import hydrozoa.multisig.backend.cardano.{FirewalledCardanoBackend, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, RequestSequencer}
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.lib.classification.Histogram
import hydrozoa.multisig.{CommonChildEvent, RuleBasedOnlyChildEvent}
import hydrozoa.rulebased.RuleBasedActorEvent
import org.scalacheck.Prop
import scala.concurrent.duration.*
import scalus.cardano.ledger.{Utxo, Utxos}
import test.{SeedPhrase, TestPeers}

/** Rule-based regime dispute flow through the [[MultiPeerHeadHarness]] — real MRM + persistence
  * + RBA against a mock L1, exercising the fallback → vote → tally → resolve sequence.
  *
  * Scenario:
  *   1. 3-peer head + 2 coil followers; per-head-peer [[FirewalledCardanoBackend]] drops any
  *      [[SettlementTx]] with
  *      `versionMajor == 2`, so on-chain lags at v1 while peers hard-confirm through v2 off-chain.
  *   2. Bootstrap L2 request + periodic requests give each Major stack a trailing Minor with SEC.
  *   3. When CL dispatches `Action.FallbackToRuleBased` for major-2, HMRM spawns
  *      [[RuleBasedRegimeManager]] which spawns [[RuleBasedActor]].
  *   4. RBA's persistence-backed `loadAction` walks backward, finds the SEC matching the
  *      on-chain treasury's `versionMajor = 1`, and submits a Vote that Plutus accepts.
  *   5. Voting deadline elapses → TallyTx → ResolutionTx.
  *   6. Every peer's RBA builds and submits an [[EvacuationTx]] until its `Evacuation.NoMore`
  *      terminal event fires.
  *   7. Test asserts the shared-L1 UTxO histogram matches the expected terminal cardinalities.
  */
object EvacuationPropertyTest extends MultiPeerDisputeProperties("RBR Evacuation Property"):

    private val nHeadPeers: Int              = 3
    private val nCoilPeers: Int              = 2
    private val scenarioTimeout: FiniteDuration = 5.minutes

    val _ = property("ws: fallback→RRM→vote→tally→resolve→evacuate happy path") =
        testProperty(TransportMode.WebSocket)

    private def testProperty(transportMode: TransportMode): Prop =
        val testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers, nCoilPeers)

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
            _ <- step3_awaitResolutionSubmitted
            _ <- step4_awaitEvacuationDone
            _ <- step5_assertTerminalHistogram
        yield true

    /** State + handles threaded between steps. */
    private final case class Ctx(
        harness: MultiPeerHeadHarness.Harness[Option[RequestSequencer.Handle]],
        fallbackDispatched: Deferred[IO, Unit],
        resolutionSubmitted: Deferred[IO, Unit],
        // Set of peers whose RBA has fired `Evacuation.NoMore`. `evacuationDone` is only
        // completed once every head + coil peer has fired — waiting for the first NoMore is
        // racy: the winning-drain peer fires while others may still be mid-submission.
        peersEvacuationDone: Ref[IO, Set[PeerId]],
        evacuationDone: Deferred[IO, Unit],
        // First `PayoutsLeft(n)` observed on any peer — the KZG-committed evacuation count at
        // the RBA's read of the resolved treasury. Subsequent PayoutsLeft values are strictly
        // smaller as drain progresses; taking the first captures the pre-drain total.
        firstPayoutsLeft: Ref[IO, Option[Int]],
        periodicRequestFiber: Ref[IO, Option[FiberIO[Nothing]]],
    )

    private def step1a_submitBootstrapRequest: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _ <- lift(MultiPeerHeadHarness.submitEmptyTransactionRequest(ctx.harness))
        yield ()

    /** Keep feeding requests so each Major stack has a trailing Minor with an SEC — otherwise
      * `loadAction` walks backward past every Major-only stack and abstains, tally/resolve then
      * takes the default vote's kzg. Either terminal state is acceptable for this test, but the
      * shorter path via a real Vote exercises more of the flow.
      */
    private def step1b_startPeriodicRequestLoop: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            fiber <- lift(
              (IO.sleep(1.second) >> MultiPeerHeadHarness.submitEmptyTransactionRequest(
                ctx.harness
              )).foreverM.start
            )
            _ <- lift(ctx.periodicRequestFiber.set(Some(fiber)))
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

    private def step4_awaitEvacuationDone: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(ctx.evacuationDone.get.timeout(scenarioTimeout))
        yield ()

    /** After evacuation completes, cancel the periodic-request loop, wait a beat for any
      * in-flight tx to settle, snapshot the shared mock L1, and check the UTxO distribution
      * matches the expected terminal buckets.
      */
    private def step5_assertTerminalHistogram: test.TestM[Ctx, Unit] =
        for
            ctx    <- ask
            _      <- lift(ctx.periodicRequestFiber.get.flatMap(_.traverse_(_.cancel)))
            _      <- lift(IO.sleep(quiescenceDelay))
            utxos  <- lift(ctx.harness.l1Snapshot)
            actual <- lift(runClassifier(utxos)(using ctx.harness.multiNodeConfig))
            expectedEvacCount <- lift(ctx.firstPayoutsLeft.get.flatMap(
              IO.fromOption(_)(
                new IllegalStateException(
                  "no `Evacuation.PayoutsLeft` observed; RBA never entered the drain loop"
                )
              )
            ))
            expected = expectedCardinalities(expectedEvacCount)
            _ <- assertWith(
              actual == expected,
              s"Cardinality mismatch:\n  expected: $expected\n  actual:   $actual",
            )
        yield ()

    // Time to wait after cancelling the periodic loop for any in-flight tx to reach the mock.
    // 2s covers a full CL polling period + tx submission at the fast harness timing.
    private val quiescenceDelay: FiniteDuration = 2.seconds

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
            fallbackDispatched   <- Resource.eval(Deferred[IO, Unit])
            resolutionSubmitted  <- Resource.eval(Deferred[IO, Unit])
            peersEvacuationDone  <- Resource.eval(Ref[IO].of(Set.empty[PeerId]))
            evacuationDone       <- Resource.eval(Deferred[IO, Unit])
            firstPayoutsLeft     <- Resource.eval(Ref[IO].of(Option.empty[Int]))
            periodicRequestFiber <- Resource.eval(Ref[IO].of(Option.empty[FiberIO[Nothing]]))

            harness <- MultiPeerHeadHarness.disputeHarnessResource(
              label = s"RBREvacuation-${transportMode.toString.toLowerCase}",
              transportMode = transportMode,
              multiNodeConfig = multiNodeConfig,
              testPeers = testPeers,
              takeoffTime = takeoffTime,
              tracer = MultiPeerHeadHarness.humanFormatTracer(nHeadPeers) |+| observerTracer(
                fallbackDispatched,
                resolutionSubmitted,
                peersEvacuationDone,
                evacuationDone,
                firstPayoutsLeft,
              ),
              wrapBackend = (peerId, backend) =>
                  FirewalledCardanoBackend(
                    underlying = backend,
                    shouldDrop = MultiPeerHeadHarness.DropRule.settlementProducingMajor(2).toGate,
                    firewallTracer = MultiPeerHeadHarness.firewallSlf4jSink(peerId),
                  ),
            )
        yield Ctx(
          harness = harness,
          fallbackDispatched = fallbackDispatched,
          resolutionSubmitted = resolutionSubmitted,
          peersEvacuationDone = peersEvacuationDone,
          evacuationDone = evacuationDone,
          firstPayoutsLeft = firstPayoutsLeft,
          periodicRequestFiber = periodicRequestFiber,
        )

    // ------------------------------------------------------------------
    // Wiring
    // ------------------------------------------------------------------

    private def observerTracer(
        fallbackDispatched: Deferred[IO, Unit],
        resolutionSubmitted: Deferred[IO, Unit],
        peersEvacuationDone: Ref[IO, Set[PeerId]],
        evacuationDone: Deferred[IO, Unit],
        firstPayoutsLeft: Ref[IO, Option[Int]],
    ): ContraTracer[IO, MultiPeerHeadHarness.Event] =
        // Both HeadRegimeManagerEvent and CoilRegimeManagerEvent embed the same
        // `CommonChildEvent` and `RuleBasedOnlyChildEvent` cases, so one PF handles either
        // side.
        val onEvent: PeerId => Any => IO[Unit] = peer => {
            case CommonChildEvent.CardanoLiaison(
                  _: CardanoLiaisonEvent.FallbackToRuleBasedDispatched
                ) =>
                fallbackDispatched.complete(()).void

            case RuleBasedOnlyChildEvent.RuleBasedActor(
                  RuleBasedActorEvent.Tx.SubmitSuccess(tx)
                ) if tx.transactionFamily == "Resolution" =>
                resolutionSubmitted.complete(()).void

            case RuleBasedOnlyChildEvent.RuleBasedActor(
                  RuleBasedActorEvent.Evacuation.PayoutsLeft(n)
                ) =>
                firstPayoutsLeft.update(_.orElse(Some(n)))

            case RuleBasedOnlyChildEvent.RuleBasedActor(
                  RuleBasedActorEvent.Evacuation.NoMore
                ) =>
                peersEvacuationDone
                    .updateAndGet(_ + peer)
                    .flatMap { seen =>
                        IO.whenA(seen.size == nHeadPeers + nCoilPeers)(
                          evacuationDone.complete(()).void
                        )
                    }

            case _ => IO.unit
        }
        ContraTracer[IO, MultiPeerHeadHarness.Event] {
            case MultiPeerHeadHarness.Event.Head(peerNum, evt) =>
                onEvent(PeerId.Head(peerNum))(evt)
            case MultiPeerHeadHarness.Event.Coil(coilNum, evt) =>
                onEvent(PeerId.Coil(coilNum))(evt)
        }

    // ------------------------------------------------------------------
    // Terminal-state classifier
    // ------------------------------------------------------------------

    /** Bucket every UTxO in the shared L1 snapshot via [[RBRClassifier]]. Raises on ambiguity —
      * classifier fns are meant to be disjoint. Returns a total map keyed by every place id so
      * comparisons against `expectedCardinalities` see missing buckets as 0.
      */
    private def runClassifier(
        utxos: Utxos
    )(using MultiNodeConfig): IO[Map[RBRPlaceId, Int]] =
        val classifier = new RBRClassifier
        val allUtxos   = utxos.toList.map { case (i, o) => Utxo(i, o) }
        Histogram.empty(classifier).addAll(allUtxos).toEither match
            case Left(errs) =>
                IO.raiseError(
                  new RuntimeException(s"Ambiguous UTxO classification: ${errs.toList}")
                )
            case Right(hist) =>
                // logAmbientUtxos(classifier, allUtxos) *>
                IO.pure(RBRPlaceId.values.toList.map(k => k -> hist(k)).toMap)

    /** Diagnostic helper: emit each ambient-bucket UTxO (input / address / value / datum) via
      * Slf4j so the composition of [[AmbientPlaceId]] can be identified when the terminal
      * cardinality shifts. Toggle by uncommenting the `logAmbientUtxos(...)` call site in
      * [[runClassifier]]. Not on the happy path so callers pay nothing when commented out.
      */
    @unused
    private def logAmbientUtxos(classifier: RBRClassifier, utxos: List[Utxo]): IO[Unit] =
        val ambient = utxos.filter(u => classifier.classify(u).contains(AmbientPlaceId))
        val lines = ambient.zipWithIndex.map { case (u, idx) =>
            s"  [$idx] input=${u.input} addr=${u.output.address} value=${u.output.value} datum=${u.output.datumOption}"
        }.mkString("\n")
        Slf4jTracer.sink.traceWith(
          hydrozoa.lib.logging.LogEvent
              .From(Map.empty, "AmbientDiagnostic")
              .info(s"AmbientPlaceId utxos (${ambient.size}):\n$lines")
        )

    /** Expected terminal cardinalities: init → settle-v1 → fallback → vote → tally → resolve →
      * evacuate on the happy path. `evacuationCount` is the first `PayoutsLeft(n)` observed
      * from any peer's RBA — exactly the size of the resolved treasury's evacuation map before
      * drain begins (see `RuleBasedActor.Evacuation.handle`).
      *   - `EvacuationOutputPlaceId -> evacuationCount`: every L2 output was drained to L1 via
      *     the RBA's `EvacuationTx` loop; each output carries the `"evacuation"` inline-datum
      *     sentinel stamped by [[InitializationParametersGen.generatePeerContribution]], which
      *     survives the KZG membership hash and so appears on the L1 evacuation outputs.
      *   - `PayoutObligationsPlaceId -> 0`: no in-flight payout obligations remain on the
      *     resolved treasury.
      *   - `CollateralPlaceId -> 0`: the [[RBRClassifier]] identifies collateral by the
      *     `"collateral"` datum sentinel, which only exists in the synthetic
      *     [[InitialDisputeUtxos]] fixture — the real tx builders draw collateral from plain
      *     Ada wallet UTxOs (no datum). Preserved collateral therefore lands in
      *     [[AmbientPlaceId]] here. TODO: the right way to bucket collateral in an end-to-end
      *     scenario is to walk the tx graph and mark any output that is later consumed as a
      *     `collateral_input` of some downstream tx — i.e. classify by role in tx history
      *     rather than by content sentinel.
      *   - `AmbientPlaceId -> nHeadPeers * 2 + nCoilPeers`: head peers keep two wallet-ADA
      *     streams at their shelley address — an [[InitializationTx]] change output and a
      *     [[FallbackTx]] equity payout. Coil peers keep only the init-change stream (no
      *     fallback equity). Every RBR-side script tx that pays its fee from collateral
      *     (Vote/Resolution/Evacuation) picks one of these as its collateral input and returns
      *     it (fee-subtracted) at the same address, so no utxo is destroyed.
      */
    private def expectedCardinalities(evacuationCount: Int): Map[RBRPlaceId, Int] =
        Map(
          TreasuryRefPlaceId        -> 1,
          DisputeRefPlaceId         -> 1,
          RegimeRefPlaceId          -> 1,
          SetupLadderRefPlaceId     -> 7,
          ResolvedTreasuryPlaceId   -> 1,
          UnresolvedTreasuryPlaceId -> 0,
          VotedPlaceId              -> 0,
          UnvotedPlaceId            -> 0,
          CollateralPlaceId         -> 0,
          EvacuationOutputPlaceId   -> evacuationCount,
          PayoutObligationsPlaceId  -> 0,
          AmbientPlaceId            -> (nHeadPeers * 2 + nCoilPeers),
        )
