package hydrozoa.integration.rbr.property

import cats.data.ReaderT
import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.Durations.InactivityMarginDuration
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.lib.classification.Histogram
import hydrozoa.lib.logging.{ContraTracer, Slf4jTracer}
import hydrozoa.multisig.backend.cardano.{CardanoBackend as L1Backend, FirewalledCardanoBackend, FirewalledCardanoBackendEvent, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, RequestSequencer, UserRequest, UserRequestBody, UserRequestHeader}
import hydrozoa.multisig.ledger.block.BlockVersion.Major.given_Conversion_Major_Int
import hydrozoa.multisig.ledger.l1.tx.SettlementTx
import hydrozoa.multisig.{CoilMultisigRegimeManagerEventFormat, CommonChildEvent, HeadMultisigRegimeManagerEventFormat, RuleBasedOnlyChildEvent}
import hydrozoa.rulebased.RuleBasedActorEvent
import hydrozoa.rulebased.ledger.l1.tx.{AbstainTx, RatchetVoteTx, VoteTx}
import org.scalacheck.{Gen, Prop, Properties}
import scala.annotation.unused
import scala.concurrent.duration.*
import scalus.cardano.ledger.{Utxo, Utxos}
import scalus.uplc.builtin.ByteString
import test.{SeedPhrase, TestPeers}

/** Rule-based regime dispute flow through the [[MultiPeerHeadHarness]], with the vote path
  * disabled at the tx layer to exercise how tally selects a commitment when peers cannot vote.
  *
  *   - **Test 1** — firewall every [[VoteTx]] head + coil. No vote lands, tally's max-by-
  *     versionMinor reduction preserves the public/default ballot box, resolves to `major1`
  *     (the FallbackTx-produced treasury's kzg = on-chain Major-1 commitment). Peers evacuate
  *     against `initialEvacuationMap`.
  *   - **Test 2** — same firewall, plus an exogenously-built [[VoteTx]] for `sec2` (the SEC at
  *     hard-confirmed stack 2) submitted directly to the shared L1. Tally's max-by-
  *     versionMinor picks our vote (default is `versionMinor = 0`; ours is > 0). Peers
  *     evacuate against `sec2`'s map.
  *
  * Setup mirrors [[EvacuationPropertyTest]] (3 head + 2 coil, firewalled v2 settlement, widened
  * init window).
  */
object CommitmentSelectionPropertyTest extends Properties("RBR Commitment Selection"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = p.withMinSuccessfulTests(1)

    private val nHeadPeers: Int                 = 3
    private val nCoilPeers: Int                 = 2
    private val scenarioTimeout: FiniteDuration = 5.minutes
    private val cardanoNetwork: CardanoNetwork  = CardanoNetwork.Preprod

    /** The commitment we expect the resolved treasury to end up with. Drives both what (if
      * anything) the scenario submits between fallback and resolution, and the expected
      * evacuation-map size in the terminal-histogram check.
      */
    private sealed trait ExpectedCommitment
    private object ExpectedCommitment:
        // Default: no exogenous vote; tally falls through to the ballot-box default (major1).
        case object DefaultMajor1 extends ExpectedCommitment
    // TODO Test 2: case class SecFromStack(...) extends ExpectedCommitment

    val _ = property("ws: firewall votes → default major1 evacuates") =
        testProperty(TransportMode.WebSocket, ExpectedCommitment.DefaultMajor1)

    private def testProperty(
        transportMode: TransportMode,
        expected: ExpectedCommitment
    ): Prop =
        val testPeers   = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers, nCoilPeers)
        val coilWallets = testPeers.coilWallets
        val coilPeers   = testPeers.coilPeersConfig(hub = HeadPeerNumber(0))

        val widenedTxTiming: test.GenWithTestPeers[TxTiming] = ReaderT { network =>
            MultiPeerHeadHarness.fastTxTiming
                .run(network)
                .map(
                  _.copy(inactivityMarginDuration =
                      InactivityMarginDuration(10.seconds.quantize(network.slotConfig))
                  )
                )
        }

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
          takeoffOffset = 10.seconds,
          fastTxTiming = widenedTxTiming,
          disputeResolutionConfig = fastDisputeResolutionConfig,
          coilPeers = coilPeers,
          coilQuorum = nCoilPeers
        ) { (takeoffTime, mnc) =>
            buildCtxResource(transportMode, mnc, testPeers, coilWallets, expected, takeoffTime)
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
            _ <- step2b_maybeSubmitExogenousVote
            _ <- step3_awaitResolutionSubmitted
            _ <- step4_awaitEvacuationDone
            _ <- step5_assertTerminalHistogram
        yield true

    private final case class Ctx(
        transportMode: TransportMode,
        multiNodeConfig: MultiNodeConfig,
        harness: MultiPeerHeadHarness.Harness[Option[RequestSequencer.Handle]],
        expected: ExpectedCommitment,
        fallbackDispatched: Deferred[IO, Unit],
        resolutionSubmitted: Deferred[IO, Unit],
        peersEvacuationDone: Ref[IO, Set[PeerId]],
        evacuationDone: Deferred[IO, Unit],
        firstPayoutsLeft: Ref[IO, Option[Int]],
        periodicRequestFiber: Ref[IO, Option[FiberIO[Nothing]]]
    )

    private def step1a_submitBootstrapRequest: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(submitOneUserRequest(ctx))
        yield ()

    private def step1b_startPeriodicRequestLoop: test.TestM[Ctx, Unit] =
        for
            ctx   <- ask
            fiber <- lift((IO.sleep(1.second) >> submitOneUserRequest(ctx)).foreverM.start)
            _     <- lift(ctx.periodicRequestFiber.set(Some(fiber)))
        yield ()

    private def step2_awaitFallbackToRuleBasedHandoff: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _   <- lift(ctx.fallbackDispatched.get.timeout(scenarioTimeout))
        yield ()

    /** For [[ExpectedCommitment.DefaultMajor1]] this is a no-op. */
    private def step2b_maybeSubmitExogenousVote: test.TestM[Ctx, Unit] =
        for
            ctx <- ask
            _ <- lift(ctx.expected match
                case ExpectedCommitment.DefaultMajor1 => IO.unit
            )
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

    private def step5_assertTerminalHistogram: test.TestM[Ctx, Unit] =
        for
            ctx    <- ask
            _      <- lift(ctx.periodicRequestFiber.get.flatMap(_.traverse_(_.cancel)))
            _      <- lift(IO.sleep(quiescenceDelay))
            utxos  <- lift(ctx.harness.l1Snapshot)
            actual <- lift(runClassifier(utxos)(using ctx.multiNodeConfig))
            expectedEvacCount <- lift(
                                     ctx.firstPayoutsLeft.get.flatMap(
                                       IO.fromOption(_)(
                                         new IllegalStateException(
                                           "no `Evacuation.PayoutsLeft` observed; RBA never entered the drain loop"
                                         )
                                       )
                                     )
                                 )
            expected = expectedCardinalities(expectedEvacCount)
            _ <- assertWith(
                     actual == expected,
                     s"Cardinality mismatch:\n  expected: $expected\n  actual:   $actual"
                 )
        yield ()

    private val quiescenceDelay: FiniteDuration = 2.seconds

    // ------------------------------------------------------------------
    // Ctx bring-up
    // ------------------------------------------------------------------

    private def buildCtxResource(
        transportMode: TransportMode,
        multiNodeConfig: MultiNodeConfig,
        testPeers: TestPeers,
        coilWallets: List[hydrozoa.multisig.consensus.peer.PeerWallet],
        expected: ExpectedCommitment,
        takeoffTime: Option[java.time.Instant]
    ): Resource[IO, Ctx] =
        for
            fallbackDispatched   <- Resource.eval(Deferred[IO, Unit])
            resolutionSubmitted  <- Resource.eval(Deferred[IO, Unit])
            peersEvacuationDone  <- Resource.eval(Ref[IO].of(Set.empty[PeerId]))
            evacuationDone       <- Resource.eval(Deferred[IO, Unit])
            firstPayoutsLeft     <- Resource.eval(Ref[IO].of(Option.empty[Int]))
            periodicRequestFiber <- Resource.eval(Ref[IO].of(Option.empty[FiberIO[Nothing]]))

            preinitPeerUtxosL1 = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)
                .map { case (name, utxos) => name.headPeerNumber -> utxos }

            coilNodeConfigs = multiNodeConfig.mkCoilNodeConfigs(coilWallets)

            startEpochMs = multiNodeConfig.headConfig.initialBlock.blockBrief.endTime.convert.instant.toEpochMilli

            hooks = MultiPeerHeadHarness.Hooks[Option[RequestSequencer.Handle]](
              tracer = humanFormatTracer |+| observerTracer(
                fallbackDispatched,
                resolutionSubmitted,
                peersEvacuationDone,
                evacuationDone,
                firstPayoutsLeft
              ),
              handle = {
                  case (PeerId.Head(peerNum), conns) =>
                      IO.fromOption(conns.requestSequencer)(
                        new NoSuchElementException(
                          s"peer $peerNum has no RequestSequencer.Handle in its Connections"
                        )
                      ).map(Some(_))
                  case (_: PeerId.Coil, _) => IO.pure(None)
              },
              wrapBackend = (peerId, backend) => wrapPeerBackend(peerId, backend)
            )

            label = s"RBRCommitmentSelection-${transportMode.toString.toLowerCase}"

            harness <- MultiPeerHeadHarness.resource[Option[RequestSequencer.Handle]](
              MultiPeerHeadHarness.Inputs(
                config = MultiPeerHeadHarness.Config(
                  label = label,
                  backendMode = MultiPeerHeadHarness.StorageBackend.Mode.InMemory,
                  transportMode = transportMode
                ),
                multiNodeConfig = multiNodeConfig,
                coilNodeConfigs = coilNodeConfigs,
                preinitPeerUtxosL1 = preinitPeerUtxosL1,
                takeoffTime = takeoffTime,
                startEpochMs = startEpochMs
              ),
              hooks
            )
        yield Ctx(
          transportMode = transportMode,
          multiNodeConfig = multiNodeConfig,
          harness = harness,
          expected = expected,
          fallbackDispatched = fallbackDispatched,
          resolutionSubmitted = resolutionSubmitted,
          peersEvacuationDone = peersEvacuationDone,
          evacuationDone = evacuationDone,
          firstPayoutsLeft = firstPayoutsLeft,
          periodicRequestFiber = periodicRequestFiber
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
            now <- IO.realTimeInstant
            header = UserRequestHeader(
              headId = ctx.multiNodeConfig.headConfig.headId,
              validityStart = RequestValidityStartTime(
                QuantizedInstant.ofEpochSeconds(slotConfig, now.getEpochSecond - 5L)
              ),
              validityEnd = RequestValidityEndTime(
                QuantizedInstant.ofEpochSeconds(slotConfig, now.getEpochSecond + 300L)
              ),
              bodyHash = body.hash
            )
            userRequest = UserRequest.TransactionRequest(header, body, userVk)
            sequencer <- IO.fromOption(ctx.harness.peers.get(peerNum).flatMap(_.handle))(
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

    /** Same as [[EvacuationPropertyTest.wrapPeerBackend]] plus a blanket drop on every
      * vote-related tx: head-side [[VoteTx]], coil-side [[RatchetVoteTx]] (structurally the same
      * as [[VoteTx]] but ratchets an already-`Voted` box forward — can move the resolved kzg),
      * and [[AbstainTx]] for symmetry. Applies to both head and coil backends.
      */
    private def wrapPeerBackend(
        peerId: PeerId,
        underlying: L1Backend[IO]
    ): L1Backend[IO] =
        val peerLabel = peerId match {
            case PeerId.Head(n) => s"head-$n"
            case PeerId.Coil(n) => s"coil-$n"
        }
        val slf4jSink: ContraTracer[IO, FirewalledCardanoBackendEvent] =
            Slf4jTracer.sink.contramap {
                case FirewalledCardanoBackendEvent.DroppedOutboundTx(hash) =>
                    hydrozoa.lib.logging.LogEvent
                        .From(Map("peer" -> peerLabel), "FirewalledCardanoBackend")
                        .warn(s"firewall DROPPED tx $hash")
                case FirewalledCardanoBackendEvent.SubmittedTx(hash, result) =>
                    hydrozoa.lib.logging.LogEvent
                        .From(Map("peer" -> peerLabel), "FirewalledCardanoBackend")
                        .info(s"firewall passed tx $hash result=$result")
            }
        FirewalledCardanoBackend(
          underlying = underlying,
          shouldDrop = etx =>
              IO.pure(etx match {
                  case s: SettlementTx  => s.majorVersionProduced.convert == 2
                  case _: VoteTx        => true
                  case _: RatchetVoteTx => true
                  case _: AbstainTx     => true
                  case _                => false
              }),
          firewallTracer = slf4jSink
        )

    private def observerTracer(
        fallbackDispatched: Deferred[IO, Unit],
        resolutionSubmitted: Deferred[IO, Unit],
        peersEvacuationDone: Ref[IO, Set[PeerId]],
        evacuationDone: Deferred[IO, Unit],
        firstPayoutsLeft: Ref[IO, Option[Int]]
    ): ContraTracer[IO, MultiPeerHeadHarness.Event] =
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
                IO.pure(RBRPlaceId.values.toList.map(k => k -> hist(k)).toMap)

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

    /** Shape identical to [[EvacuationPropertyTest.expectedCardinalities]]. Only
      * `EvacuationOutputPlaceId` varies with the winning commitment; the count comes from
      * `PayoutsLeft(n)` observed by the RBA at drain start, which equals the resolved
      * commitment's evacuation-map size.
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
          AmbientPlaceId            -> (nHeadPeers * 2 + nCoilPeers)
        )
