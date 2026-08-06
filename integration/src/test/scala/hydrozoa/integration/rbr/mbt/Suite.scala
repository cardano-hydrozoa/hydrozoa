package hydrozoa.integration.rbr.mbt

import cats.data.Validated
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.harness.MultiPeerHeadHarness.CardanoBackend as HarnessCardanoBackend
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.integration.harness.{DiagnosticTracers, MultiPeerHeadHarness}
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet
import hydrozoa.integration.rbr.property.{ObservableMarking, RbrSeed}
import hydrozoa.integration.stage4.Model
import hydrozoa.integration.yaci.{DevKit, YaciDevnet, YaciSetup}
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.{FirewalledCardanoBackend, FirewalledCardanoBackendEvent, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.CardanoLiaisonEvent
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.eutxol2.toUtxos
import hydrozoa.multisig.ledger.eutxol2.tx.{GenesisObligation, genesisObligationDecoder}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import hydrozoa.multisig.{CommonChildEvent, RuleBasedOnlyChildEvent}
import hydrozoa.rulebased.RuleBasedActorEvent
import io.bullet.borer.Cbor
import org.scalacheck.commands.{ModelBasedSuite, ScenarioGen}
import org.scalacheck.{Gen, Prop, PropertyM}
import scala.collection.immutable.Queue
import scala.concurrent.duration.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{TransactionInput, TransactionOutput, Utxos}
import scalus.testing.yaci.YaciConfig
import test.{SeedPhrase, TestPeerName, TestPeers}

/** RBR fallback→evacuation as a `ModelBasedSuite` (stage4-style).
  *
  * The generator submits pre-fallback L1 deposits (reusing stage4's model + deposit generator); the
  * dynamic settlement firewall stays disarmed while those deposits settle on-chain, then
  * `beforeFinalize` arms it to trip fallback. The SUT's autonomous `RuleBasedActor` runs
  * dispute→evacuation, and the terminal L1 snapshot must project to the same `ObservableMarking` as
  * the model net seeded with `initial + Σ committed-deposit outputs` obligations — the "autonomous
  * match". WebSocket / real clock only.
  */
case class RbrMbtSuite(
    label: String = "rbr-mbt",
    nHeadPeers: Int = 3,
    nCoilPeers: Int = 2,
    maxVersionMinor: Int = 2,
    nCommands: Int = 4,
    /** L1 backend for the run. `Mock` uses the in-memory `CardanoBackendMock` with fabricated
      * `yaciTestSauceGenesis` UTxOs. `Yaci` acquires a shared Testcontainers-managed devnet, resets
      * + redeploys per iteration, and runs the harness against real Blockfrost.
      */
    backendSpec: RbrMbtSuite.BackendSpec = RbrMbtSuite.BackendSpec.Mock,
) extends ModelBasedSuite:

    override type Env = RbrMbtSuite.RbrMbtEnv
    override type State = Model.ModelState
    override type Sut = hydrozoa.integration.rbr.mbt.Sut

    private val scenarioTimeout: FiniteDuration = 7.minutes
    private val quiescenceDelay: FiniteDuration = 2.seconds

    /** How long `beforeFinalize` waits, best-effort, for the generated deposits to commit before
      * arming the firewall. Long enough to clear deposit maturity + one settlement so the common
      * path exercises deposit evacuation; on expiry we arm anyway and the stragglers take the
      * refund path (the committed-set computation keeps the model in agreement either way).
      */
    private val depositCommitWindow: FiniteDuration = 90.seconds

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("RbrMbt.Suite"))

    override def useTestControl: Boolean = false

    override def scenarioGen: ScenarioGen[Model.ModelState, Sut] = RbrMbtScenarioGen

    override def commandGenTweaker: [A] => Gen[A] => Gen[A] =
        [A] => (g: Gen[A]) => Gen.resize(nCommands, g)

    override def initEnv: PropertyM[IO, RbrMbtSuite.RbrMbtEnv] = backendSpec match {
        case RbrMbtSuite.BackendSpec.Mock =>
            PropertyM.run(IO.pure(RbrMbtSuite.RbrMbtEnv.Mock))
        case RbrMbtSuite.BackendSpec.Yaci(cfg) =>
            // Singleton container: first call warms it, every subsequent iteration reuses.
            PropertyM.run(YaciDevnet.acquireShared(cfg).map(RbrMbtSuite.RbrMbtEnv.Yaci.apply))
    }

    override def canStartupNewSut(): Boolean = true

    override def genInitialState(env: RbrMbtSuite.RbrMbtEnv): PropertyM[IO, Model.ModelState] =
        env match {
            case RbrMbtSuite.RbrMbtEnv.Mock =>
                val network = CardanoNetwork.Preprod
                val testPeers = TestPeers(SeedPhrase.Yaci, network, nHeadPeers, nCoilPeers)
                val testPeerToUtxos = yaciTestSauceGenesis(network.network)(testPeers)
                MultiPeerHeadHarness
                    .genDisputeMnc(
                      transportMode = TransportMode.WebSocket,
                      testPeers = testPeers,
                      testPeerToUtxos = testPeerToUtxos,
                      takeoffOffset = 120.seconds,
                      coilPeers = testPeers.coilPeersConfig(hub = HeadPeerNumber(0)),
                      coilQuorum = nCoilPeers,
                    )
                    .map { case (takeoffTime, mnc) =>
                        mkModelState(
                          config = mnc,
                          takeoffTime = takeoffTime,
                          testPeers = testPeers,
                          testPeerToUtxos = testPeerToUtxos,
                          cardanoBackendMode = HarnessCardanoBackend.Mode.Mock,
                        )
                    }

            case RbrMbtSuite.RbrMbtEnv.Yaci(devKit) =>
                for {
                    _ <- PropertyM.run(log.info("Deploying scripts on the Yaci devnet"))
                    ready <- PropertyM.run(YaciSetup.prepare(devKit, nHeadPeers, nCoilPeers))
                    takeoffAndMnc <- MultiPeerHeadHarness.genDisputeMnc(
                      transportMode = TransportMode.WebSocket,
                      testPeers = ready.testPeers,
                      testPeerToUtxos = ready.genesisByPeer,
                      takeoffOffset = 10.seconds,
                      // Longer deposit maturity so the CardanoLiaison poll can be slowed to 1s
                      // (see yaciTxTiming) — the real yaci-store can't take the fast mock cadence.
                      fastTxTiming = MultiPeerHeadHarness.yaciTxTiming,
                      scriptReferenceUtxos = Some(ready.scriptReferenceUtxos),
                      coilPeers = ready.testPeers.coilPeersConfig(hub = HeadPeerNumber(0)),
                      coilQuorum = nCoilPeers,
                    )
                    (takeoffTime, mnc) = takeoffAndMnc
                } yield mkModelState(
                  config = mnc,
                  takeoffTime = takeoffTime,
                  testPeers = ready.testPeers,
                  testPeerToUtxos = ready.genesisByPeer,
                  cardanoBackendMode = HarnessCardanoBackend.Mode
                      .Yaci(ready.network, devKit.blockfrostApiBaseUri),
                )
        }

    /** Build the stage4 `ModelState` from a generated dispute `MultiNodeConfig` (mirrors
      * `Stage4Suite.genInitialState`'s post-config construction). `testPeers` and `testPeerToUtxos`
      * come from the caller (they differ between the Mock and Yaci backend paths — Mock fabricates
      * from `yaciTestSauceGenesis`, Yaci queries the real devnet).
      */
    private def mkModelState(
        config: MultiNodeConfig,
        takeoffTime: Option[java.time.Instant],
        testPeers: TestPeers,
        testPeerToUtxos: Map[TestPeerName, Utxos],
        cardanoBackendMode: HarnessCardanoBackend.Mode,
    ): Model.ModelState =
        val preinitPeerUtxosL1 = testPeerToUtxos.map { case (k, v) => k.headPeerNumber -> v }
        val coilNodeConfigs = config.mkCoilNodeConfigs(testPeers.coilWallets)
        val initTx = config.headConfig.initializationTx.tx
        val spentInputs = initTx.body.value.inputs.toSet
        val initOutputsList = initTx.body.value.outputs.toList.map(_.value).zipWithIndex
        val peers = config.nodeConfigs.keys.toSeq.sortBy(p => p: Int)
        val peerUtxosL1 = peers.map { pn =>
            val peerAddr = config.addressOf(pn)
            val survived: Utxos = preinitPeerUtxosL1.getOrElse(pn, Map.empty) -- spentInputs
            val newOutputs: Utxos = initOutputsList
                .filter((out, _) => out.address.asInstanceOf[ShelleyAddress] == peerAddr)
                .map((out, ix) => TransactionInput(initTx.id, ix) -> out)
                .toMap
            pn -> (survived ++ newOutputs)
        }.toMap
        Model.ModelState(
          params = Model.Params(
            multiNodeConfig = config,
            absorptionSlack = 60.seconds,
            meanInterArrivalTimes = peers.map(pn => pn -> 12.seconds).toMap,
            testPeers = testPeers,
            cardanoBackendMode = cardanoBackendMode,
            coilNodeConfigs = coilNodeConfigs,
          ),
          preinitPeerUtxosL1 = preinitPeerUtxosL1,
          currentModelTime = config.headConfig.initialBlock.blockBrief.endTime.convert,
          takeoffTime = takeoffTime,
          utxosL2Active = config.headConfig.initializationParameters.initialEvacuationMap.toUtxos,
          peerUtxosL1 = peerUtxosL1,
          nextRequestNumbers = peers.map(_ -> RequestNumber(0)).toMap,
          pendingDeposits = peers.map(_ -> Nil).toMap,
          modelFlags = Map.empty,
          registeredDeposits = Map.empty,
        )

    override def sutResource(state: Model.ModelState): Resource[IO, Sut] =
        for
            fallbackDispatched <- Resource.eval(Deferred[IO, Unit])
            evacuationDone <- Resource.eval(Deferred[IO, Unit])
            firstPayoutsLeft <- Resource.eval(Ref[IO].of(Option.empty[Int]))
            settlementFirewallArmed <- Resource.eval(Ref[IO].of(false))
            peersEvacuationDone <- Resource.eval(Ref[IO].of(Set.empty[PeerId]))
            submittedSettlementInputs <- Resource.eval(Ref[IO].of(Set.empty[TransactionInput]))
            harness <- MultiPeerHeadHarness.disputeHarnessResource(
              label = s"$label-ws",
              transportMode = TransportMode.WebSocket,
              multiNodeConfig = state.params.multiNodeConfig,
              testPeers = state.params.testPeers,
              takeoffTime = state.takeoffTime,
              tracer = MultiPeerHeadHarness.humanFormatTracer(nHeadPeers) |+|
                  // Reusable test-side diagnostic tracer (composed, not baked into production
                  // formatting) that surfaces the RBA's candidate-map / resolved-kzg detail.
                  DiagnosticTracers.rbrDiagnostics |+|
                  observerTracer(
                    fallbackDispatched,
                    evacuationDone,
                    peersEvacuationDone,
                    firstPayoutsLeft,
                  ),
              // Dynamic gate: drop settlements only once armed (in beforeFinalize, once the
              // generated deposits are in a determinate on-chain state) — tripping fallback.
              wrapBackend = (peerId, backend) =>
                  FirewalledCardanoBackend(
                    underlying = backend,
                    shouldDrop = (etx: EnrichedTx[?]) =>
                        settlementFirewallArmed.get
                            .map(_ && etx.transactionFamily == "SettlementTx"),
                    // Record the inputs of every settlement that clears the firewall, so the
                    // committed-deposit set can be derived at fallback (see [[Sut]]).
                    firewallTracer = MultiPeerHeadHarness.firewallSlf4jSink(peerId) |+|
                        settlementAbsorptionObserver(submittedSettlementInputs),
                  ),
              cardanoBackendMode = state.params.cardanoBackendMode,
              // Evacuation payouts land at `RbrSeed.payoutAddress`, which is neither a peer wallet
              // nor a head script address — so the Yaci `l1Snapshot` would otherwise miss every
              // evacuation output and `beta` would undercount. (The mock backend sees the whole
              // ledger and ignores this.)
              extraSnapshotAddresses = List(RbrSeed.payoutAddress),
            )
        yield Sut(
          harness,
          fallbackDispatched,
          evacuationDone,
          firstPayoutsLeft,
          settlementFirewallArmed,
          submittedSettlementInputs,
        )

    /** Firewall observer that records the L1 inputs of every settlement the SUT actually submits
      * (i.e. clears the firewall and reaches the backend). Crossed against the L1 snapshot at
      * fallback, `submittedSettlementInputs` distinguishes a deposit genuinely absorbed by a landed
      * settlement from one whose deposit tx simply hasn't surfaced yet — the distinction the old
      * "utxo absent from the snapshot" check could not make.
      */
    private def settlementAbsorptionObserver(
        submittedSettlementInputs: Ref[IO, Set[TransactionInput]]
    ): ContraTracer[IO, FirewalledCardanoBackendEvent] =
        ContraTracer[IO, FirewalledCardanoBackendEvent] {
            case FirewalledCardanoBackendEvent.SubmittedTx(etx, _)
                if etx.transactionFamily == "SettlementTx" =>
                submittedSettlementInputs.update(_ ++ etx.tx.body.value.inputs.toSet)
            case _ => IO.unit
        }

    // TODO: assert the refund path explicitly. Deposits still pending at fallback (not absorbed
    // into a committed major) are excluded from `alpha` here and left to the cardano-liaison refund
    // path, which this suite exercises but does not verify. Deliberately inject racing (arm the
    // firewall while deposits are in flight) and assert each pending deposit's value reappears at
    // the originating peer's L1 address — likely by stamping deposits with a refund sentinel datum
    // so `beta` can bucket refunds directly rather than inferring them from addresses.
    override def beforeFinalize(lastState: Model.ModelState, sut: Sut): IO[Prop] =
        val depositUtxos = lastState.registeredDeposits.values.map(_.depositProduced).toSet
        for
            _ <- log.info(
              s"beforeFinalize: awaiting up to $depositCommitWindow for " +
                  s"${depositUtxos.size} deposit(s) to commit"
            )
            // 1. Best-effort: wait for the deposits to commit (so the common path evacuates them),
            //    but arm anyway once the window elapses — stragglers then take the refund path.
            _ <- awaitDepositsCommitted(sut, depositUtxos).timeoutTo(
              depositCommitWindow,
              log.info("beforeFinalize: commit window elapsed; arming with deposit(s) pending"),
            )
            // 2. Arm the firewall → the next settlement is dropped → fallback.
            _ <- sut.settlementFirewallArmed.set(true)
            _ <- sut.fallbackDispatched.get.timeout(scenarioTimeout)
            // 3. Snapshot at fallback (before refunds/evacuation move things) to fix the committed
            //    set: a deposit is committed iff a submitted settlement spent it AND it is now gone.
            fallbackUtxos <- sut.harness.l1Snapshot
            submittedInputs <- sut.submittedSettlementInputs.get
            _ <- sut.evacuationDone.get.timeout(scenarioTimeout)
            _ <- IO.sleep(quiescenceDelay)
            utxos <- sut.harness.l1Snapshot
            payouts <- sut.firstPayoutsLeft.get
            errors <- sut.harness.sutErrors.get
            (initialObligations, depositedObligations) =
                obligationBreakdown(lastState, submittedInputs, fallbackUtxos)
            obligationCount = initialObligations + depositedObligations
            alpha = alphaTerminal(obligationCount)
            betaEither = ObservableMarking.beta(utxos)(using sut.harness.multiNodeConfig)
            _ <- log.info(
              s"beforeFinalize: firstPayoutsLeft=$payouts obligationCount=$obligationCount " +
                  s"(initial=$initialObligations committed-deposits=$depositedObligations " +
                  s"of ${depositUtxos.size} deposit(s))\n" +
                  s"  alpha (model): $alpha\n  beta  (L1):    $betaEither"
            )
        yield
            if errors.nonEmpty then
                Prop.exception(RuntimeException(s"SUT actor errors:\n${errors.mkString("\n")}"))
            else
                betaEither match
                    case Left(msg) => Prop.falsified :| s"beta projection failed: $msg"
                    case Right(beta) =>
                        Prop(alpha == beta) :|
                            s"autonomous match failed (SUT RBA saw $payouts payouts):\n" +
                            s"  alpha (model): $alpha\n  beta  (L1):    $beta"

    /** Poll the shared L1 until every deposit is committed: a settlement the SUT actually submitted
      * spent its utxo (`submittedSettlementInputs`) AND that utxo is gone from L1 (the settlement
      * landed). Requiring both — rather than "utxo absent from the snapshot" — avoids mistaking a
      * deposit that simply hasn't surfaced on L1 yet for a committed one. The caller bounds this
      * with `depositCommitWindow` and arms anyway on expiry, so an un-committed straggler is left
      * to the refund path rather than hanging the test.
      */
    private def awaitDepositsCommitted(sut: Sut, depositUtxos: Set[TransactionInput]): IO[Unit] =
        def loop: IO[Unit] =
            for
                utxos <- sut.harness.l1Snapshot
                submittedInputs <- sut.submittedSettlementInputs.get
                committed = depositUtxos.forall(d =>
                    submittedInputs.contains(d) && !utxos.contains(d)
                )
                _ <- IO.unlessA(committed)(IO.sleep(500.millis) >> loop)
            yield ()
        if depositUtxos.isEmpty then IO.unit else loop

    /** `(initial-map obligations, committed-deposit obligations)`. The initial evacuation map is
      * the genesis L2 seed (always evacuated). A registered deposit contributes its obligations
      * only if it was committed by fallback: a settlement the SUT submitted spent its L1 utxo
      * (`submittedInputs`) AND that utxo is gone from the fallback snapshot (the settlement
      * landed). Deposits failing either test were left pending and are refunded, not evacuated.
      */
    private def obligationBreakdown(
        state: Model.ModelState,
        submittedInputs: Set[TransactionInput],
        fallbackUtxos: Utxos,
    ): (Int, Int) =
        val initial =
            state.params.multiNodeConfig.headConfig.initializationParameters.initialEvacuationMap.size
        val committed = state.registeredDeposits.values.filter(pd =>
            submittedInputs.contains(pd.depositProduced) &&
                !fallbackUtxos.contains(pd.depositProduced)
        )
        val deposited = committed.map(depositOutputCount).sum
        (initial, deposited)

    private def depositOutputCount(pd: Model.PendingDeposit): Int =
        Cbor.decode(pd.l2Payload.bytes).to[Queue[GenesisObligation]].value.size

    /** The model's all-evacuated terminal projection: instantiate `RBRHlNet` seeded with
      * `obligationCount` committed outputs and drive it through the dispute to full evacuation.
      */
    private def alphaTerminal(obligationCount: Int): ObservableMarking =
        val obligations: Map[BigInt, List[TransactionOutput]] =
            (1 to maxVersionMinor)
                .map(v => BigInt(v) -> RbrSeed.committedOutputs(obligationCount))
                .toMap
        val seed = RBRHlNet(nHeadPeers, obligations) match
            case Validated.Valid(net) => net
            case Validated.Invalid(errs) =>
                throw RuntimeException(
                  s"RBRHlNet failed (nHeadPeers=$nHeadPeers, obligationCount=$obligationCount, " +
                      s"versions=${obligations.keySet}): ${errs.toList.mkString("; ")}"
                )
        ObservableMarking.alpha(NetDriver.driveToEvacuated(seed))

    /** Completes `fallbackDispatched` on the first `FallbackToRuleBasedDispatched`, records the
      * first `Evacuation.PayoutsLeft`, and completes `evacuationDone` once every head + coil peer
      * has fired `Evacuation.NoMore`.
      */
    private def observerTracer(
        fallbackDispatched: Deferred[IO, Unit],
        evacuationDone: Deferred[IO, Unit],
        peersEvacuationDone: Ref[IO, Set[PeerId]],
        firstPayoutsLeft: Ref[IO, Option[Int]],
    ): ContraTracer[IO, MultiPeerHeadHarness.Event] =
        val onEvent: PeerId => Any => IO[Unit] = peer => {
            case CommonChildEvent.CardanoLiaison(
                  _: CardanoLiaisonEvent.FallbackToRuleBasedDispatched
                ) =>
                fallbackDispatched.complete(()).void

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
            case MultiPeerHeadHarness.Event.Head(peerNum, evt) => onEvent(PeerId.Head(peerNum))(evt)
            case MultiPeerHeadHarness.Event.Coil(coilNum, evt) => onEvent(PeerId.Coil(coilNum))(evt)
        }

object RbrMbtSuite:

    /** L1 backend selector. `Mock` runs in-memory. `Yaci` runs against a real
      * Testcontainers-managed devnet — one JVM-wide container (see [[YaciDevnet.acquireShared]]),
      * reset + redeployed per ScalaCheck iteration. The `YaciConfig` selects the container's config
      * (image tag, log enable, container reuse) — see `scalus.testing.yaci.YaciConfig`.
      */
    enum BackendSpec:
        case Mock
        case Yaci(config: YaciConfig = YaciConfig())

    /** Per-iteration environment threaded from [[RbrMbtSuite.initEnv]] to
      * [[RbrMbtSuite.genInitialState]]. `Mock` carries no state (the suite reconstructs `TestPeers`
      * from the fixed Preprod network). `Yaci` carries the shared devnet handle so the iteration
      * can `reset()` it and run [[YaciSetup.prepare]] before generating the config.
      */
    enum RbrMbtEnv:
        case Mock
        case Yaci(devKit: DevKit)
