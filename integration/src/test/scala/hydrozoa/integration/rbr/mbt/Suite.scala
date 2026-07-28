package hydrozoa.integration.rbr.mbt

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet
import hydrozoa.integration.rbr.property.ObservableMarking
import hydrozoa.integration.stage4.Model
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.{FirewalledCardanoBackend, yaciTestSauceGenesis}
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
import scalus.cardano.ledger.{TransactionInput, Utxos}
import test.{SeedPhrase, TestPeers}

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
) extends ModelBasedSuite:

    override type Env = Unit
    override type State = Model.ModelState
    override type Sut = hydrozoa.integration.rbr.mbt.Sut

    private val cardanoNetwork: CardanoNetwork = CardanoNetwork.Preprod
    private val scenarioTimeout: FiniteDuration = 5.minutes
    private val quiescenceDelay: FiniteDuration = 2.seconds

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("RbrMbt.Suite"))

    override def useTestControl: Boolean = false

    override def scenarioGen: ScenarioGen[Model.ModelState, Sut] = RbrMbtScenarioGen

    override def commandGenTweaker: [A] => Gen[A] => Gen[A] =
        [A] => (g: Gen[A]) => Gen.resize(nCommands, g)

    override def initEnv: PropertyM[IO, Unit] = PropertyM.run(IO.unit)

    override def canStartupNewSut(): Boolean = true

    private val testPeers: TestPeers =
        TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers, nCoilPeers)

    override def genInitialState(env: Unit): PropertyM[IO, Model.ModelState] =
        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)
        MultiPeerHeadHarness
            .genDisputeMnc(
              transportMode = TransportMode.WebSocket,
              testPeers = testPeers,
              testPeerToUtxos = testPeerToUtxos,
              takeoffOffset = 60.seconds,
              coilPeers = testPeers.coilPeersConfig(hub = HeadPeerNumber(0)),
              coilQuorum = nCoilPeers,
            )
            .map { case (takeoffTime, mnc) => mkModelState(mnc, takeoffTime) }

    /** Build the stage4 `ModelState` from a generated dispute `MultiNodeConfig` (mirrors
      * `Stage4Suite.genInitialState`'s post-config construction).
      */
    private def mkModelState(
        config: MultiNodeConfig,
        takeoffTime: Option[java.time.Instant],
    ): Model.ModelState =
        val preinitPeerUtxosL1 =
            yaciTestSauceGenesis(cardanoNetwork.network)(testPeers).map { case (k, v) =>
                k.headPeerNumber -> v
            }
        val coilNodeConfigs = config.mkCoilNodeConfigs(testPeers.coilWallets)
        val initTx = config.headConfig.initializationTx.tx
        val spentInputs = initTx.body.value.inputs.toSet
        val initOutputsList = initTx.body.value.outputs.toList.map(_.value).zipWithIndex
        val peers = config.nodeConfigs.keys.toSeq.sortBy(p => p: Int)
        val peerUtxosL1 = peers.map { pn =>
            val peerAddr = config.addressOf(pn)
            val survived: Utxos = preinitPeerUtxosL1(pn) -- spentInputs
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
            harness <- MultiPeerHeadHarness.disputeHarnessResource(
              label = s"$label-ws",
              transportMode = TransportMode.WebSocket,
              multiNodeConfig = state.params.multiNodeConfig,
              testPeers = testPeers,
              takeoffTime = state.takeoffTime,
              tracer = MultiPeerHeadHarness.humanFormatTracer(nHeadPeers) |+|
                  observerTracer(
                    fallbackDispatched,
                    evacuationDone,
                    peersEvacuationDone,
                    firstPayoutsLeft,
                  ),
              // Dynamic gate: drop settlements only once armed (in beforeFinalize, after the
              // generated deposits have settled on-chain) — so their majors commit before fallback.
              wrapBackend = (peerId, backend) =>
                  FirewalledCardanoBackend(
                    underlying = backend,
                    shouldDrop = (etx: EnrichedTx[?]) =>
                        settlementFirewallArmed.get
                            .map(_ && etx.transactionFamily == "SettlementTx"),
                    firewallTracer = MultiPeerHeadHarness.firewallSlf4jSink(peerId),
                  ),
            )
            // Force block/major production so deposits settle and (post-arming) fallback trips.
            _ <- Resource.make(
              (MultiPeerHeadHarness.submitKickRequest(harness) >> IO.sleep(1.second)).foreverM.start
            )(_.cancel)
        yield Sut(
          harness,
          fallbackDispatched,
          evacuationDone,
          firstPayoutsLeft,
          settlementFirewallArmed,
        )

    override def beforeFinalize(lastState: Model.ModelState, sut: Sut): IO[Prop] =
        val depositUtxos = lastState.registeredDeposits.values.map(_.depositProduced).toSet
        for
            _ <- log.info(s"beforeFinalize: awaiting ${depositUtxos.size} deposit(s) to commit")
            // 1. Wait until every deposit L1 utxo is consumed by a settlement (committed on-chain).
            _ <- awaitDepositsCommitted(sut, depositUtxos).timeout(scenarioTimeout)
            // 2. Arm the firewall → the next settlement is dropped → fallback.
            _ <- sut.settlementFirewallArmed.set(true)
            _ <- sut.fallbackDispatched.get.timeout(scenarioTimeout)
            _ <- sut.evacuationDone.get.timeout(scenarioTimeout)
            _ <- IO.sleep(quiescenceDelay)
            utxos <- sut.harness.l1Snapshot
            payouts <- sut.firstPayoutsLeft.get
            errors <- sut.harness.sutErrors.get
            obligationCount = committedObligationCount(lastState)
            alpha = alphaTerminal(obligationCount)
            betaEither = ObservableMarking.beta(utxos)(using sut.harness.multiNodeConfig)
            _ <- log.info(
              s"beforeFinalize: firstPayoutsLeft=$payouts obligationCount=$obligationCount\n" +
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

    /** Poll the shared L1 until none of the deposit utxos remain — each was consumed by the
      * settlement that absorbed it into a committed major.
      */
    private def awaitDepositsCommitted(sut: Sut, depositUtxos: Set[TransactionInput]): IO[Unit] =
        def loop: IO[Unit] =
            sut.harness.l1Snapshot.flatMap { utxos =>
                if depositUtxos.intersect(utxos.keySet).isEmpty then IO.unit
                else IO.sleep(500.millis) >> loop
            }
        if depositUtxos.isEmpty then IO.unit else loop

    /** The committed obligations the head will evacuate: the initial evacuation map plus the L2
      * outputs of every registered deposit (all committed by the commit-wait above).
      */
    private def committedObligationCount(state: Model.ModelState): Int =
        val initial =
            state.params.multiNodeConfig.headConfig.initializationParameters.initialEvacuationMap.size
        val deposited = state.registeredDeposits.values.map(depositOutputCount).sum
        initial + deposited

    private def depositOutputCount(pd: Model.PendingDeposit): Int =
        Cbor.decode(pd.l2Payload.bytes).to[Queue[GenesisObligation]].value.size

    /** The model's all-evacuated terminal projection: instantiate `RBRHlNet` seeded with
      * `obligationCount` committed outputs and drive it through the dispute to full evacuation.
      */
    private def alphaTerminal(obligationCount: Int): ObservableMarking =
        val seed = RBRHlNet(
          nHeadPeers,
          maxVersionMinor,
          _ => RBRHlNet.committedOutputs(obligationCount),
        ).toOption.get
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
