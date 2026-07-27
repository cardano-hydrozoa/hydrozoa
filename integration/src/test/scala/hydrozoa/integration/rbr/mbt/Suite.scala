package hydrozoa.integration.rbr.mbt

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet
import hydrozoa.integration.rbr.property.ObservableMarking
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.{FirewalledCardanoBackend, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, RequestSequencer}
import hydrozoa.multisig.{CommonChildEvent, RuleBasedOnlyChildEvent}
import hydrozoa.rulebased.RuleBasedActorEvent
import org.scalacheck.commands.{ModelBasedSuite, ScenarioGen}
import org.scalacheck.{Gen, Prop, PropertyM}
import scala.concurrent.duration.*
import test.{SeedPhrase, TestPeers}

/** RBR fallback→evacuation as a `ModelBasedSuite` (stage4-style).
  *
  * A trivial generator produces pre-fallback load (deposits/L2 txs — not yet); the settlement
  * firewall (`settlementProducingMajor`) trips fallback; the SUT's autonomous `RuleBasedActor` runs
  * dispute→evacuation; and `beforeFinalize` asserts the terminal L1 snapshot projects to the same
  * `ObservableMarking` as the model net driven to all-evacuated — the "autonomous match".
  *
  * WebSocket / real clock only: virtual time cannot drive the OS scheduler behind the WS mesh, and
  * the RBA/CL polling is wall-clock driven.
  */
case class RbrMbtSuite(
    label: String = "rbr-mbt",
    nHeadPeers: Int = 3,
    nCoilPeers: Int = 2,
    maxVersionMinor: Int = 2,
    settlementFirewallMajor: Int = 2,
    nCommands: Int = 3,
) extends ModelBasedSuite:

    override type Env = Unit
    override type State = ModelState
    override type Sut = hydrozoa.integration.rbr.mbt.Sut

    private val cardanoNetwork: CardanoNetwork     = CardanoNetwork.Preprod
    private val scenarioTimeout: FiniteDuration    = 5.minutes
    private val quiescenceDelay: FiniteDuration    = 2.seconds

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("RbrMbt.Suite"))

    override def useTestControl: Boolean = false

    override def scenarioGen: ScenarioGen[ModelState, Sut] = RbrMbtScenarioGen

    override def commandGenTweaker: [A] => Gen[A] => Gen[A] =
        [A] => (g: Gen[A]) => Gen.resize(nCommands, g)

    override def initEnv: PropertyM[IO, Unit] = PropertyM.run(IO.unit)

    override def canStartupNewSut(): Boolean = true

    private val testPeers: TestPeers =
        TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nHeadPeers, nCoilPeers)

    override def genInitialState(env: Unit): PropertyM[IO, ModelState] =
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
            .map { case (takeoffTime, mnc) =>
                ModelState(mnc, takeoffTime, testPeers, nHeadPeers, nCoilPeers, maxVersionMinor)
            }

    override def sutResource(state: ModelState): Resource[IO, Sut] =
        for
            fallbackDispatched  <- Resource.eval(Deferred[IO, Unit])
            evacuationDone      <- Resource.eval(Deferred[IO, Unit])
            firstPayoutsLeft    <- Resource.eval(Ref[IO].of(Option.empty[Int]))
            peersEvacuationDone <- Resource.eval(Ref[IO].of(Set.empty[PeerId]))
            harness <- MultiPeerHeadHarness.disputeHarnessResource(
              label = s"$label-ws",
              transportMode = TransportMode.WebSocket,
              multiNodeConfig = state.multiNodeConfig,
              testPeers = state.testPeers,
              takeoffTime = state.takeoffTime,
              tracer = MultiPeerHeadHarness.humanFormatTracer(nHeadPeers) |+|
                  observerTracer(
                    fallbackDispatched,
                    evacuationDone,
                    peersEvacuationDone,
                    firstPayoutsLeft,
                  ),
              wrapBackend = (peerId, backend) =>
                  FirewalledCardanoBackend(
                    underlying = backend,
                    shouldDrop = MultiPeerHeadHarness.DropRule
                        .settlementProducingMajor(settlementFirewallMajor)
                        .toGate,
                    firewallTracer = MultiPeerHeadHarness.firewallSlf4jSink(peerId),
                  ),
            )
            // Force major blocks so the head advances to the firewalled major and trips fallback.
            // The loop is cancelled when the Resource releases (after beforeFinalize).
            _ <- Resource.make(
              (MultiPeerHeadHarness.submitKickRequest(harness) >> IO.sleep(1.second)).foreverM.start
            )(_.cancel)
        yield Sut(harness, fallbackDispatched, evacuationDone, firstPayoutsLeft)

    override def beforeFinalize(lastState: ModelState, sut: Sut): IO[Prop] =
        for
            _       <- log.info("beforeFinalize: awaiting fallback + autonomous evacuation")
            _       <- sut.fallbackDispatched.get.timeout(scenarioTimeout)
            _       <- sut.evacuationDone.get.timeout(scenarioTimeout)
            _       <- IO.sleep(quiescenceDelay)
            utxos   <- sut.harness.l1Snapshot
            payouts <- sut.firstPayoutsLeft.get
            errors  <- sut.harness.sutErrors.get
            // Instantiate the net seeded with the committed-obligation count the head evacuated (for
            // now, with no pre-fallback load, that is the initial evacuation-map size) and drive it
            // to the all-evacuated terminal — the "autonomous match".
            obligationCount = lastState.multiNodeConfig.headConfig.initialEvacuationMap.size
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
                            s"autonomous match failed:\n  alpha (model): $alpha\n  beta  (L1):    $beta"

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

    /** Completes `fallbackDispatched` on the first `FallbackToRuleBasedDispatched`, records the first
      * `Evacuation.PayoutsLeft`, and completes `evacuationDone` once every head + coil peer has fired
      * `Evacuation.NoMore`.
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
