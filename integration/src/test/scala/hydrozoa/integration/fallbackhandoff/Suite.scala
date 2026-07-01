package hydrozoa.integration.fallbackhandoff

import cats.effect.{IO, Resource}
import hydrozoa.integration.fallbackhandoff.Plugins.HandoffPlugins
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.StorageBackend.Mode as BackendMode
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import org.scalacheck.commands.{AnyCommand, ModelBasedSuite, ScenarioGen}
import org.scalacheck.{Gen, Prop, PropertyM}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** TDD scaffold for the multisig→rule-based fallback handoff.
  *
  * Scenario: spin up an N-peer head, run a configurable number of happy-path commands, then
  * firewall peer 1 (drop its outbound peer-transport sigs AND its outbound L1 tx submissions —
  * the two channels needed to deprive the head of progress entirely). After T_fallback both
  * CLs must dispatch `FallbackToRuleBased`; after T_handoff both HMRMs must report
  * `RuleBasedRegimeStarted` with their multisig children stopped.
  *
  * Imagined surface (`???` until implemented):
  *   - [[Firewall]] — CRUD-mutable rule set wired into PeerTransport + CardanoBackend filter
  *     hooks; commands mutate it via the interpreter.
  *   - `RuleBasedRegimeStartedDispatched` — new HMRM event the handoff implementation emits.
  *   - `MultisigChildrenStopped` — observable signal that MRM has torn down its children.
  */
final case class FallbackHandoffSuite(
    label: String = "fallback-handoff",
    nPeers: Int = 2,
    nHappyCommands: Int = 2,
    firewalledPeer: HeadPeerNumber = HeadPeerNumber(1),
    fallbackTimeout: FiniteDuration = 30.seconds,
    handoffTimeout: FiniteDuration = 30.seconds,
    transportMode: TransportMode = TransportMode.Direct,
    backendMode: BackendMode = BackendMode.InMemory,
) extends ModelBasedSuite:

    override type Env   = Unit
    override type State = ModelState
    override type Sut   = FallbackHandoffSut

    override def useTestControl: Boolean = transportMode match
        case TransportMode.Direct    => true
        case TransportMode.WebSocket => false

    override def scenarioGen: ScenarioGen[ModelState, FallbackHandoffSut] = ???

    override def commandGenTweaker: [A] => Gen[A] => Gen[A] = [A] =>
        (g: Gen[A]) => Gen.resize(nHappyCommands, g)

    override def initEnv: PropertyM[IO, Unit] = PropertyM.run(IO.unit)

    override def genInitialState(env: Unit): PropertyM[IO, ModelState] = ???

    override def canStartupNewSut(): Boolean = true

    override def sutResource(state: ModelState): Resource[IO, FallbackHandoffSut] =
        val multiNodeConfig = state.params.multiNodeConfig
        val peers           = multiNodeConfig.nodeConfigs.keys.toSeq.sortBy(p => p: Int)
        val startEpochMs    = state.currentModelTime.getEpochSecond * 1000L

        for
            // Per-peer fallback signal (CL-dispatched `FallbackToRuleBased`).
            fallbackByPeer <- Resource.eval(Plugins.fallbackEnteredByPeer(peers))
            // Per-peer "RBRM is up + multisig children stopped" signal.
            ruleBasedByPeer <- Resource.eval(Plugins.ruleBasedStartedByPeer(peers))
            // Empty firewall; the command interpreter issues add/remove/clear against it.
            firewall <- Resource.eval(Firewall.make)

            handoffPlugins = HandoffPlugins(fallbackByPeer, ruleBasedByPeer)

            hooks = MultiPeerHeadHarness.Hooks[FallbackHandoffPeerHandle, Unit](
                      tracer = handoffPlugins.tracer,
                      peerHandle = (_, conns) => IO.pure(FallbackHandoffPeerHandle(conns)),
                      coilHandle = (_, _) => IO.unit,
                    )

            harness <- MultiPeerHeadHarness.resource(
                           MultiPeerHeadHarness.Inputs(
                             config = MultiPeerHeadHarness
                                 .Config(label, backendMode, transportMode),
                             multiNodeConfig = multiNodeConfig,
                             coilNodeConfigs = Nil,
                             preinitPeerUtxosL1 = state.preinitPeerUtxosL1,
                             takeoffTime = state.takeoffTime,
                             startEpochMs = startEpochMs,
                           ),
                           hooks,
                       )

            // The firewall's predicates need to be threaded into every peer's PeerTransport +
            // the shared mock CardanoBackend — the missing hook surface on both sides is the
            // TDD target.
            _ <- Resource.eval(firewall.installInto(harness))
        yield FallbackHandoffSut(
          peers = harness.peers.map { case (n, p) => n -> p.handle },
          sutErrors = harness.sutErrors,
          fallbackByPeer = fallbackByPeer,
          ruleBasedByPeer = ruleBasedByPeer,
          firewall = firewall,
        )

    override def beforeFinalize(lastState: ModelState, sut: FallbackHandoffSut): IO[Prop] =
        // Sequence: (1) the scenario's happy-path commands already ran; (2) tests' final command
        // flips the firewall; (3) wait for fallback signals; (4) wait for rule-based-started.
        for
            // Every peer must dispatch FallbackToRuleBased within T_fallback.
            _ <- waitAllWithin(
                     sut.fallbackByPeer.values.toList.map(_.await),
                     fallbackTimeout,
                     "fallback",
                 )
            // Then every peer must surface RuleBasedRegimeStarted within T_handoff.
            _ <- waitAllWithin(
                     sut.ruleBasedByPeer.values.toList.map(_.await),
                     handoffTimeout,
                     "rule-based start",
                 )
            errors <- sut.sutErrors.get
        yield
            if errors.nonEmpty then
                Prop.exception(RuntimeException(s"SUT actor errors:\n${errors.mkString("\n")}"))
            else Prop.passed

    /** Run all `waits` concurrently; fail if any doesn't complete inside `timeout`. */
    private def waitAllWithin(
        waits: List[IO[?]],
        timeout: FiniteDuration,
        label: String,
    ): IO[Unit] = ???
