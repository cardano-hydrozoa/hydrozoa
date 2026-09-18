package hydrozoa.integration.harness

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.integration.stage4.Stage4Suite
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.RequestSequencer
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.persistence.{Markers, StoreKey}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*

/** Wipe a coil peer's store and let it come back — the failure GUM-312 exists to close.
  *
  * Seen live twice: a coil crash-looped after its volume was recreated, and again when an operator
  * removed a coil database and restarted. The store reads as cold, the coil re-derives stack 0 from
  * config, and the head is long past it; the hub then asks for hard-acks the coil cannot produce
  * and the link dead-ends at
  * `OwnHardAck.Get cursor out of bounds — asked=4 bound=2 lastAppended=1`.
  *
  * ⚠️ **What makes this test non-vacuous is the head being past stack 0 when the wipe happens.** A
  * coil wiped while the head is still at stack 0 bootstraps correctly on its own and proves nothing
  * — the whole failure is the gap between where the coil restarts and where the head has got to. So
  * the run waits for a hard-confirmation before wiping, and asserts on it afterwards.
  */
class CoilRejoinTest extends AnyFunSuite {

    test("a coil peer whose store is wiped rejoins from a start point and acks again") {
        val victim = CoilPeerNumber(0)
        // Long enough for the head to force a major block: a start point needs a settlement, and
        // kick requests alone only ever make minor blocks. Virtual time, so the cost is iterations
        // of the kick loop rather than wall clock.
        val runUp = 12.minutes
        val rejoinWindow = 3.minutes
        val kickEvery = 10.seconds

        val state = Stage4Suite
            .genInitialState(nPeers = 2, nCoilPeers = 2)
            .pureApply(Gen.Parameters.default, Seed(0L))

        val inputs = MultiPeerHeadHarness.Inputs(
          config = MultiPeerHeadHarness.Config(
            label = "coil-rejoin",
            backendMode = MultiPeerHeadHarness.StorageBackend.Mode.InMemory,
            transportMode = MultiPeerHeadHarness.Transport.Mode.Direct,
          ),
          multiNodeConfig = state.params.multiNodeConfig,
          coilNodeConfigs = state.params.coilNodeConfigs,
          preinitPeerUtxosL1 = state.preinitPeerUtxosL1,
          takeoffTime = state.takeoffTime,
          startEpochMs = state.currentModelTime.getEpochSecond * 1000L,
        )

        val hooks = MultiPeerHeadHarness.Hooks[Option[RequestSequencer.Handle]](
          tracer = ContraTracer.nullTracer[IO, MultiPeerHeadHarness.Event],
          handle = MultiPeerHeadHarness.requestSequencerHandle,
        )

        def kickFor(
            harness: MultiPeerHeadHarness.Harness[Option[RequestSequencer.Handle]],
            d: FiniteDuration
        ) =
            List
                .range(0, (d / kickEvery).toInt)
                .traverse_ { i =>
                    IO.sleep(kickEvery) >> MultiPeerHeadHarness
                        .submitKickRequest(harness, HeadPeerNumber(i % 2))
                        .attempt
                        .void
                }

        // `Coil` exposes its backend, not a typed `Persistence`; build one to read the markers and
        // the start point back.
        given hydrozoa.config.head.network.CardanoNetwork.Section =
            state.params.multiNodeConfig.headConfig
        val persistenceTracer =
            ContraTracer.nullTracer[IO, hydrozoa.multisig.persistence.PersistenceEvent]

        /** Has the head made a major yet? Without one there is no settlement to seed from, and the
          * hub answers `NoMajorYet` — a real limitation, but not what this test is about.
          */
        def sawMajor(store: hydrozoa.multisig.persistence.BackendStore[IO]): IO[Boolean] =
            hydrozoa.multisig.persistence.Persistence
                .fromBackend(store, persistenceTracer)
                .flatMap { p =>
                    Markers
                        .derive(p, PeerId.Head(HeadPeerNumber(0)))
                        .flatMap(m =>
                            (0 to m.hardConfirmed.fold(0)(s => s: Int)).toList
                                .traverse(n =>
                                    p.get(
                                      StoreKey.HardConfirmation(
                                        hydrozoa.multisig.ledger.stack.StackNumber(n)
                                      )
                                    )
                                )
                                .map(_.flatten.exists(_.payload match {
                                    case hydrozoa.multisig.ledger.stack.StackEffects.HardConfirmed
                                            .Regular(ps) =>
                                        ps.toList.exists(
                                          _.isInstanceOf[
                                            hydrozoa.multisig.ledger.stack.PartitionEffects.Major[?]
                                          ]
                                        )
                                    case _ => false
                                }))
                        )
                }

        def readState(store: hydrozoa.multisig.persistence.BackendStore[IO]) =
            hydrozoa.multisig.persistence.Persistence
                .fromBackend(store, persistenceTracer)
                .flatMap(p =>
                    (Markers.derive(p, PeerId.Coil(victim)), p.get(StoreKey.StartPoint)).tupled
                )

        val program =
            MultiPeerHeadHarness.resource(inputs, hooks).use { harness =>
                for
                    _ <- kickFor(harness, runUp)
                    // Where the coil stood before the wipe, and how far the head had got.
                    majorMade <- sawMajor(harness.peers(HeadPeerNumber(0)).backendStore)
                    before <- readState(harness.coils(victim).backendStore)
                    (beforeWipe, _) = before
                    rejoined <- harness.rejoinCoilPeer(victim)
                    _ <- kickFor(harness, rejoinWindow)
                    errors <- harness.sutErrors.get
                    afterPair <- readState(rejoined.backendStore)
                    (after, startPoint) = afterPair
                yield (errors, beforeWipe, after, startPoint, majorMade)
            }

        val (errors, beforeWipe, after, startPoint, majorMade) = TestControlDriver.run(program)

        val problems = List(
          Option.when(beforeWipe.hardConfirmed.isEmpty)(
            "the head never hard-confirmed before the wipe, so the coil would have bootstrapped " +
                "stack 0 correctly on its own and this run proves nothing"
          ),
          Option.when(!majorMade)(
            "the head made no major block in the run-up, so there was no settlement to seed " +
                "from and the hub could only answer NoMajorYet — lengthen `runUp`"
          ),
          Option.when(errors.nonEmpty)(s"uncaught actor errors after the rejoin: $errors"),
          Option.when(startPoint.isEmpty)(
            "the wiped coil adopted no start point — it re-derived stack 0 instead, which is the " +
                "original failure"
          ),
          Option.when(after.hardAckedStack.isEmpty)(
            "the rejoined coil produced no hard-ack of its own, so it is not acking for the head"
          ),
        ).flatten
        assert(problems.isEmpty, problems.mkString("; "))
    }
}
