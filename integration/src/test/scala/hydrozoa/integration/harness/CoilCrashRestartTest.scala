package hydrozoa.integration.harness

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.integration.stage4.Stage4Suite
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.RequestSequencer
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.persistence.Markers
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*

/** The coil-side counterpart to [[CrashRestartTest]]: crash a coil peer at a chosen durable write,
  * restart it against its own store, and assert the head goes on hard-confirming.
  *
  * Coil recovery had no end-to-end exercise at all — the harness could not even restart a coil
  * (`restartCoilPeer` and the `wrapPersistence` hook on `buildCoil` are new), which is part of why
  * the fleet's coil peers were the ones that would not come back.
  *
  * What is asserted is that the RESTARTED coil's own fast anchor keeps advancing. `fastBlockMark`
  * is `max(BlockResult)`, written by the coil itself once per block it applies, so a mark that
  * climbs after the restart means the coil rebuilt its state from its store, re-attached to its
  * hub, and is applying the blocks being relayed to it. A coil that refused to boot — the fleet's
  * failure — leaves it pinned wherever recovery found it.
  *
  * Asserting on the head's hard-confirmation instead would not work: `CrashingPersistence` signals
  * the crash point and lets the peer keep running (blocking there would stall its mailbox so `stop`
  * could never be processed), so between the signal and the restart the "crashed" coil is still
  * live and still signing. A control run that skipped the restart entirely still saw the head
  * confirm, which is exactly the vacuous pass that assertion would have given.
  */
class CoilCrashRestartTest extends AnyFunSuite {

    test("a coil peer crashed mid-run recovers from its store and keeps applying blocks") {
        val victim = CoilPeerNumber(0)
        // An early durable write on the coil: it persists inbound population entries from its hub
        // (CR8 write-before-advance), so a low count lands during bring-up.
        val crashAtWrite = 3
        // Virtual time granted after the restart for recovery + a fresh hard-confirmation.
        val recoveryWindow = 3.minutes
        val kickEvery = 10.seconds

        val state = Stage4Suite
            .genInitialState(nPeers = 2, nCoilPeers = 2)
            .pureApply(Gen.Parameters.default, Seed(0L))

        val inputs = MultiPeerHeadHarness.Inputs(
          config = MultiPeerHeadHarness.Config(
            label = "coil-crash-restart",
            backendMode = MultiPeerHeadHarness.StorageBackend.Mode.InMemory,
            transportMode = MultiPeerHeadHarness.Transport.Mode.Direct,
          ),
          multiNodeConfig = state.params.multiNodeConfig,
          coilNodeConfigs = state.params.coilNodeConfigs,
          preinitPeerUtxosL1 = state.preinitPeerUtxosL1,
          takeoffTime = state.takeoffTime,
          startEpochMs = state.currentModelTime.getEpochSecond * 1000L,
        )

        val program =
            for
                crashSignal <- IO.deferred[Unit]
                writeCounter <- IO.ref(0)
                hooks = MultiPeerHeadHarness.Hooks[Option[RequestSequencer.Handle]](
                  tracer = ContraTracer.nullTracer[IO, MultiPeerHeadHarness.Event],
                  handle = MultiPeerHeadHarness.requestSequencerHandle,
                  wrapPersistence = (peerId, persistence) =>
                      if peerId == PeerId.Coil(victim) then
                          CrashingPersistence.wrap(
                            persistence,
                            writeCounter,
                            Some(
                              CrashingPersistence.Plan(
                                crashAtWrite,
                                CrashVariant.After,
                                crashSignal,
                              )
                            ),
                          )
                      else persistence,
                )
                outcome <- MultiPeerHeadHarness.resource(inputs, hooks).use { harness =>
                    for
                        _ <- crashSignal.get.timeoutTo(
                          3.minutes,
                          IO.raiseError(
                            new AssertionError(s"coil $victim never reached write $crashAtWrite")
                          ),
                        )
                        // Stop the coil's subtree and re-spawn it against the SAME store + L2
                        // ledger; recovery runs inline on boot and it re-attaches to its hub.
                        restarted <- harness.restartCoilPeer(victim)
                        // The coil's own fast anchor as recovery left it — the baseline the mark
                        // has to beat for the coil to be doing any work at all.
                        markAtRestart <- Markers.recoverFastBlockMark(restarted.backendStore)
                        // Keep blocks flowing so the slow side has something to confirm.
                        _ <- List
                            .range(0, (recoveryWindow / kickEvery).toInt)
                            .traverse_ { i =>
                                IO.sleep(kickEvery) >> MultiPeerHeadHarness
                                    .submitKickRequest(harness, HeadPeerNumber(i % 2))
                                    .attempt
                                    .void
                            }
                        errors <- harness.sutErrors.get
                        markAtEnd <- Markers.recoverFastBlockMark(restarted.backendStore)
                    yield (errors, markAtRestart, markAtEnd)
                }
            yield outcome

        val (errors, markAtRestart, markAtEnd) = TestControlDriver.run(program)

        val advanced = (markAtRestart, markAtEnd) match
            case (_, None)          => false
            case (None, Some(_))    => true
            case (Some(b), Some(a)) => Ordering[Int].gt(a: Int, b: Int)

        val problems = List(
          Option.when(errors.nonEmpty)(s"uncaught actor errors after restart: $errors"),
          Option.when(!advanced)(
            "restarted coil applied no further blocks: fastBlockMark stayed at " +
                s"$markAtRestart (was $markAtEnd at the end of the window)"
          ),
        ).flatten
        assert(problems.isEmpty, problems.mkString("; "))
    }
}
