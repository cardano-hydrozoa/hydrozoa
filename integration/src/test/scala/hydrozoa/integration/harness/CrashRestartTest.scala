package hydrozoa.integration.harness

import cats.effect.IO
import hydrozoa.integration.stage4.Stage4Suite
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.persistence.Markers
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*

/** End-to-end proof for the deterministic crash-recovery mechanism (see
  * `.scratch/crash-after-write-n-scope.md`): crash one head peer at a chosen durable write during
  * bring-up, restart it against its own store, and assert it recovers and rejoins the running head.
  *
  * This is the single hand-written case that proves the harness plumbing — [[CrashingPersistence]]
  * (the injection seam), `MultiPeerHeadHarness.Hooks.wrapPersistence`, and
  * `MultiPeerHeadHarness.restartHeadPeer` (stop the subtree, re-spawn against the same store + L2
  * ledger, re-attach). The exhaustive `N × variant × victim-role` sweep and the ScalaCheck breadth
  * layer build on it. It runs entirely on the cats-effect `TestControl` virtual clock (Direct
  * transport, in-memory stores), so it is deterministic and fast.
  *
  * It directly guards the two recovery bugs fixed on `fix/recovery-crash-restart`: without the
  * stack-0-close-bundle fix the restart fatally terminates (out-of-bounds on the own-hard-ack
  * lane), and without the Puller cold-cursor fix the restarted peer loops rejecting and never
  * rejoins — so the head could not hard-confirm past the crash.
  */
class CrashRestartTest extends AnyFunSuite {

    test("a head peer crashed mid-init recovers from its store and rejoins the running head") {
        val victim = HeadPeerNumber(1)
        // An early durable write during head-1's bring-up. Small enough to fire well within the
        // crash-wait budget below; the exact op it lands on is deterministic under TestControl.
        val crashAtWrite = 3
        // Virtual time granted after the restart for recovery + stack-0 hard-confirmation.
        val recoveryWindow = 30.seconds

        // A deterministic 2-head + 2-coil head (coil quorum 2) on the TestControl clock.
        val state = Stage4Suite
            .genInitialState(nPeers = 2, nCoilPeers = 2)
            .pureApply(Gen.Parameters.default, Seed(0L))

        val inputs = MultiPeerHeadHarness.Inputs(
          config = MultiPeerHeadHarness.Config(
            label = "crash-restart",
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
                hooks = MultiPeerHeadHarness.Hooks[Unit](
                  tracer = ContraTracer.nullTracer[IO, MultiPeerHeadHarness.Event],
                  handle = (_, _) => IO.unit,
                  // Wrap only the victim's persistence: crash it at the N-th durable write.
                  wrapPersistence = (peerId, persistence) =>
                      if peerId == PeerId.Head(victim) then
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
                        // Wait until head-1 actually crashes at write N — bounded so a mis-chosen N
                        // fails fast rather than spinning the virtual clock forever.
                        _ <- crashSignal.get.timeoutTo(
                          3.minutes,
                          IO.raiseError(
                            new AssertionError(s"head $victim never reached write $crashAtWrite")
                          ),
                        )
                        // Stop head-1's subtree and re-spawn it against the SAME store + L2 ledger;
                        // recovery runs inline on boot and it re-attaches to the running head.
                        restarted <- harness.restartHeadPeer(victim)
                        _ <- IO.sleep(recoveryWindow)
                        errors <- harness.sutErrors.get
                        headConfirmed <- Markers.recoverHardConfirmed(
                          harness.peers(HeadPeerNumber(0)).backendStore
                        )
                        victimOwnAcks <- Markers.recoverHardAcked(
                          restarted.backendStore,
                          PeerId.Head(victim),
                        )
                    yield (errors, headConfirmed, victimOwnAcks)
                }
            yield outcome

        // Driven on the virtual clock (see TestControlDriver for why not executeEmbed).
        val (errors, headConfirmed, victimOwnAcks) = TestControlDriver.run(program)

        // Collect all failures so one assertion reports the full picture.
        val problems = List(
          Option.when(errors.nonEmpty)(s"uncaught actor errors after restart: $errors"),
          // The head hard-confirmed a stack AFTER head-1 was crashed mid-init — only possible if
          // head-1 recovered and rejoined (hard-confirmation needs every head peer's signature).
          Option.when(headConfirmed.isEmpty)(
            "head did not hard-confirm any stack after the restart"
          ),
          // head-1's own hard-acks are durable post-recovery (the stack-0-close-bundle fix), so its
          // own-hard-ack marker is restored rather than empty.
          Option.when(victimOwnAcks.isEmpty)("recovered head-1 has no own hard-acks"),
        ).flatten
        assert(problems.isEmpty, problems.mkString("; "))
    }
}
