package hydrozoa.integration.harness

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.integration.stage4.Stage4Suite
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.RequestSequencer
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.persistence.{BackendStore, Markers}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*

/** Restarts a peer that has already produced a run of blocks, rather than one crashed during
  * bring-up.
  *
  * That distinction is the whole point. [[CrashRestartTest]] and `RecoverSeamsTest` restart a peer
  * at durable write ~3, while it is still coming up, and both passed throughout two defects that
  * made recovery impossible for any peer with real history:
  *
  *   - `BlockWeaver` opened on block 1 regardless of what it had applied, so a peer with blocks
  *     behind it armed as leader for block 2 and terminated the actor system on the first
  *     confirmation the `FastConsensusActor` re-derived;
  *   - nothing re-sent the last soft-confirmation, so a weaver resuming as leader waited forever
  *     for a confirmation already on disk — the peer came up "active", filled its mempool, and
  *     produced nothing.
  *
  * Opening on block 1 happens to be correct during bring-up, which is exactly why crashing there
  * could not see either. So this test lets the head run first, and only then restarts a peer.
  *
  * The assertion is that the restarted peer's own `fastBlockMark` — `max(BlockResult)`, which it
  * writes once per block it applies — is above where recovery left it. Termination fails the
  * uncaught-error check; the stall fails the mark check.
  */
class RestartAfterBlocksTest extends AnyFunSuite {

    /** Virtual time the head runs before the restart, so the victim has real history to recover. */
    private val warmup = 25.seconds

    /** Virtual time after the restart for recovery plus a few fresh blocks. */
    private val recoveryWindow = 3.minutes

    private val kickEvery = 10.seconds

    test("a head peer restarted after producing blocks resumes weaving") {
        val (errors, before, after) = run(restartHead = Some(HeadPeerNumber(1)), restartCoil = None)
        report("head peer 1", errors, before, after)
    }

    test("a coil peer restarted after applying blocks resumes applying") {
        val (errors, before, after) = run(restartHead = None, restartCoil = Some(CoilPeerNumber(0)))
        report("coil peer 0", errors, before, after)
    }

    private def report(
        who: String,
        errors: List[String],
        before: Option[BlockNumber],
        after: Option[BlockNumber],
    ): Unit =
        val advanced = (before, after) match
            case (_, None)          => false
            case (None, Some(_))    => true
            case (Some(b), Some(a)) => Ordering[Int].gt(a: Int, b: Int)
        val problems = List(
          Option.when(errors.nonEmpty)(s"uncaught actor errors after restarting $who: $errors"),
          Option.when(!advanced)(
            s"restarted $who applied no further blocks: fastBlockMark was $before at restart " +
                s"and $after at the end of the window"
          ),
        ).flatten
        assert(problems.isEmpty, problems.mkString("; "))

    /** Warm the head up, restart the named peer, then keep kicking blocks. Returns the uncaught
      * actor errors and the victim's own fast anchor at restart and at the end.
      */
    private def run(
        restartHead: Option[HeadPeerNumber],
        restartCoil: Option[CoilPeerNumber],
    ): (List[String], Option[BlockNumber], Option[BlockNumber]) = {
        val nHeadPeers = 2

        val state = Stage4Suite
            .genInitialState(nPeers = nHeadPeers, nCoilPeers = 2)
            .pureApply(Gen.Parameters.default, Seed(0L))

        val inputs = MultiPeerHeadHarness.Inputs(
          config = MultiPeerHeadHarness.Config(
            label = "restart-after-blocks",
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

        val program = MultiPeerHeadHarness.resource(inputs, hooks).use { harness =>
            def kickFor(window: FiniteDuration): IO[Unit] =
                List
                    .range(0, (window / kickEvery).toInt)
                    .traverse_ { i =>
                        IO.sleep(kickEvery) >> MultiPeerHeadHarness
                            .submitKickRequest(harness, HeadPeerNumber(i % nHeadPeers))
                            .attempt
                            .void
                    }

            for
                // Give the victim real history first — this is the part the bring-up-time crash
                // tests skip, and the only reason either defect is reachable.
                _ <- kickFor(warmup)
                store <- restartHead match
                    case Some(peer) => harness.restartHeadPeer(peer).map(_.backendStore)
                    case None =>
                        val coil = restartCoil.getOrElse(
                          throw new IllegalArgumentException("name a peer to restart")
                        )
                        harness.restartCoilPeer(coil).map(_.backendStore)
                before <- Markers.recoverFastBlockMark(store: BackendStore[IO])
                _ <- kickFor(recoveryWindow)
                errors <- harness.sutErrors.get
                after <- Markers.recoverFastBlockMark(store)
            yield (errors, before, after)
        }

        TestControlDriver.run(program)
    }
}
