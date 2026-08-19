package hydrozoa.integration.harness

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.integration.stage4.Stage4Suite
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.RequestSequencer
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*

/** Guards the durability ordering every peer's store has to keep, on both peer types.
  *
  * `ReplayActor.validateInvariants` refuses to boot a peer whose store has `confirmed > acked`, and
  * a coil peer on the dev fleet hit exactly that, repeatedly:
  *
  * {{{
  * Recovery refused: store inconsistency (confirmed > acked).
  *   softConfirmed=Some(492933) hardConfirmed=Some(301)
  *   fastBlockMark=Some(492932), hardAckedStack=Some(301)
  * }}}
  *
  * `softConfirmed = fastBlockMark + 1` on the fast arm, the slow arm clean: the peer had persisted
  * `SoftConfirmation[n]` but not `BlockResult[n]`. That is a write-ordering bug, not a crash-timing
  * accident — a coil authors no soft-ack, so its consensus cell holds the head peers' acks already
  * and saturates the instant `JointLedger` hands it the brief. `FastConsensusActor` then wrote
  * `SoftConfirmation[n]` on its own fiber while `JointLedger` was still on its way to
  * `persistCoilBlockBundle`. A head peer's cell also waits for its own soft-ack, authored after
  * that persist, so head peers never showed it.
  *
  * The test does not try to crash inside that window — it is a few instructions wide, and a
  * crash-injection test samples one op out of thousands. [[DurabilityOrderOracle]] instead
  * evaluates the boot gate after every durable write on every peer, so an ordering that violates it
  * is caught on the write that violates it. Any recorded violation is a moment at which killing
  * that peer would have left it unable to start.
  */
class DurabilityOrderTest extends AnyFunSuite {

    /** The topology ladder, smallest first. A singleton head has no cross-peer round at all; each
      * rung after it adds one source of concurrency, so a violation that appears at a given rung
      * names the thing that rung introduced.
      */
    private val topologies: List[(Int, Int)] = List(
      1 -> 0,
      1 -> 1,
      1 -> 2,
      2 -> 1,
      2 -> 2,
      3 -> 3,
    )

    // Fiber turns handed out before each durable write; see SlowPersistence. Several settings,
    // because the number of turns decides which interleavings are reachable: too few and the racing
    // fiber never gets scheduled inside the window, too many and it always completes before the
    // window opens. Zero is the control — it must pass whatever the write ordering is, since
    // nothing is given a turn, and it did even against the pre-fix JointLedger.
    private val yieldSettings: List[Int] = List(0, 2, 8)

    for
        (nHeadPeers, nCoilPeers) <- topologies
        preWriteYields <- yieldSettings
    do
        test(
          s"no store outruns its own durability: $nHeadPeers head + $nCoilPeers coil, " +
              s"$preWriteYields pre-write yields"
        ) {
            check(nHeadPeers, nCoilPeers, preWriteYields)
        }

    private def check(nHeadPeers: Int, nCoilPeers: Int, preWriteYields: Int): Unit = {
        // Long enough for bring-up, stack 0, and a run of kicked blocks on every leader.
        val runFor = 5.minutes
        val kickEvery = 10.seconds

        val state = Stage4Suite
            .genInitialState(nPeers = nHeadPeers, nCoilPeers = nCoilPeers)
            .pureApply(Gen.Parameters.default, Seed(0L))

        val inputs = MultiPeerHeadHarness.Inputs(
          config = MultiPeerHeadHarness.Config(
            label = s"durability-order-h$nHeadPeers-c$nCoilPeers-y$preWriteYields",
            backendMode = MultiPeerHeadHarness.StorageBackend.Mode.InMemory,
            transportMode = MultiPeerHeadHarness.Transport.Mode.Direct,
          ),
          multiNodeConfig = state.params.multiNodeConfig,
          coilNodeConfigs = state.params.coilNodeConfigs,
          preinitPeerUtxosL1 = state.preinitPeerUtxosL1,
          takeoffTime = state.takeoffTime,
          startEpochMs = state.currentModelTime.getEpochSecond * 1000L,
        )

        val everyPeer: List[PeerId] =
            (0 until nHeadPeers).toList.map(i => PeerId.Head(HeadPeerNumber(i))) :::
                (0 until nCoilPeers).toList.map(i => PeerId.Coil(CoilPeerNumber(i)))

        val program =
            for
                violations <- IO.ref(List.empty[DurabilityOrderOracle.Violation])
                // One op counter per peer, allocated up front: `wrapPersistence` is a pure
                // function, so it cannot allocate its own.
                counters <- everyPeer.traverse(p => IO.ref(0).map(p -> _)).map(_.toMap)
                hooks = MultiPeerHeadHarness.Hooks[Option[RequestSequencer.Handle]](
                  tracer = ContraTracer.nullTracer[IO, MultiPeerHeadHarness.Event],
                  handle = MultiPeerHeadHarness.requestSequencerHandle,
                  wrapPersistence = (peerId, persistence) =>
                      counters.get(peerId) match
                          case Some(counter) =>
                              // Slow the store down first, then observe it: the oracle sees the
                              // same writes either way, but the yields give every other fiber of
                              // this peer a turn at the moment a slow disk would, which is what
                              // makes an ordering that depends on winning a race fail here rather
                              // than only on real hardware. See SlowPersistence.
                              DurabilityOrderOracle.wrap(
                                SlowPersistence.wrap(persistence, preWriteYields),
                                peerId,
                                counter,
                                violations,
                              )
                          case None => persistence,
                )
                observed <- MultiPeerHeadHarness.resource(inputs, hooks).use { harness =>
                    // Kick each leader in turn so blocks keep being produced across the whole fast
                    // round for the entire window; the coils see every one of them relayed.
                    val kicks = (0 until (runFor / kickEvery).toInt).toList
                    kicks.traverse_ { i =>
                        IO.sleep(kickEvery) >> MultiPeerHeadHarness
                            .submitKickRequest(harness, HeadPeerNumber(i % nHeadPeers))
                            .attempt
                            .void
                    } >> (
                      violations.get,
                      counters.toList.traverse { case (p, c) =>
                          c.get.map(p -> _)
                      }
                    ).tupled
                }
            yield observed

        val (found, opCounts) = TestControlDriver.run(program)

        val opsSeen = opCounts.map { case (p, n) => s"$p=$n" }.mkString(", ")
        val problems = List(
          // A silently unwired oracle would pass vacuously, so make that a failure of its own.
          Option.when(!opCounts.exists(_._2 > 0))(
            s"the oracle observed no durable writes at all — it was not wired in: $opsSeen"
          ),
          Option.when(found.nonEmpty)(
            s"store reached an unrecoverable ordering ${found.size} time(s) " +
                s"(durable ops per peer: $opsSeen):" +
                found.take(10).map("\n  " + _).mkString
          ),
        ).flatten
        val _ = assert(problems.isEmpty, problems.mkString("; "))
    }
}
