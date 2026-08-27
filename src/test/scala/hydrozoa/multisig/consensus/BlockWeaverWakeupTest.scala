package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.test.TestKit
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.multisig.ledger.block.BlockNumber
import java.util.concurrent.atomic.AtomicReference
import org.scalacheck.{Properties, Test}
import scala.concurrent.duration.DurationInt
import test.TestPeerName.Carol

/** The forced-wakeup path — the weaver's deadman, and until now completely untested.
  *
  * A leader that has confirmed the previous block but received no request sits in
  * `Leader.AwaitingRequest` with one wakeup armed, sleeping until whichever of
  * `mDepositDecisionWakeupTime` and `forcedMajorBlockWakeupTime` is EARLIER. When it fires the
  * block is completed with no request at all — that is how a deposit gets absorbed on a quiet head,
  * and how a fallback tx gets replaced before it can be posted. Nothing exercised it: both header
  * builders in `BlockWeaverTest` hard-code `mDepositDecisionWakeupTime = None` and no property ever
  * sends a `Wakeup`. The one comment that mentions it says "(or the forced wakeup)" and moves on.
  *
  * ==Why this suite exists now==
  * `18a62249` reintroduced cancel-on-replace for the wakeup fiber, to stop the weaver orphaning one
  * fiber per block. Fiber cancellation on this path had been deliberately REMOVED once before, in
  * "Fix: prevent race condition when wakeup" (`e6ca939e`, PR #412), because back then cancellation
  * was what kept a stale wakeup from forcing a block for the wrong block number, and the cancel
  * raced the send. That commit made `Wakeup` carry a block number and filter at the handler
  * instead.
  *
  * So correctness now lives in the guard, and cancellation is only a resource fix. This suite pins
  * both halves of that claim: the wakeups still fire when they should (`L*`), the guard still
  * refuses the ones it should (`S*`), and cancellation actually collects superseded fibers rather
  * than merely letting the guard hide them (`R*`).
  *
  * ⚠️ These are wall-clock tests against a 1-second slot quantization, so targets are placed whole
  * seconds out and each property runs a couple of times rather than the suite default of 100.
  */
object BlockWeaverWakeupTest extends Properties("Block weaver wakeup"), TestKit {

    import BlockWeaverTestHelpers.*
    import bwTest.*

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withMinSuccessfulTests(2).withWorkers(1)

    /** Long enough that nothing fires during a test that is asserting something else. */
    private val NeverInThisTest = 300.seconds

    /** Poll until the condition holds, then give up silently and let the caller assert with a
      * readable message. `waitForIdle` can return while a message is still in flight to the ledger
      * mock, so a one-shot read straight after it is stale. (Same helper as `BlockWeaverTest`,
      * which keeps it private to its own object.)
      */
    private def settle(condition: => Boolean): BWTest[Unit] =
        lift(awaitCond(IO(condition), 8.seconds, 50.millis).attempt.void)

    private def events(seen: AtomicReference[Vector[BlockWeaverEvent]]): Vector[BlockWeaverEvent] =
        seen.get

    // ===================================
    // L1 -- the deadman fires with no request at all (George's case 1)
    // ===================================
    val _ = property("a forced-major wakeup completes the block with no request") = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          made <- mkBlockWeaverActorWithEvents(Carol.headPeerNumber)
          weaver = made._1
          seen = made._2
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          brief1 <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift((weaver ! brief1) >> env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get == Vector(BlockNumber(1)))
          confirmed <- mkConfirmedWithWakeups(
            BlockNumber(1),
            config.headConfig,
            forcedMajorIn = 2.seconds,
            depositWakeupIn = None
          )
          _ <- lift((weaver ! confirmed) >> env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get.contains(BlockNumber(2)))
          starts <- lift(IO(env.jointLedgerMock.startBlockNums.get))
          fed <- lift(IO(env.jointLedgerMock.events.get))
          evs <- lift(IO(events(seen)))
          _ <- assertWith(
            starts == Vector(BlockNumber(1), BlockNumber(2)),
            s"the armed wakeup must start and complete block 2 unaided, but StartBlocks were $starts"
          )
          _ <- assertWith(
            fed.isEmpty,
            s"no request was ever sent, so none may reach the ledger, but got $fed"
          )
          _ <- assertWith(
            evs.contains(BlockWeaverEvent.ForcedBlockCompletion(BlockNumber(2))),
            s"expected a ForcedBlockCompletion for block 2; events were $evs"
          )
      } yield true
    )

    // ===================================
    // S3 -- the EARLIER of the two targets wins (George's case 2: a deposit pulls the block forward)
    // ===================================
    val _ = property("a deposit wakeup earlier than the forced-major target wins") = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          made <- mkBlockWeaverActorWithEvents(Carol.headPeerNumber)
          weaver = made._1
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          brief1 <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift((weaver ! brief1) >> env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get == Vector(BlockNumber(1)))
          // The forced-major target is minutes out; only the deposit target can explain a
          // completion inside this test's settle window.
          confirmed <- mkConfirmedWithWakeups(
            BlockNumber(1),
            config.headConfig,
            forcedMajorIn = NeverInThisTest,
            depositWakeupIn = Some(2.seconds)
          )
          _ <- lift((weaver ! confirmed) >> env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get.contains(BlockNumber(2)))
          starts <- lift(IO(env.jointLedgerMock.startBlockNums.get))
          _ <- assertWith(
            starts.contains(BlockNumber(2)),
            s"the deposit target should have pulled block 2 forward, but StartBlocks were $starts"
          )
      } yield true
    )

    // ===================================
    // The control for both tests above. Without it, "block 2 completed" could be satisfied by a
    // weaver that completes blocks for some entirely unrelated reason, and neither test would know.
    // ===================================
    val _ = property("control: with both targets far out, nothing completes on its own") = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          made <- mkBlockWeaverActorWithEvents(Carol.headPeerNumber)
          weaver = made._1
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          brief1 <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift((weaver ! brief1) >> env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get == Vector(BlockNumber(1)))
          confirmed <- mkConfirmedWithWakeups(
            BlockNumber(1),
            config.headConfig,
            forcedMajorIn = NeverInThisTest,
            depositWakeupIn = None
          )
          _ <- lift((weaver ! confirmed) >> env.system.waitForIdle())
          _ <- lift(IO.sleep(4.seconds))
          starts <- lift(IO(env.jointLedgerMock.startBlockNums.get))
          _ <- assertWith(
            starts == Vector(BlockNumber(1)),
            s"no wakeup was due, so block 2 must not start, but StartBlocks were $starts"
          )
      } yield true
    )

    // ===================================
    // L2 -- GUM-111: a target already in the past must fire immediately, not never
    // ===================================
    val _ = property("a wakeup target already in the past fires immediately") = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          made <- mkBlockWeaverActorWithEvents(Carol.headPeerNumber)
          weaver = made._1
          seen = made._2
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          brief1 <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift((weaver ! brief1) >> env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get == Vector(BlockNumber(1)))
          // Observed for real on the stage4 20-peer head: virtual time advanced past a
          // deposit-driven target during cross-peer ack collection, leaving a negative sleep.
          confirmed <- mkConfirmedWithWakeups(
            BlockNumber(1),
            config.headConfig,
            forcedMajorIn = NeverInThisTest,
            depositWakeupIn = Some(-30.seconds)
          )
          _ <- lift((weaver ! confirmed) >> env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get.contains(BlockNumber(2)))
          starts <- lift(IO(env.jointLedgerMock.startBlockNums.get))
          evs <- lift(IO(events(seen)))
          _ <- assertWith(
            starts.contains(BlockNumber(2)),
            s"a past-due target must fire at once, but StartBlocks were $starts"
          )
          _ <- assertWith(
            evs.contains(BlockWeaverEvent.NonPositiveWakeupDelay(BlockNumber(2))),
            s"expected the non-positive-delay path to be taken; events were $evs"
          )
      } yield true
    )

    // ===================================
    // S1 -- the guard. This is the property `e6ca939e` added, and the reason cancellation is safe
    // to have back: a cancel that loses its race delivers a superseded Wakeup, and the guard is
    // what refuses it.
    // ===================================
    val _ = property("a Wakeup for any other block is ignored; the current one is honoured") = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          made <- mkBlockWeaverActorWithEvents(Carol.headPeerNumber)
          weaver = made._1
          seen = made._2
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          brief1 <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift((weaver ! brief1) >> env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get == Vector(BlockNumber(1)))
          confirmed <- mkConfirmedWithWakeups(
            BlockNumber(1),
            config.headConfig,
            forcedMajorIn = NeverInThisTest,
            depositWakeupIn = None
          )
          _ <- lift((weaver ! confirmed) >> env.system.waitForIdle())
          // Stale (a wakeup armed for an already-superseded block) and future (impossible today,
          // but the handler distinguishes them and the distinction is worth pinning).
          _ <- lift((weaver ! BlockWeaver.Wakeup(BlockNumber(1))) >> env.system.waitForIdle())
          _ <- lift((weaver ! BlockWeaver.Wakeup(BlockNumber(9))) >> env.system.waitForIdle())
          startsAfterBogus <- lift(IO(env.jointLedgerMock.startBlockNums.get))
          _ <- assertWith(
            startsAfterBogus == Vector(BlockNumber(1)),
            s"a Wakeup for another block must not complete anything, but StartBlocks were $startsAfterBogus"
          )
          evs <- lift(IO(events(seen)))
          _ <- assertWith(
            evs.contains(
              BlockWeaverEvent.WakeupIgnored(BlockNumber(1), BlockNumber(2), isFuture = false)
            ) && evs.contains(
              BlockWeaverEvent.WakeupIgnored(BlockNumber(9), BlockNumber(2), isFuture = true)
            ),
            s"expected both wakeups ignored, with past/future distinguished; events were $evs"
          )
          // ...and the same message for the CURRENT block does complete it, so the assertion above
          // is about the block number and not about wakeups being inert in this state.
          _ <- lift((weaver ! BlockWeaver.Wakeup(BlockNumber(2))) >> env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get.contains(BlockNumber(2)))
          finalStarts <- lift(IO(env.jointLedgerMock.startBlockNums.get))
          _ <- assertWith(
            finalStarts == Vector(BlockNumber(1), BlockNumber(2)),
            s"the current block's Wakeup must complete block 2, but StartBlocks were $finalStarts"
          )
      } yield true
    )

    // ===================================
    // R3 -- ⛔ the discriminator for cancellation, and the reason this suite exists.
    //
    // Arm a wakeup due in ~2s, then complete its block with a request well before it is due. WITH
    // cancellation the fiber is collected at completion and its Wakeup never arrives. WITHOUT it
    // the fiber sleeps out its term and delivers, and the guard quietly absorbs the message --
    // exactly the old behaviour, and invisible unless you go looking for the absorbed message. So
    // "zero ignored and zero dropped" is what separates a cancellation that works from a guard
    // covering for one that does not.
    //
    // ⚠️ This test is why the cancellation is at block completion and not only cancel-on-replace.
    // Written against cancel-on-replace alone it FAILED with `WakeupDropped(2)`: leadership
    // rotates, so after completing block 2 this peer became a FOLLOWER for block 3 and never armed
    // another wakeup, leaving the old fiber to sleep out its term. Bounded to one stale fiber, but
    // one sleeping for hours per role change is still wrong.
    //
    // It fails loudly in the other direction too: if cancellation ever ate a LIVE wakeup, the
    // forced-major and deposit properties above would stop completing their blocks.
    // ===================================
    val _ = property("a superseded wakeup fiber is cancelled, not merely ignored on arrival") = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          anyRequest <- pick(genUserRequest)
          made <- mkBlockWeaverActorWithEvents(Carol.headPeerNumber)
          weaver = made._1
          seen = made._2
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          brief1 <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift((weaver ! brief1) >> env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get == Vector(BlockNumber(1)))
          // Arms a wakeup for block 2, due in ~2s.
          confirmed1 <- mkConfirmedWithWakeups(
            BlockNumber(1),
            config.headConfig,
            forcedMajorIn = 2.seconds,
            depositWakeupIn = None
          )
          _ <- lift((weaver ! confirmed1) >> env.system.waitForIdle())
          // Supersede it well before it is due: the request completes block 2, which is where the
          // wakeup armed for block 2 stops being wanted.
          _ <- lift((weaver ! anyRequest) >> env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get.contains(BlockNumber(2)))
          // Sleep past when the cancelled fiber would have fired.
          _ <- lift(IO.sleep(4.seconds))
          evs <- lift(IO(events(seen)))
          stragglers = evs.collect {
              case e: BlockWeaverEvent.WakeupIgnored => e
              case e: BlockWeaverEvent.WakeupDropped => e
          }
          _ <- assertWith(
            stragglers.isEmpty,
            "the superseded wakeup fiber was not cancelled -- it slept out its term and delivered, " +
                s"and only the block-number guard hid it: $stragglers. Full trace: $evs"
          )
          starts <- lift(IO(env.jointLedgerMock.startBlockNums.get))
          _ <- assertWith(
            starts == Vector(BlockNumber(1), BlockNumber(2)),
            s"nothing should start block 3 here; StartBlocks were $starts"
          )
      } yield true
    )
}
