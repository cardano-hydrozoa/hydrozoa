package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import com.suprnation.actor.test.TestKit
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.consensus.UserRequest.TransactionRequest
import hydrozoa.multisig.consensus.UserRequestBody.TransactionRequestBody
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import org.scalacheck.{Gen, Properties, PropertyM, Test}
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.{Blake2b_256, Hash}
import scalus.uplc.builtin.ByteString
import test.Generators.Hydrozoa.genRequestId
import test.TestPeerName.{Bob, Carol}
import test.{PeersNumberSpec, TestM, TestMFixedEnv, TestPeersSpec}

object BlockWeaverTestHelpers {
    type BWTest[A] = TestM[TestR, A]
    val bwTest = TestMFixedEnv[TestR]()
    import bwTest.*

    case class TestR(
        multiNodeConfig: MultiNodeConfig,
        system: ActorSystem[IO],
        jointLedgerMock: JointLedgerMock,
        jointLedgerMockActor: JointLedger.Handle
    )

    val defaultResource: PropertyM[IO, Resource[IO, TestR]] =
        PropertyM
            .pick[IO, MultiNodeConfig](
              MultiNodeConfig
                  .generate(
                    TestPeersSpec.default.withPeersNumberSpec(PeersNumberSpec.Exact(3))
                  )()
            )
            .map { multiNodeConfig =>
                for {
                    system <- ActorSystem[IO]("Weaver SUT")
                    jointLedgerMock = JointLedgerMock()
                    jointLedgerMockActor <- Resource.eval(
                      system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock"))
                    )
                } yield TestR(multiNodeConfig, system, jointLedgerMock, jointLedgerMockActor)
            }

    /** A dummy user request whose content is not interesting to the block weaver — only the
      * request id matters. Closed over the trial's [[MultiNodeConfig]] so the head id / peer vkey
      * match the env.
      */
    def genUserRequest(mnc: MultiNodeConfig): Gen[UserRequestWithId] = {
        val zeroQI = QuantizedInstant(mnc.slotConfig, Instant.ofEpochSecond(0))
        for {
            requestId <- genRequestId
            userRequest = TransactionRequest(
              header = UserRequestHeader(
                headId = mnc.headConfig.headId,
                validityStart = RequestValidityStartTime(zeroQI),
                validityEnd = RequestValidityEndTime(zeroQI),
                bodyHash = Hash[Blake2b_256, Any](ByteString.fromArray(Array.fill[Byte](32)(0)))
              ),
              body = TransactionRequestBody(ByteString.empty),
              userVk = mnc.headConfig.headPeerVKeys.head
            )
        } yield UserRequestWithId(userRequest = userRequest, requestId = requestId)
    }

    def mkBlockWeaverActor(peerNumber: HeadPeerNumber): BWTest[BlockWeaver.Handle] =
        for {
            env <- ask
            config = env.multiNodeConfig.nodeConfigs(peerNumber)
            connections = BlockWeaver.ConnectionsPartial(env.jointLedgerMockActor)
            tracer = Slf4jTracer.sink.contramap(BlockWeaverEventFormat.humanFormat(peerNumber))
            actor <- lift(env.system.actorOf(BlockWeaver(config, connections, tracer)))
        } yield actor

    /** Empty block brief for block 1, so Carol starts working on block 2. */
    def mkDummyBlockBrief1(config: HeadConfig): BWTest[BlockBrief.Next] =
        lift(for {
            now <- realTimeQuantizedInstant(config.slotConfig)
        } yield {
            val blockCreationStartTime = BlockCreationStartTime(now)
            val blockCreationEndTime = BlockCreationEndTime(now + 1.second)
            val fallbackTxStartTime = config.txTiming.newFallbackStartTime(blockCreationEndTime)
            val forcedMajorBlockWakeupTime =
                config.txTiming.forcedMajorBlockWakeupTime(fallbackTxStartTime)

            BlockBrief.Minor(
              BlockHeader.Minor(
                blockNum = BlockNumber(1),
                blockVersion = BlockVersion.Full(0, 0),
                startTime = blockCreationStartTime,
                endTime = blockCreationEndTime,
                fallbackTxStartTime = fallbackTxStartTime,
                forcedMajorBlockWakeupTime = forcedMajorBlockWakeupTime,
                mDepositDecisionWakeupTime = None
              ),
              BlockBody.Minor(
                events = List.empty,
                depositsRefunded = List.empty
              )
            )
        })

    class JointLedgerMock extends Actor[IO, JointLedger.Requests.Request]:

        val events: AtomicReference[Vector[UserRequestWithId]] = AtomicReference(Vector.empty)
        val startBlockNums: AtomicReference[Vector[BlockNumber]] = AtomicReference(Vector.empty)

        override def receive: Receive[IO, JointLedger.Requests.Request] = {
            case e: UserRequestWithId =>
                IO { events.updateAndGet(_ :+ e) }
            case s: StartBlock =>
                IO { startBlockNums.updateAndGet(_ :+ s.blockNum) }
            case c: CompleteBlockRegular =>
                IO.pure(())
            case f: CompleteBlockFinal =>
                IO.pure(())
        }
}

object BlockWeaverTest extends Properties("Block weaver test"), TestKit {

    import BlockWeaverTestHelpers.*
    import bwTest.*

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withMinSuccessfulTests(100).withWorkers(1)

    /** Poll until the condition holds, giving up silently after a timeout. `waitForIdle` observes
      * each actor's idle state independently and can return while a message is still in flight to
      * the joint ledger mock (likewise, `expectMsgPF` matches on the tracked message buffer before
      * the mock processes the messages that follow), so a one-shot read of the mock's state right
      * after either of them can be stale. The caller asserts the condition, with a readable
      * message, after settling.
      */
    def settle(condition: => Boolean): BWTest[Unit] =
        lift(awaitCond(IO(condition), 5.seconds, 50.millis).attempt.void)

    // ===================================
    // Bob doesn't start block 1 until an event is received
    // ===================================
    val _ = property("Bob doesn't start block 1 until an event is received") = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          _ <- mkBlockWeaverActor(Bob.headPeerNumber)
          _ <- lift(env.system.waitForIdle())
          startBlockNums <- lift(IO(env.jointLedgerMock.startBlockNums.get))
          _ <- assertWith(
            startBlockNums.isEmpty,
            s"No StartBlock is expected, but $startBlockNums found"
          )
      } yield true
    )

    // ===================================
    // Carol (2) doesn't start block 2 until the first request after the block 1 brief
    // ===================================
    val _ = property(
      "Carol (2) doesn't start block 2 until the first request after the block 1 brief"
    ) = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          anyRequest <- pick(genUserRequest(env.multiNodeConfig))
          weaver <- mkBlockWeaverActor(Carol.headPeerNumber)
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          brief <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift(weaver ! brief)
          _ <- lift(env.system.waitForIdle())
          _ <- settle(env.jointLedgerMock.startBlockNums.get == Vector(BlockNumber(1)))
          startBlockNumsBeforeRequest <- lift(IO(env.jointLedgerMock.startBlockNums.get))
          _ <- assertWith(
            startBlockNumsBeforeRequest == Vector(BlockNumber(1)),
            "Only the block 1 StartBlock is expected before any request," +
                s" but $startBlockNumsBeforeRequest found"
          )
          _ <- lift((weaver ! anyRequest) >> env.system.waitForIdle())
          _ <- settle(
            env.jointLedgerMock.startBlockNums.get == Vector(BlockNumber(1), BlockNumber(2))
          )
          startBlockNumsAfterRequest <- lift(IO(env.jointLedgerMock.startBlockNums.get))
          _ <- assertWith(
            startBlockNumsAfterRequest == Vector(BlockNumber(1), BlockNumber(2)),
            "Block 2 StartBlock is expected after the first request," +
                s" but $startBlockNumsAfterRequest found"
          )
      } yield true
    )

    // ===================================
    // Bob finishes first block after receiving any event
    // ===================================
    val _ = property("Bob finishes first block after receiving any event") = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          anyLedgerEvent <- pick(
            genUserRequest(env.multiNodeConfig).label("any event to finish the first block")
          )
          weaver <- mkBlockWeaverActor(Bob.headPeerNumber)
          _ <- lift(weaver ! anyLedgerEvent)
          _ <- lift(env.system.waitForIdle())
          _ <- lift(expectMsgPF(env.jointLedgerMockActor, 5.seconds) {
              case CompleteBlockRegular(None, _, _, _) => ()
          })
      } yield true
    )

    // ===================================
    // Carol (2) leads block (2), feeding residual requests in order
    // ===================================
    val _ = property("Carol (2) leads block (2), feeding residual requests in order") = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          requests <- pick(
            Gen
                .nonEmptyListOf(genUserRequest(env.multiNodeConfig))
                .map(_.distinctBy(_.requestId))
                .label("random user requests")
          )
          weaver <- mkBlockWeaverActor(Carol.headPeerNumber)
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          _ <- lift(requests.traverse_(weaver ! _))
          brief <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift(weaver ! brief)
          _ <- lift(env.system.waitForIdle())
          now <- lift(realTimeQuantizedInstant(slotConfig = config.slotConfig))
          isAroundNow = (other: Instant) =>
              now.instant.toEpochMilli - 10.second.toMillis < other.toEpochMilli &&
                  now.instant.toEpochMilli + 10.second.toMillis > other.toEpochMilli
          _ <- lift(expectMsgPF(env.jointLedgerMockActor, 5.second) {
              case s: StartBlock
                  if s.blockNum == BlockNumber(2) && isAroundNow(s.blockCreationStartTime.instant) =>
                  ()
          })
          _ <- settle(env.jointLedgerMock.events.get == requests)
          fedRequests <- lift(IO(env.jointLedgerMock.events.get))
          _ <- assertWith(
            fedRequests == requests,
            "feed all residual/recovered mempool requests in order: " +
                s"expected ${requests.map(_.requestId)}, got ${fedRequests.map(_.requestId)}"
          )
      } yield true
    )

    // ===================================
    // Carol (2) leads block (2), feeding new requests in order
    // ===================================
    val _ = property("Carol (2) leads block (2), feeding new requests in order") = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          events <- pick(
            Gen
                .nonEmptyListOf(genUserRequest(env.multiNodeConfig))
                .map(_.distinctBy(_.requestId))
          )
          weaver <- mkBlockWeaverActor(Carol.headPeerNumber)
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          brief <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift(weaver ! brief)
          _ <- lift(events.traverse_ { e =>
              (weaver ! e) >>
                  awaitCond(
                    IO(env.jointLedgerMock.events.get.lastOption.contains(e)),
                    5.seconds,
                    50.millis
                  ).attempt.void
          })
          allContained <- lift(IO(events.forall(e => env.jointLedgerMock.events.get.contains(e))))
          _ <- assertWith(allContained, "events are passed through with no delay")
      } yield true
    )

    // ===================================
    // Carol (2) does NOT start block 2 when only previous block brief is received
    // ===================================
    val _ = property(
      "Carol (2) does NOT start block 2 when only previous block brief is received"
    ) = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          weaver <- mkBlockWeaverActor(Carol.headPeerNumber)
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          brief <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift((weaver ! brief) >> env.system.waitForIdle())
          // Reproducing block 1 sends its StartBlock, but block 2 must not be started
          // (only brief, no request)
          _ <- settle(env.jointLedgerMock.startBlockNums.get == Vector(BlockNumber(1)))
          startBlockNums <- lift(IO(env.jointLedgerMock.startBlockNums.get))
          _ <- assertWith(
            startBlockNums == Vector(BlockNumber(1)),
            "Only the block 1 StartBlock is expected when only brief received," +
                s" but $startBlockNums found"
          )
          eventsEmpty <- lift(IO(env.jointLedgerMock.events.get.isEmpty))
          _ <- assertWith(eventsEmpty, "No events should be fed when only brief is received")
      } yield true
    )

    // ===================================
    // Carol (2) starts block 2 when previous block brief then request received
    // ===================================
    val _ = property(
      "Carol (2) starts block 2 when previous block brief then request received"
    ) = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          anyLedgerEvent <- pick(
            genUserRequest(env.multiNodeConfig).label("any event to start the block")
          )
          weaver <- mkBlockWeaverActor(Carol.headPeerNumber)
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          brief <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift(weaver ! brief)
          _ <- lift(weaver ! anyLedgerEvent)
          _ <- lift(env.system.waitForIdle())
          _ <- lift(expectMsgPF(env.jointLedgerMockActor, 5.seconds) {
              case s: StartBlock if s.blockNum == BlockNumber(2) => ()
          })
          _ <- settle(env.jointLedgerMock.events.get.contains(anyLedgerEvent))
          eventsContainsRequest <- lift(IO(env.jointLedgerMock.events.get.contains(anyLedgerEvent)))
          _ <- assertWith(eventsContainsRequest, "Event should be fed through to joint ledger")
      } yield true
    )

    // ===================================
    // Carol (2) starts block 2 when request then previous block brief received
    // ===================================
    val _ = property(
      "Carol (2) starts block 2 when request then previous block brief received"
    ) = run(
      resource = defaultResource,
      testM = for {
          env <- ask
          anyLedgerEvent <- pick(
            genUserRequest(env.multiNodeConfig).label("any event before brief")
          )
          weaver <- mkBlockWeaverActor(Carol.headPeerNumber)
          config = env.multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
          _ <- lift(weaver ! anyLedgerEvent)
          brief <- mkDummyBlockBrief1(config.headConfig)
          _ <- lift(weaver ! brief)
          _ <- lift(env.system.waitForIdle())
          _ <- lift(expectMsgPF(env.jointLedgerMockActor, 5.seconds) {
              case s: StartBlock if s.blockNum == BlockNumber(2) => ()
          })
          _ <- settle(env.jointLedgerMock.events.get.contains(anyLedgerEvent))
          eventsContainsRequest <- lift(IO(env.jointLedgerMock.events.get.contains(anyLedgerEvent)))
          _ <- assertWith(eventsContainsRequest, "Event should be fed through to joint ledger")
      } yield true
    )
}
