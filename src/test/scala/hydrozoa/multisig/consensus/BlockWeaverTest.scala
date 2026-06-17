package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import com.suprnation.actor.test.TestKit
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.UserRequest.TransactionRequest
import hydrozoa.multisig.consensus.UserRequestBody.TransactionRequestBody
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import org.scalacheck.Prop.propBoolean
import org.scalacheck.PropertyM.*
import org.scalacheck.{Arbitrary, Gen, Properties, Test}
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.{Blake2b_256, Hash}
import scalus.uplc.builtin.ByteString
import test.Generators.Hydrozoa.genRequestId
import test.TestPeerName.{Bob, Carol}
import test.{PeersNumberSpec, TestPeersSpec}

object BlockWeaverTest extends Properties("Block weaver test"), TestKit {
    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p.withMinSuccessfulTests(100)
            .withWorkers(1)
    }

    /** Poll until the condition holds, giving up silently after a timeout. `waitForIdle` observes
      * each actor's idle state independently and can return while a message is still in flight to
      * the joint ledger mock (likewise, `expectMsgPF` matches on the tracked message buffer before
      * the mock processes the messages that follow), so a one-shot read of the mock's state right
      * after either of them can be stale. The caller asserts the condition, with a readable
      * message, after settling.
      */
    def settleOnIO(condition: => Boolean): IO[Unit] =
        awaitCond(IO(condition), 5.seconds, 50.millis).attempt.void

    val multiNodeConfig: MultiNodeConfig = {
        println("Generating multi peer node config (3 peers)...")
        MultiNodeConfig
            .generate(
              TestPeersSpec.default.withPeersNumberSpec(PeersNumberSpec.Exact(3))
            )()
            .sample
            .get
    }

    // In block weaver test we don't care about the content of user requests.
    // What we are interested in is request ids though.
    given dummyUserRequestWithId: Arbitrary[UserRequestWithId] = Arbitrary {
        val zeroQI = QuantizedInstant(multiNodeConfig.slotConfig, Instant.ofEpochSecond(0))
        for {
            requestId <- genRequestId
            userRequest = TransactionRequest(
              header = UserRequestHeader(
                headId = multiNodeConfig.headConfig.headId,
                validityStart = RequestValidityStartTime(zeroQI),
                validityEnd = RequestValidityEndTime(zeroQI),
                bodyHash = Hash[Blake2b_256, Any](ByteString.fromArray(Array.fill[Byte](32)(0)))
              ),
              body = TransactionRequestBody(ByteString.empty),
              userVk = multiNodeConfig.headConfig.headPeerVKeys.head
            )
        } yield UserRequestWithId(userRequest = userRequest, requestId = requestId)
    }

    case class TestSetup(
        system: ActorSystem[IO],
        jointLedgerMock: JointLedgerMock,
        jointLedgerMockActor: JointLedger.Handle
    )

    def mkTestSetup(): Resource[IO, TestSetup] =
        for
            system <- ActorSystem[IO]("Weaver SUT")
            jointLedgerMock = JointLedgerMock()
            jointLedgerMockActor <- Resource.eval(
              system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock"))
            )
        yield TestSetup(system, jointLedgerMock, jointLedgerMockActor)

    def mkBlockWeaverActor(
        system: ActorSystem[IO],
        jointLedgerMockActor: JointLedger.Handle,
        peerNumber: HeadPeerNumber
    ): IO[BlockWeaver.Handle] = {
        val config = multiNodeConfig.nodeConfigs(peerNumber)
        val connections = BlockWeaver.ConnectionsPartial(jointLedgerMockActor)
        system.actorOf(
          BlockWeaver(config, connections, ContraTracer.nullTracer[IO, BlockWeaverEvent])
        )
    }

    // Empty block brief for block 1, so Carol starts working on block 2
    def mkDummyBlockBrief1(config: HeadConfig): IO[BlockBrief.Next] =
        for {
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
        }

    // ===================================
    // Bob doesn't start block 1 until an event is received
    // ===================================

    val _ = property("Bob doesn't start block 1 until an event is received") = monadicIO(for {
        _ <- run[IO, Unit](mkTestSetup().use {
            case TestSetup(system, jointLedgerMock, jointLedgerMockActor) =>
                for {
                    _ <- mkBlockWeaverActor(system, jointLedgerMockActor, Bob.headPeerNumber)
                    _ <- system.waitForIdle()
                    _ <- IO {
                        val startBlockNums = jointLedgerMock.startBlockNums.get
                        require(
                          startBlockNums.isEmpty,
                          s"No StartBlock is expected, but $startBlockNums found"
                        )
                    }
                } yield ()
        })
    } yield true)

    // ===================================
    // Carol (2) doesn't start block 2 until the first request after the block 1 brief
    // ===================================

    val _ = property(
      "Carol (2) doesn't start block 2 until the first request after the block 1 brief"
    ) = {
        val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
        monadicIO(for {
            anyRequest <- pick[IO, UserRequestWithId](Arbitrary.arbitrary)
            _ <- run[IO, Unit](mkTestSetup().use {
                case TestSetup(system, jointLedgerMock, jointLedgerMockActor) =>
                    for {
                        blockWeaver <- mkBlockWeaverActor(
                          system,
                          jointLedgerMockActor,
                          Carol.headPeerNumber
                        )
                        brief <- mkDummyBlockBrief1(config.headConfig)
                        _ <- blockWeaver ! brief
                        _ <- system.waitForIdle()
                        _ <- settleOnIO(
                          jointLedgerMock.startBlockNums.get == Vector(BlockNumber(1))
                        )
                        _ <- IO {
                            val startBlockNumsBeforeRequest = jointLedgerMock.startBlockNums.get
                            require(
                              startBlockNumsBeforeRequest == Vector(BlockNumber(1)),
                              "Only the block 1 StartBlock is expected before any request," +
                                  s" but $startBlockNumsBeforeRequest found"
                            )
                        }
                        _ <- (blockWeaver ! anyRequest) >> system.waitForIdle()
                        _ <- settleOnIO(
                          jointLedgerMock.startBlockNums.get == Vector(
                            BlockNumber(1),
                            BlockNumber(2)
                          )
                        )
                        _ <- IO {
                            val startBlockNumsAfterRequest = jointLedgerMock.startBlockNums.get
                            require(
                              startBlockNumsAfterRequest == Vector(BlockNumber(1), BlockNumber(2)),
                              "Block 2 StartBlock is expected after the first request," +
                                  s" but $startBlockNumsAfterRequest found"
                            )
                        }
                    } yield ()
            })
        } yield true)
    }

    // ===================================
    // Bob finishes first block after receiving any event
    // ===================================

    val _ = property("Bob finishes first block after receiving any event") = monadicIO(for {
        anyLedgerEvent <- pick[IO, UserRequestWithId](
          Arbitrary.arbitrary[UserRequestWithId].label("any event to finish the first block")
        )
        _ <- run[IO, Unit](mkTestSetup().use {
            case TestSetup(system, jointLedgerMock, jointLedgerMockActor) =>
                for {
                    weaverActor <- mkBlockWeaverActor(
                      system,
                      jointLedgerMockActor,
                      Bob.headPeerNumber
                    )
                    _ <- weaverActor ! anyLedgerEvent
                    _ <- system.waitForIdle()
                    _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) {
                        case CompleteBlockRegular(None, _, _, _) => ()
                    }
                } yield ()
        })
    } yield true)

    // ===================================
    // Carol (2) leads block (2), feeding residual requests in order
    // ===================================

    val _ = property(
      "Carol (2) leads block (2), feeding residual requests in order"
    ) = {
        val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
        monadicIO(for {
            requests <- pick[IO, List[UserRequestWithId]](
              Gen
                  .nonEmptyListOf(Arbitrary.arbitrary[UserRequestWithId])
                  .map(_.distinctBy(_.requestId))
                  .label("random user requests")
            )
            _ <- run[IO, Unit](
              mkTestSetup().use { case TestSetup(system, jointLedgerMock, jointLedgerMockActor) =>
                  for {
                      weaverActor <- mkBlockWeaverActor(
                        system,
                        jointLedgerMockActor,
                        Carol.headPeerNumber
                      )
                      _ <- requests.traverse_(weaverActor ! _)
                      brief <- mkDummyBlockBrief1(config.headConfig)
                      _ <- weaverActor ! brief
                      _ <- system.waitForIdle()
                      now <- realTimeQuantizedInstant(slotConfig = config.slotConfig)
                      isAroundNow = (other: Instant) =>
                          now.instant.toEpochMilli - 10.second.toMillis < other.toEpochMilli &&
                              now.instant.toEpochMilli + 10.second.toMillis > other.toEpochMilli
                      _ <- expectMsgPF(jointLedgerMockActor, 5.second) {
                          case s: StartBlock
                              if s.blockNum == BlockNumber(2) &&
                                  isAroundNow(s.blockCreationStartTime.instant) =>
                              ()
                      }
                      _ <- settleOnIO(jointLedgerMock.events.get == requests)
                      _ <- IO {
                          val fedRequests = jointLedgerMock.events.get
                          require(
                            fedRequests == requests,
                            "feed all residual/recovered mempool requests in order: " +
                                s"expected ${requests.map(_.requestId)}, got ${fedRequests.map(_.requestId)}"
                          )
                      }
                  } yield ()
              }
            )
        } yield true)
    }

    // ===================================
    // Carol (2) leads block (2), feeding new requests in order
    // ===================================
    val _ = property("Carol (2) leads block (2), feeding new requests in order") = {
        val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
        monadicIO(for {
            events <- pick[IO, List[UserRequestWithId]](
              Gen
                  .nonEmptyListOf(Arbitrary.arbitrary[UserRequestWithId])
                  .map(_.distinctBy(_.requestId))
            )
            _ <- run[IO, Unit](mkTestSetup().use {
                case TestSetup(system, jointLedgerMock, jointLedgerMockActor) =>
                    for {
                        weaverActor <- mkBlockWeaverActor(
                          system,
                          jointLedgerMockActor,
                          Carol.headPeerNumber
                        )
                        brief <- mkDummyBlockBrief1(config.headConfig)
                        _ <- weaverActor ! brief
                        _ <- events.traverse_ { e =>
                            (weaverActor ! e) >>
                                settleOnIO(jointLedgerMock.events.get.lastOption.contains(e))
                        }
                        _ <- IO {
                            require(
                              events.forall(e => jointLedgerMock.events.get.contains(e)),
                              "events are passed through with no delay"
                            )
                        }
                    } yield ()
            })
        } yield true)
    }

    // ===================================
    // Carol (2) does NOT start block 2 when only previous block brief is received
    // ===================================
    val _ = property(
      "Carol (2) does NOT start block 2 when only previous block brief is received"
    ) = {
        val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
        monadicIO(for {
            _ <- run[IO, Unit](mkTestSetup().use {
                case TestSetup(system, jointLedgerMock, jointLedgerMockActor) =>
                    for {
                        weaverActor <- mkBlockWeaverActor(
                          system,
                          jointLedgerMockActor,
                          Carol.headPeerNumber
                        )
                        brief <- mkDummyBlockBrief1(config.headConfig)
                        _ <- (weaverActor ! brief) >> system.waitForIdle()
                        // Reproducing block 1 sends its StartBlock, but block 2 must not be started
                        // (only brief, no request)
                        _ <- settleOnIO(
                          jointLedgerMock.startBlockNums.get == Vector(BlockNumber(1))
                        )
                        _ <- IO {
                            val startBlockNums = jointLedgerMock.startBlockNums.get
                            require(
                              startBlockNums == Vector(BlockNumber(1)),
                              "Only the block 1 StartBlock is expected when only brief received," +
                                  s" but $startBlockNums found"
                            )
                            require(
                              jointLedgerMock.events.get.isEmpty,
                              "No events should be fed when only brief is received"
                            )
                        }
                    } yield ()
            })
        } yield true)
    }

    // ===================================
    // Carol (2) starts block 2 when previous block brief then request received
    // ===================================
    val _ = property(
      "Carol (2) starts block 2 when previous block brief then request received"
    ) = {
        val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
        monadicIO(for {
            anyLedgerEvent <- pick[IO, UserRequestWithId](
              Arbitrary.arbitrary[UserRequestWithId].label("any event to start the block")
            )
            _ <- run[IO, Unit](mkTestSetup().use {
                case TestSetup(system, jointLedgerMock, jointLedgerMockActor) =>
                    for {
                        weaverActor <- mkBlockWeaverActor(
                          system,
                          jointLedgerMockActor,
                          Carol.headPeerNumber
                        )
                        brief <- mkDummyBlockBrief1(config.headConfig)
                        _ <- weaverActor ! brief
                        _ <- weaverActor ! anyLedgerEvent
                        _ <- system.waitForIdle()
                        _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) {
                            case s: StartBlock if s.blockNum == BlockNumber(2) => ()
                        }
                        _ <- settleOnIO(
                          jointLedgerMock.events.get.contains(anyLedgerEvent)
                        )
                        _ <- IO {
                            require(
                              jointLedgerMock.events.get.contains(anyLedgerEvent),
                              "Event should be fed through to joint ledger"
                            )
                        }
                    } yield ()
            })
        } yield true)
    }

    // ===================================
    // Carol (2) starts block 2 when request then previous block brief received
    // ===================================
    val _ = property(
      "Carol (2) starts block 2 when request then previous block brief received"
    ) = {
        val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)
        monadicIO(for {
            anyLedgerEvent <- pick[IO, UserRequestWithId](
              Arbitrary.arbitrary[UserRequestWithId].label("any event before brief")
            )
            _ <- run[IO, Unit](mkTestSetup().use {
                case TestSetup(system, jointLedgerMock, jointLedgerMockActor) =>
                    for {
                        weaverActor <- mkBlockWeaverActor(
                          system,
                          jointLedgerMockActor,
                          Carol.headPeerNumber
                        )
                        _ <- weaverActor ! anyLedgerEvent
                        brief <- mkDummyBlockBrief1(config.headConfig)
                        _ <- weaverActor ! brief
                        _ <- system.waitForIdle()
                        _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) {
                            case s: StartBlock if s.blockNum == BlockNumber(2) => ()
                        }
                        _ <- settleOnIO(
                          jointLedgerMock.events.get.contains(anyLedgerEvent)
                        )
                        _ <- IO {
                            require(
                              jointLedgerMock.events.get.contains(anyLedgerEvent),
                              "Event should be fed through to joint ledger"
                            )
                        }
                    } yield ()
            })
        } yield true)
    }

    // TODO: finalization
    // TODO: follower mode

    class JointLedgerMock extends Actor[IO, JointLedger.Requests.Request]:

        val events: AtomicReference[Vector[UserRequestWithId]] = AtomicReference(Vector.empty)
        val startBlockNums: AtomicReference[Vector[BlockNumber]] = AtomicReference(Vector.empty)

        override def receive: Receive[IO, JointLedger.Requests.Request] = {
            case e: UserRequestWithId =>
                // IO.println(s"mock: LedgerEvent: $e") >>
                IO { events.updateAndGet(_ :+ e) }
            case s: StartBlock =>
                // IO.println(s"mock: StartBlock: $s") >>
                IO { startBlockNums.updateAndGet(_ :+ s.blockNum) }
            case c: CompleteBlockRegular =>
                // IO.println(s"mock: CompleteBlockRegular: ${c.referenceBlock}") >>
                IO.pure(())
            case f: CompleteBlockFinal =>
                // IO.println("mock:CompleteBlockFinal") >>
                IO.pure(())
        }
}
