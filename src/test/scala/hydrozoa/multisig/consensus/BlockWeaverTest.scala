package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import com.suprnation.actor.test.TestKit
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.consensus.UserRequest.TransactionRequest
import hydrozoa.multisig.consensus.UserRequestBody.TransactionRequestBody
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.commitment.KzgCommitment
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import org.scalacheck.{Arbitrary, Gen, Properties, PropertyBuilder, Test}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.{Blake2b_256, Hash}
import scalus.crypto.ed25519.Signature
import scalus.uplc.builtin.ByteString
import test.Generators.Hydrozoa.genRequestId
import test.TestPeerName.{Bob, Carol}
import test.{PeersNumberSpec, TestPeersSpec}

object BlockWeaverTest extends Properties("Block weaver test"), TestKit {
    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p.withMinSuccessfulTests(100)
            .withWorkers(1)
    }

    // TODO: check that assertion labels are printed
    // TODO: move to PropertyBuilder
    def handleBoolean(comp: IO[Unit]): IO[Boolean] =
        (comp >> IO.pure(true)).handleErrorWith(e => IO.println(s"exception: $e") >> IO.pure(false))

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
        for {
            requestId <- genRequestId
            userRequest = TransactionRequest(
              header = UserRequestHeader(
                headId = multiNodeConfig.headConfig.headId,
                validityStart = RequestValidityStartTimeRaw.apply(0),
                validityEnd = RequestValidityEndTimeRaw.apply(0),
                bodyHash = Hash[Blake2b_256, Any](ByteString.fromArray(Array.fill[Byte](32)(0)))
              ),
              body = TransactionRequestBody(ByteString.empty),
              userVk = multiNodeConfig.headConfig.headPeerVKeys.head,
              signature = Signature.unsafeFromArray(Array.fill[Byte](64)(0))
            ).toOption.get
        } yield UserRequestWithId(userRequest = userRequest, requestId = requestId)
    }

    // Setup helper for tests
    case class TestSetup(
        system: ActorSystem[IO],
        jointLedgerMock: JointLedgerMock,
        jointLedgerMockActor: JointLedger.Handle
    )

    def setupTestEnvironment(p: PropertyBuilder[?]): TestSetup = {
        val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
        val jointLedgerMock = JointLedgerMock()
        val jointLedgerMockActor: JointLedger.Handle =
            p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))
        TestSetup(system, jointLedgerMock, jointLedgerMockActor)
    }

    def createBlockWeaverActor(
        p: PropertyBuilder[?],
        system: ActorSystem[IO],
        jointLedgerMockActor: JointLedger.Handle,
        peerNumber: HeadPeerNumber
    ): BlockWeaver.Handle = {
        val config = multiNodeConfig.nodeConfigs(peerNumber)
        val connections = BlockWeaver.ConnectionsPartial(jointLedgerMockActor)
        p.runIO(system.actorOf(BlockWeaver(config, connections)))
    }

    // Empty block brief for block 1, so Carol starts working on block 2
    def mkDummyBlockBrief1(config: HeadConfig): IO[BlockBrief.Next] =
        for {
            now <- realTimeQuantizedInstant(config.slotConfig)
        } yield {
            val blockCreationStartTime = BlockCreationStartTime(now)
            val blockCreationEndTime = BlockCreationEndTime(now + 1.second)
            val fallbackTxStartTime = config.txTiming.newFallbackStartTime(blockCreationEndTime)
            val forcedMajorBlockTime = config.txTiming.forcedMajorBlockTime(fallbackTxStartTime)
            val majorBlockWakeupTime = TxTiming.majorBlockWakeupTime(forcedMajorBlockTime, None)
            BlockBrief.Minor(
              BlockHeader.Minor(
                blockNum = BlockNumber(1),
                blockVersion = BlockVersion.Full(0, 0),
                startTime = blockCreationStartTime,
                endTime = blockCreationEndTime,
                kzgCommitment = KzgCommitment.empty,
                fallbackTxStartTime = fallbackTxStartTime,
                majorBlockWakeupTime = majorBlockWakeupTime
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

    val _ = property("Bob doesn't start block 1 until an event is received") =
        PropertyBuilder.property { p =>

            val TestSetup(system, jointLedgerMock, jointLedgerMockActor) = setupTestEnvironment(p)

            p.runIO(for {
                _ <- IO.pure(
                  createBlockWeaverActor(p, system, jointLedgerMockActor, Bob.headPeerNumber)
                )
                _ <- system.waitForIdle()
            } yield ())

            val startBlockCounter = jointLedgerMock.startBlocksCounter.get
            p.assert(
              startBlockCounter == 0,
              s"No StartBlock is expected, but ${startBlockCounter} found"
            )

            true
        }

    // ===================================
    // Carol doesn't start block 2 until an event is received
    // ===================================

    val _ = property("Carol doesn't start block 2 until an event is received") =
        PropertyBuilder.property { p =>

            val TestSetup(system, jointLedgerMock, jointLedgerMockActor) = setupTestEnvironment(p)

            val blockWeaver =
                createBlockWeaverActor(p, system, jointLedgerMockActor, Carol.headPeerNumber)
            val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)

            // Block 1 brief
            p.runIO(for {
                brief <- mkDummyBlockBrief1(config.headConfig)
                _ <- blockWeaver ! brief
                _ <- system.waitForIdle()
            } yield ())

            val startBlockCounter = jointLedgerMock.startBlocksCounter.get
            p.assert(
              startBlockCounter == 1,
              s"Only block 1 StartBlock is expected, but ${startBlockCounter} found"
            )

            true
        }

    // ===================================
    // Bob finishes first block after receiving any event
    // ===================================

    val _ = property("Bob finishes first block after receiving any event") =
        PropertyBuilder.property { p =>

            val TestSetup(system, jointLedgerMock, jointLedgerMockActor) = setupTestEnvironment(p)

            val weaverActor =
                createBlockWeaverActor(p, system, jointLedgerMockActor, Bob.headPeerNumber)

            val anyLedgerEvent =
                p.pick(
                  Arbitrary.arbitrary[UserRequestWithId],
                  "any event to finish the first block"
                )

            p.assert(
              p.runIO(
                handleBoolean(for {
                    _ <- weaverActor ! anyLedgerEvent
                    _ <- system.waitForIdle()
                    _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) {
                        case CompleteBlockRegular(None, _, _, _) => ()
                    }
                } yield ())
              )
            )

            true
        }

    // ===================================
    // Carol (2) leads block (2), feeding residual requests in order
    // ===================================

    val _ = property(
      "Carol (2) leads block (2), feeding residual requests in order"
    ) = PropertyBuilder.property { p =>

        val TestSetup(system, jointLedgerMock, jointLedgerMockActor) = setupTestEnvironment(p)

        val weaverActor =
            createBlockWeaverActor(p, system, jointLedgerMockActor, Carol.headPeerNumber)
        val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)

        // Generate and send some random requests
        val requests: Seq[UserRequestWithId] = p.pick(
          Gen.nonEmptyListOf(Arbitrary.arbitrary[UserRequestWithId]).map(_.distinctBy(_.requestId)),
          "random user requests"
        )
        p.runIO(IO.traverse_(requests)(weaverActor ! _))

        // Brief for block 1
        val brief = p.runIO(mkDummyBlockBrief1(config.headConfig))
        p.runIO(weaverActor ! brief)
        p.runIO(system.waitForIdle())

        def aroundNow(other: Instant): Boolean = {
            val now = p.runIO(realTimeQuantizedInstant(slotConfig = config.slotConfig))
            // println(now)
            // println(other)
            now.instant.toEpochMilli - 10.second.toMillis < other.toEpochMilli &&
            now.instant.toEpochMilli + 10.second.toMillis > other.toEpochMilli
        }

        // then ...
        p.assert(
          p.runIO(handleBoolean(expectMsgPF(jointLedgerMockActor, 5.second) {
              case s: StartBlock if aroundNow(s.blockCreationStartTime.instant) => ()
          })),
          "weaver should start the block with sensible creation time."
        )

        p.assert(
          jointLedgerMock.events.toSeq == requests,
          s"feed all residual/recovered mempool requests: expected ${requests.size}, got: ${jointLedgerMock.events.size}"
        )

        p.runIO(system.terminate())

        // Done
        true
    }

    // ===================================
    // Carol (2) leads block (2), feeding new requests in order
    // ===================================
    val _ = property("Carol (2) leads block (2), feeding new requests in order") =
        PropertyBuilder.property { p =>

            val TestSetup(system, jointLedgerMock, jointLedgerMockActor) = setupTestEnvironment(p)

            val weaverActor =
                createBlockWeaverActor(p, system, jointLedgerMockActor, Carol.headPeerNumber)
            val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)

            // Brief for block 1
            val brief = p.runIO(mkDummyBlockBrief1(config.headConfig))
            p.runIO(weaverActor ! brief)

            // Any random events
            val events: Seq[UserRequestWithId] =
                p.pick(Gen.nonEmptyListOf(Arbitrary.arbitrary[UserRequestWithId]))

            // Should be passed through with no delay
            p.assert(
              events
                  .map { e =>
                      p.runIO(
                        handleBoolean(for {
                            _ <- weaverActor ! e
                            _ <- system.waitForIdle()
                        } yield ())
                      ) &&
                      (jointLedgerMock.events.last == e)
                  }
                  .forall(identity),
              "events are passed through with no delay"
            )

            true
        }

    // ===================================
    // Carol (2) does NOT start block 2 when only previous block brief is received
    // ===================================
    val _ = property(
      "Carol (2) does NOT start block 2 when only previous block brief is received"
    ) = PropertyBuilder.property { p =>

        val TestSetup(system, jointLedgerMock, jointLedgerMockActor) = setupTestEnvironment(p)

        val weaverActor =
            createBlockWeaverActor(p, system, jointLedgerMockActor, Carol.headPeerNumber)
        val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)

        // Brief for block 1
        val brief = p.runIO(mkDummyBlockBrief1(config.headConfig))
        p.runIO((weaverActor ! brief) >> system.waitForIdle())

        // Should NOT have sent StartBlock for block 2 (only brief, no request)
        val startBlockCounter = jointLedgerMock.startBlocksCounter.get
        p.assert(
          startBlockCounter == 0,
          s"No StartBlock is expected when only brief received, but ${startBlockCounter} found"
        )

        // Check that no events were fed (since we didn't send any user requests)
        p.assert(
          jointLedgerMock.events.isEmpty,
          "No events should be fed when only brief is received"
        )

        p.runIO(system.terminate())

        true
    }

    // ===================================
    // Carol (2) starts block 2 when previous block brief then request received
    // ===================================
    val _ = property(
      "Carol (2) starts block 2 when previous block brief then request received"
    ) = PropertyBuilder.property { p =>

        val TestSetup(system, jointLedgerMock, jointLedgerMockActor) = setupTestEnvironment(p)

        val weaverActor =
            createBlockWeaverActor(p, system, jointLedgerMockActor, Carol.headPeerNumber)
        val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)

        // Brief for block 1 first
        val brief = p.runIO(mkDummyBlockBrief1(config.headConfig))
        p.runIO(weaverActor ! brief)

        // Then send a user request
        val anyLedgerEvent =
            p.pick(
              Arbitrary.arbitrary[UserRequestWithId],
              "any event to start the block"
            )

        p.assert(
          p.runIO(
            handleBoolean(for {
                _ <- weaverActor ! anyLedgerEvent
                _ <- system.waitForIdle()
                _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) { case s: StartBlock =>
                    ()
                }
            } yield ())
          ),
          "StartBlock should be sent after both brief and request are received"
        )

        // Verify the event was fed through
        p.assert(
          jointLedgerMock.events.contains(anyLedgerEvent),
          "Event should be fed through to joint ledger"
        )

        p.runIO(system.terminate())

        true
    }

    // ===================================
    // Carol (2) starts block 2 when request then previous block brief received
    // ===================================
    val _ = property(
      "Carol (2) starts block 2 when request then previous block brief received"
    ) = PropertyBuilder.property { p =>

        val TestSetup(system, jointLedgerMock, jointLedgerMockActor) = setupTestEnvironment(p)

        val weaverActor =
            createBlockWeaverActor(p, system, jointLedgerMockActor, Carol.headPeerNumber)
        val config = multiNodeConfig.nodeConfigs(Carol.headPeerNumber)

        // Send a user request first
        val anyLedgerEvent =
            p.pick(
              Arbitrary.arbitrary[UserRequestWithId],
              "any event before brief"
            )
        p.runIO(weaverActor ! anyLedgerEvent)

        // Then send brief for block 1
        val brief = p.runIO(mkDummyBlockBrief1(config.headConfig))

        p.assert(
          p.runIO(
            handleBoolean(for {
                _ <- weaverActor ! brief
                _ <- system.waitForIdle()
                _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) { case s: StartBlock =>
                    ()
                }
            } yield ())
          ),
          "StartBlock should be sent after both request and brief are received (reverse order)"
        )

        // Verify the event was fed through
        p.assert(
          jointLedgerMock.events.contains(anyLedgerEvent),
          "Event should be fed through to joint ledger"
        )

        p.runIO(system.terminate())

        true
    }

    // TODO: finalization
    // TODO: follower mode

    // val _ = property(
    //  "Follower/awaiting modes are working as expected"
    // ) = PropertyBuilder.property { p =>
    //
    //    // Since we are going to check two blocks in a row not to peek into the weaver's state,
    //    // we need at least four peers so the SUT weaver won't become a leader at least for three
    //    // blocks (since we don't want its leader StartBlock gets into our way).
    //    val peers = p.pick(genTestPeers(4, 10), "test peers")
    //    val peer = p.pick(Gen.oneOf(peers.toList), "own peer")
    //    val peerId = HeadPeerId(peer.ordinal, peers.size)
    //
    //    // Joint ledger mock
    //    val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
    //    val jointLedgerMock = JointLedgerMock()
    //    val jointLedgerMockActor =
    //        p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))
    //
    //    // Weaver's config such that lastKnownBlock was peer's turn so we have at least 3 blocks
    //    // that the SUT weaver is guaranteed to handle as a follower.
    //    val roundsCompleted = p.pick(Gen.choose(100, 1000), "rounds completed")
    //    val lastKnownBlock = BlockNumber(roundsCompleted * peers.size + peer.ordinal)
    //    val config = BlockWeaver.Config(
    //      // This is
    //      lastKnownBlock = lastKnownBlock,
    //      peerId = peerId,
    //      recoveredMempool = BlockWeaver.Mempool.empty,
    //      slotConfig = testTxBuilderCardanoInfo.slotConfig
    //    )
    //
    //    // println(peerId)
    //    // println(lastKnownBlock)
    //    // println(peerId.isLeader(lastKnownBlock))
    //
    //    val connections = BlockWeaver.Connections(jointLedgerMockActor)
    //
    //    // Weaver
    //    val weaverActor = p.runIO(system.actorOf(BlockWeaver(config, connections)))
    //
    //    // Random ledger events
    //    val eventsTotal = p.pick(Gen.choose(5, 100), "total number of events")
    //    val events: Seq[UserRequestWithId] = p.pick(
    //      Gen.listOfN(eventsTotal, Arbitrary.arbitrary[UserRequestWithId]).map(_.distinctBy(_.eventId)),
    //      "all events"
    //    )
    //
    //    // Block events
    //    val firstBlockEventsNumber =
    //        p.pick(Gen.choose(1, events.length), "number of first block events")
    //    val firstBlockEvents =
    //        p.pick(Gen.pick(firstBlockEventsNumber, events), "first block events")
    //    val secondBlockEvents = p.pick(
    //      Gen.const(events.filterNot(firstBlockEvents.contains)),
    //      "second block events"
    //    )
    //
    //    val eventsDelayed =
    //        p.pick(
    //          Gen.someOf(events).map(es => Random.shuffle(es).toSeq),
    //          "events delayed and shuffled"
    //        )
    //
    //    val immediateEvents =
    //        p.pick(Gen.const(events.filterNot(eventsDelayed.contains)), "events immediate")
    //
    //    val version = p.pick(
    //      genVersion.map((maj, min) => BlockVersion.Full.apply(maj.toInt, min.toInt)),
    //      "first block version"
    //    )
    //
    //    p.assert(
    //      p.runIO(
    //        handleBoolean(
    //          for {
    //              // Pass all immediate events
    //              _ <- IO.traverse_(immediateEvents)(weaverActor ! _)
    //
    //              txTiming = TxTiming.default(testTxBuilderCardanoInfo.slotConfig)
    //
    //              // First block
    //              now <- realTimeQuantizedInstant(testTxBuilderCardanoInfo.slotConfig)
    //              competingFallbackStartTime = txTiming.newFallbackStartTime(now)
    //
    //              firstBlock: BlockBrief.Minor = BlockBrief.Minor(
    //                BlockHeader.Minor(
    //                  blockNum = lastKnownBlock.increment,
    //                  blockVersion = version,
    //                  startTime = now,
    //                  kzgCommitment = KzgCommitment.empty
    //                ),
    //                BlockBody.Minor(
    //                  events = firstBlockEvents.map(e => (e.eventId, Valid)).toList,
    //                  depositsRefunded = List.empty
    //                )
    //              )
    //
    //              // Second block
    //              newTime <- realTimeQuantizedInstant(testTxBuilderCardanoInfo.slotConfig)
    //
    //              secondBlock: BlockBrief.Minor = BlockBrief.Minor(
    //                firstBlock.nextHeaderMinor(newTime, KzgCommitment.empty),
    //                BlockBody
    //                    .Minor(
    //                      events = secondBlockEvents.map(e => (e.eventId, Valid)).toList,
    //                      depositsRefunded = List.empty
    //                    )
    //              )
    //
    //              // _ <- IO.println(s"first block: $firstBlock")
    //              // _ <- IO.println(s"second block: $secondBlock")
    //
    //              _ <- (for {
    //                  _ <- IO.sleep(50.millis)
    //                  _ <- weaverActor ! firstBlock
    //                  _ <- IO.whenA(eventsDelayed.nonEmpty)(
    //                    IO.traverse_(eventsDelayed)(weaverActor ! _)
    //                  )
    //                  _ <- weaverActor ! secondBlock
    //              } yield ()).start.void
    //
    //              // This `expectMsgs` spams to console with Expecting:..., which is "ok" for now
    //              _ <- expectMsgs(jointLedgerMockActor, 10.seconds)(
    //                List(
    //                  StartBlock(firstBlock.header.blockNum, firstBlock.header.startTime)
    //                )
    //                    ++ firstBlockEvents
    //                    ++ List(
    //                      CompleteBlockRegular(Some(firstBlock), Set.empty, false)
    //                    )
    //                    ++ List(
    //                      StartBlock(secondBlock.header.blockNum, secondBlock.header.startTime)
    //                    )
    //                    ++ secondBlockEvents
    //                    ++ List(CompleteBlockRegular(Some(secondBlock), Set.empty, false))*
    //              )
    //          } yield ()
    //        )
    //      )
    //    )
    //
    //    true
    // }

    class JointLedgerMock extends Actor[IO, JointLedger.Requests.Request]:

        val events: mutable.Buffer[UserRequestWithId] = mutable.Buffer.empty
        val startBlocksCounter = AtomicInteger(0)

        override def receive: Receive[IO, JointLedger.Requests.Request] = {
            case e: UserRequestWithId =>
                // IO.println(s"mock: LedgerEvent: $e") >>
                IO { events.append(e) }
            case s: StartBlock =>
                // IO.println(s"mock: StartBlock: $s") >>
                IO.pure(startBlocksCounter.incrementAndGet())
            case c: CompleteBlockRegular =>
                // IO.println(s"mock: CompleteBlockRegular: ${c.referenceBlock}") >>
                IO.pure(())
            case f: CompleteBlockFinal =>
                // IO.println("mock:CompleteBlockFinal") >>
                IO.pure(())
        }
}
