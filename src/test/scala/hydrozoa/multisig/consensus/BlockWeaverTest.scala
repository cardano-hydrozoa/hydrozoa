package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import com.suprnation.actor.test.TestKit
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.consensus.peer.HeadPeerId
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.event.LedgerEvent
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag.Valid
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.genVersion
import java.time.Instant
import org.scalacheck.{Arbitrary, Gen, Properties, PropertyBuilder, Test}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.util.Random
import test.Generators.Hydrozoa.ArbitraryInstances.given
import test.{genTestPeers, testTxBuilderCardanoInfo}

object BlockWeaverTest extends Properties("Block weaver test"), TestKit {
    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p.withMinSuccessfulTests(100)
            .withWorkers(1)
    }

    val _ = property(
      "Weaver leads when turn comes, feeding events from recovered/residual mempool"
    ) = PropertyBuilder.property { p =>

        val peers = p.pick(genTestPeers(minPeers = 3, maxPeers = 5), "test peers")
        val peer = p.pick(Gen.oneOf(peers.toList))
        val peerId = HeadPeerId(peer.ordinal, peers.size)

        // Either the initialization block or any arbitrary block
        val lastKnownBlock =
            BlockNumber(
              p.pick(
                Gen.frequency(
                  5 -> Gen.const(0),
                  5 -> Gen.choose(1, Int.MaxValue)
                )
              )
            )

        // Joint ledger mock
        val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
        val jointLedgerMock = JointLedgerMock()
        val jointLedgerMockActor =
            p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))

        val events: Seq[LedgerEvent] = p.pick(
          if lastKnownBlock > 0 then
              Gen.nonEmptyListOf(Arbitrary.arbitrary[LedgerEvent]).map(_.distinctBy(_.eventId))
          else Gen.const(Seq.empty),
          "recovered mempool"
        )

        // Weaver's config
        val config = BlockWeaver.Config(
          lastKnownBlock = lastKnownBlock,
          peerId = peerId,
          recoveredMempool = BlockWeaver.Mempool.apply(events),
          slotConfig = testTxBuilderCardanoInfo.slotConfig
        )

        val connections = BlockWeaver.Connections(jointLedgerMockActor)

        // Weaver
        val _ = p.runIO(system.actorOf(BlockWeaver(config, connections)))

        def aroundNow(other: Instant): Boolean = {
            val now = p.runIO(realTimeQuantizedInstant(slotConfig = config.slotConfig))
            now.instant.toEpochMilli - 10.second.toMillis < other.toEpochMilli &&
            now.instant.toEpochMilli + 10.second.toMillis > other.toEpochMilli
        }

        // If the next block is the peer's turn...
        p.pre(peerId.isLeader(lastKnownBlock.increment))

        // then ...
        p.assert(
          p.runIO(handleBoolean(expectMsgPF(jointLedgerMockActor, 5.second) {
              case s: StartBlock if aroundNow(s.blockCreationTime.instant) => ()
          })),
          "weaver should start the block with sensible creation time."
        )

        p.assert(
          jointLedgerMock.events.toSeq == events,
          "feed all residual/recovered mempool events"
        )

        p.runIO(system.terminate())

        // Done
        true
    }

    // TODO: check that assertion labels are printed
    // TODO: move to PropertyBuilder
    def handleBoolean(comp: IO[Unit]): IO[Boolean] =
        (comp >> IO.pure(true)).handleErrorWith(e => IO.println(s"exception: $e") >> IO.pure(false))

    val _ = property("Leader finishes first block immediately after any event") =
        PropertyBuilder.property { p =>

            val peers = p.pick(genTestPeers(), "test peers")
            // The second peer (i=1) or the first one if there is only one
            val leader = peers.tail.headOption.getOrElse(peers.head)
            val peerId = HeadPeerId(leader.ordinal, peers.size)
            val lastKnownBlock = 0

            // Joint ledger mock
            val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
            val jointLedgerMock = JointLedgerMock()
            val jointLedgerMockActor =
                p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))

            // Weaver's config
            val config = BlockWeaver.Config(
              lastKnownBlock = BlockNumber(lastKnownBlock),
              peerId = peerId,
              recoveredMempool = BlockWeaver.Mempool.empty,
              slotConfig = testTxBuilderCardanoInfo.slotConfig
            )

            val connections = BlockWeaver.Connections(jointLedgerMockActor)

            // Weaver
            val weaverActor = p.runIO(system.actorOf(BlockWeaver(config, connections)))

            val _ = p.runIO(system.waitForIdle())

            val anyLedgerEvent =
                p.pick(Arbitrary.arbitrary[LedgerEvent], "any event to finish the first block")

            p.assert(
              p.runIO(
                handleBoolean(for {
                    _ <- weaverActor ! anyLedgerEvent
                    _ <- system.waitForIdle()
                    _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) {
                        case CompleteBlockRegular(None, _, _) => ()
                    }
                } yield ())
              )
            )

            true
        }

    val _ = property("Being leader for non-first block weaver passes-through all ledger events") =
        PropertyBuilder.property { p =>

            val peers = p.pick(genTestPeers(), "test peers")
            val peer = p.pick(Gen.oneOf(peers.toList))
            val peerId = HeadPeerId(peer.ordinal, peers.size)

            // Joint ledger mock
            val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
            val jointLedgerMock = JointLedgerMock()
            val jointLedgerMockActor =
                p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))

            // Weaver's config such that the peer is going to be the leader of the next non-first block
            val roundsCompleted = p.pick(Gen.choose(100, 1000))
            val config = BlockWeaver.Config(
              lastKnownBlock = BlockNumber(roundsCompleted * peers.size + peer.ordinal - 1),
              peerId = peerId,
              recoveredMempool = BlockWeaver.Mempool.empty,
              slotConfig = testTxBuilderCardanoInfo.slotConfig
            )

            val connections = BlockWeaver.Connections(jointLedgerMockActor)

            // Weaver
            val weaverActor = p.runIO(system.actorOf(BlockWeaver(config, connections)))

            // Any random events
            val events: Seq[LedgerEvent] =
                p.pick(Gen.nonEmptyListOf(Arbitrary.arbitrary[LedgerEvent]))

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

    val _ = property(
      "Follower/awaiting modes are working as expected"
    ) = PropertyBuilder.property { p =>

        // Since we are going to check two blocks in a row not to peek into the weaver's state,
        // we need at least four peers so the SUT weaver won't become a leader at least for three
        // blocks (since we don't want its leader StartBlock gets into our way).
        val peers = p.pick(genTestPeers(4, 10), "test peers")
        val peer = p.pick(Gen.oneOf(peers.toList), "own peer")
        val peerId = HeadPeerId(peer.ordinal, peers.size)

        // Joint ledger mock
        val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
        val jointLedgerMock = JointLedgerMock()
        val jointLedgerMockActor =
            p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))

        // Weaver's config such that lastKnownBlock was peer's turn so we have at least 3 blocks
        // that the SUT weaver is guaranteed to handle as a follower.
        val roundsCompleted = p.pick(Gen.choose(100, 1000), "rounds completed")
        val lastKnownBlock = BlockNumber(roundsCompleted * peers.size + peer.ordinal)
        val config = BlockWeaver.Config(
          // This is
          lastKnownBlock = lastKnownBlock,
          peerId = peerId,
          recoveredMempool = BlockWeaver.Mempool.empty,
          slotConfig = testTxBuilderCardanoInfo.slotConfig
        )

        // println(peerId)
        // println(lastKnownBlock)
        // println(peerId.isLeader(lastKnownBlock))

        val connections = BlockWeaver.Connections(jointLedgerMockActor)

        // Weaver
        val weaverActor = p.runIO(system.actorOf(BlockWeaver(config, connections)))

        // Random ledger events
        val eventsTotal = p.pick(Gen.choose(5, 100), "total number of events")
        val events: Seq[LedgerEvent] = p.pick(
          Gen.listOfN(eventsTotal, Arbitrary.arbitrary[LedgerEvent]).map(_.distinctBy(_.eventId)),
          "all events"
        )

        // Block events
        val firstBlockEventsNumber =
            p.pick(Gen.choose(1, events.length), "number of first block events")
        val firstBlockEvents =
            p.pick(Gen.pick(firstBlockEventsNumber, events), "first block events")
        val secondBlockEvents = p.pick(
          Gen.const(events.filterNot(firstBlockEvents.contains)),
          "second block events"
        )

        val eventsDelayed =
            p.pick(
              Gen.someOf(events).map(es => Random.shuffle(es).toSeq),
              "events delayed and shuffled"
            )

        val immediateEvents =
            p.pick(Gen.const(events.filterNot(eventsDelayed.contains)), "events immediate")

        val version = p.pick(
          genVersion.map((maj, min) => BlockVersion.Full.apply(maj.toInt, min.toInt)),
          "first block version"
        )

        p.assert(
          p.runIO(
            handleBoolean(
              for {
                  // Pass all immediate events
                  _ <- IO.traverse_(immediateEvents)(weaverActor ! _)

                  txTiming = TxTiming.default(testTxBuilderCardanoInfo.slotConfig)

                  // First block
                  now <- realTimeQuantizedInstant(testTxBuilderCardanoInfo.slotConfig)
                  competingFallbackStartTime = txTiming.newFallbackStartTime(now)

                  firstBlock: BlockBrief.Minor = BlockBrief.Minor(
                    BlockHeader.Minor(
                      blockNum = lastKnownBlock.increment,
                      blockVersion = version,
                      startTime = now,
                      kzgCommitment = KzgCommitment.empty
                    ),
                    BlockBody.Minor(
                      events = firstBlockEvents.map(e => (e.eventId, Valid)).toList,
                      depositsRefunded = List.empty
                    )
                  )

                  // Second block
                  newTime <- realTimeQuantizedInstant(testTxBuilderCardanoInfo.slotConfig)

                  secondBlock: BlockBrief.Minor = BlockBrief.Minor(
                    firstBlock.nextHeaderMinor(newTime, KzgCommitment.empty),
                    BlockBody
                        .Minor(
                          events = secondBlockEvents.map(e => (e.eventId, Valid)).toList,
                          depositsRefunded = List.empty
                        )
                  )

                  // _ <- IO.println(s"first block: $firstBlock")
                  // _ <- IO.println(s"second block: $secondBlock")

                  _ <- (for {
                      _ <- IO.sleep(50.millis)
                      _ <- weaverActor ! firstBlock
                      _ <- IO.whenA(eventsDelayed.nonEmpty)(
                        IO.traverse_(eventsDelayed)(weaverActor ! _)
                      )
                      _ <- weaverActor ! secondBlock
                  } yield ()).start.void

                  // This `expectMsgs` spams to console with Expecting:..., which is "ok" for now
                  _ <- expectMsgs(jointLedgerMockActor, 10.seconds)(
                    List(
                      StartBlock(firstBlock.header.blockNum, firstBlock.header.startTime)
                    )
                        ++ firstBlockEvents
                        ++ List(
                          CompleteBlockRegular(Some(firstBlock), Set.empty, false)
                        )
                        ++ List(
                          StartBlock(secondBlock.header.blockNum, secondBlock.header.startTime)
                        )
                        ++ secondBlockEvents
                        ++ List(CompleteBlockRegular(Some(secondBlock), Set.empty, false))*
                  )
              } yield ()
            )
          )
        )

        true
    }

    class JointLedgerMock extends Actor[IO, JointLedger.Requests.Request]:

        val events: mutable.Buffer[LedgerEvent] = mutable.Buffer.empty

        override def receive: Receive[IO, JointLedger.Requests.Request] = {
            case e: LedgerEvent =>
                // IO.println(s"mock: LedgerEvent: $e") >>
                IO { events.append(e) }
            case s: StartBlock =>
                // IO.println(s"mock: StartBlock: $s") >>
                IO.pure(())
            case c: CompleteBlockRegular =>
                // IO.println(s"mock: CompleteBlockRegular: ${c.referenceBlock}") >>
                IO.pure(())
            case f: CompleteBlockFinal =>
                // IO.println("mock:CompleteBlockFinal") >>
                IO.pure(())
        }
}
