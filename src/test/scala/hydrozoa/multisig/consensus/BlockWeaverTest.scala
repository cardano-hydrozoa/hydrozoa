package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import com.suprnation.actor.test.TestKit
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, LedgerEvent, StartBlock}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.protocol.ConsensusProtocol.NewLedgerEvent
import hydrozoa.multisig.protocol.types.{Block, Peer}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.genVersion
import java.util.concurrent.TimeUnit
import org.scalacheck.{Arbitrary, Gen, Properties, PropertyBuilder, Test}
import scala.collection.mutable
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import test.Generators.Hydrozoa.ArbitraryInstances.given
import test.genTestPeers

object BlockWeaverTest extends Properties("Block weaver test"), TestKit {
    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withMinSuccessfulTests(100)

    val _ = property(
      "Weaver leads when turn comes, feeding events from recovered/residual mempool"
    ) = PropertyBuilder.property { p =>

        val peers = p.pick(genTestPeers(), "test peers")
        val peer = p.pick(Gen.oneOf(peers.toList))
        // See comment in the BlockWeaver.Config
        val turn = p.pick(Gen.choose(1, peers.size), "block lead turn")

        // Either the initialization block or any arbitrary block
        val lastKnownBlock =
            p.pick(
              Gen.frequency(
                5 -> Gen.const(0),
                5 -> Gen.choose(1, Int.MaxValue)
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
          lastKnownBlock = Block.Number(lastKnownBlock),
          peerId = Peer.Number(peer.ordinal),
          numberOfPeers = peers.size,
          blockLeadTurn = turn,
          recoveredMempool = BlockWeaver.Mempool.apply(events),
          jointLedger = jointLedgerMockActor,
        )

        // Weaver
        val _ = p.runIO(system.actorOf(BlockWeaver(config)))

        def aroundNow(other: FiniteDuration): Boolean = {
            val now = p.runIO(IO.monotonic)
            now.minus(1.second) < other && now.plus(1.second) > other
        }

        // If the next block is the peer's turn...
        p.pre((lastKnownBlock + 1) % peers.size == config.blockLeadTurn)

        // then ...
        p.assert(
          p.runIO(handleBoolean(expectMsgPF(jointLedgerMockActor, 5.second) {
              case s: StartBlock
                  if aroundNow(
                    FiniteDuration.apply(s.blockCreationTime.toLong, TimeUnit.MILLISECONDS)
                  ) =>
                  ()
          })),
          "weaver should start the block with sensible creation time"
        )

        p.assert(
          jointLedgerMock.events.toSeq == events,
          "feed all residual/recovered mempool events"
        )

        p.runIO(system.terminate())

        // Done
        true
    }

    // TODO: move to PropertyBuilder
    def handleBoolean(comp: IO[Unit]): IO[Boolean] = (comp >> IO.pure(true)).handleError(_ => false)

    val _ = property("Leader finishes first block immediately after any event") =
        PropertyBuilder.property { p =>

            val peers = p.pick(genTestPeers(), "test peers")
            val peer = peers.head
            val turn = 1
            val lastKnownBlock = 0

            // Joint ledger mock
            val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
            val jointLedgerMock = JointLedgerMock()
            val jointLedgerMockActor =
                p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))

            // Weaver's config
            val config = BlockWeaver.Config(
              lastKnownBlock = Block.Number(lastKnownBlock),
              peerId = Peer.Number(peer.ordinal),
              numberOfPeers = peers.size,
              blockLeadTurn = turn,
              recoveredMempool = BlockWeaver.Mempool.empty,
              jointLedger = jointLedgerMockActor,
            )

            // Weaver
            val weaverActor = p.runIO(system.actorOf(BlockWeaver(config)))

            val _ = p.runIO(system.waitForIdle())

            val anyLedgerEvent =
                p.pick(Arbitrary.arbitrary[LedgerEvent], "any event to finish the first block")

            p.assert(
              p.runIO(
                handleBoolean(for {
                    _ <- weaverActor ! NewLedgerEvent(0, anyLedgerEvent)
                    _ <- system.waitForIdle()
                    _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) {
                        // The first block cannot be final
                        case CompleteBlockRegular(None) => ()
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

            // See comment in the BlockWeaver.Config
            val turn = p.pick(Gen.choose(1, peers.size), "block lead turn")

            // Joint ledger mock
            val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
            val jointLedgerMock = JointLedgerMock()
            val jointLedgerMockActor =
                p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))

            // Weaver's config such that the peer is going to be the leader of the next non-first block
            val roundsCompleted = p.pick(Gen.choose(100, 1000))
            val config = BlockWeaver.Config(
              lastKnownBlock = Block.Number(turn + roundsCompleted * peers.size - 1),
              peerId = Peer.Number(peer.ordinal),
              numberOfPeers = peers.size,
              blockLeadTurn = turn,
              recoveredMempool = BlockWeaver.Mempool.empty,
              jointLedger = jointLedgerMockActor,
            )

            // Weaver
            val weaverActor = p.runIO(system.actorOf(BlockWeaver(config)))

            // Any random events
            val events: Seq[LedgerEvent] =
                p.pick(Gen.nonEmptyListOf(Arbitrary.arbitrary[LedgerEvent]))

            // Should be passed through with no delay
            p.assert(
              events
                  .map { e =>
                      p.runIO(
                        handleBoolean(for {
                            _ <- weaverActor ! NewLedgerEvent(0, e)
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
      "Knowing all events, new block is fed immediately, preserving residual mempool"
    ) = PropertyBuilder.property { p =>
        // Since we are going to check two blocks in a row not to peek into the weaver's state,
        // we need at least three peers so the SUT weaver won't become a leader accidentally.
        val peers = p.pick(genTestPeers(3), "test peers")
        val peer = p.pick(Gen.oneOf(peers.toList), "own peer")
        // See comment in the BlockWeaver.Config
        val turn = p.pick(Gen.choose(2, peers.size), "block lead turn")

        // Joint ledger mock
        val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
        val jointLedgerMock = JointLedgerMock()
        val jointLedgerMockActor =
            p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))

        // Weaver's config such that the peer is goin to be the leader of the next non-first block
        val roundsCompleted = p.pick(Gen.choose(100, 1000), "rounds completed")

        val lastKnownBlock = Block.Number(turn + roundsCompleted * peers.size)
        val config = BlockWeaver.Config(
          // This is next block after a peer's turn, so we have at least 2 blocks
          // following that the SUT weaver is guaranteed to handle as a follower.
          lastKnownBlock = lastKnownBlock,
          peerId = Peer.Number(peer.ordinal),
          numberOfPeers = peers.size,
          blockLeadTurn = turn,
          recoveredMempool = BlockWeaver.Mempool.empty,
          jointLedger = jointLedgerMockActor,
        )

        // Weaver
        val weaverActor = p.runIO(system.actorOf(BlockWeaver(config)))

        // Random ledger events
        val eventsTotal = p.pick(Gen.choose(5, 100), "total number of events")
        val knownEvents: Seq[LedgerEvent] = p.pick(
          Gen.listOfN(eventsTotal, Arbitrary.arbitrary[LedgerEvent]).map(_.distinctBy(_.eventId)),
          "events delivered and known"
        )

        // Block events
        val firstBlockEventsNumber =
            p.pick(Gen.choose(1, knownEvents.length), "number of block events")
        val firstBlockEvents =
            p.pick(Gen.pick(firstBlockEventsNumber, knownEvents), "first block events")
        val secondBlockEvents = p.pick(
          Gen.const(knownEvents.filterNot(firstBlockEvents.contains)),
          "second block events"
        )

        val version = p.pick(
          genVersion.map((maj, min) => Block.Version.Full.apply(maj.toInt, min.toInt)),
          "first block version"
        )

        p.assert(
          p.runIO(
            handleBoolean(
              for {
                  // Pass all events
                  _ <- IO.traverse_(knownEvents)(weaverActor ! NewLedgerEvent(0, _))

                  // First block
                  now <- IO.monotonic
                  firstBlock: Block = Block.Minor(
                    Block.Header.Minor(
                      blockNum = lastKnownBlock.increment,
                      blockVersion = version,
                      timeCreation = now,
                      commitment = KzgCommitment.empty
                    ),
                    Block.Body.Minor(
                      events = firstBlockEvents.map(e => (e.eventId, true)).toList,
                      depositsRefunded = List.empty
                    )
                  )

                  // Second block
                  newTime <- IO.monotonic
                  secondBlock: Block = firstBlock.nextBlock(
                    Block.Body.Minor(
                      events = secondBlockEvents.map(e => (e.eventId, true)).toList,
                      depositsRefunded = List.empty
                    ),
                    newTime = newTime,
                    newCommitment = KzgCommitment.empty
                  )

                  _ <- weaverActor ! firstBlock
                  _ <- weaverActor ! secondBlock

                  // Check we get StartBlock
                  _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) {
                      case StartBlock(blockCreationTime, _)
                          if blockCreationTime == firstBlock.header.timeCreation.toMillis =>
                          ()
                  }

                  // Check we get CompleteBlock
                  _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) {
                      case CompleteBlockRegular(block) if Some(firstBlock) == block => ()
                  }

                  // Check we get StartBlock
                  _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) {
                      case StartBlock(blockCreationTime, _)
                          if blockCreationTime == secondBlock.header.timeCreation.toMillis =>
                          ()
                  }

                  // Check we get CompleteBlock
                  _ <- expectMsgPF(jointLedgerMockActor, 5.seconds) {
                      case CompleteBlockRegular(block) if Some(secondBlock) == block => ()
                  }
                  // Check all block events are present
                  _ <- IO.raiseUnless(
                    jointLedgerMock.events == firstBlockEvents ++ secondBlockEvents
                  )(
                    RuntimeException("second block events are different from what expected")
                  )
              } yield ()
            )
          )
        )

        true
    }

    val _ = property(
      "Waits till all events are know and finishes block preserving residual mempool "
    ) = PropertyBuilder.property { p =>
        true
    }

    class JointLedgerMock extends Actor[IO, JointLedger.Requests.Request]:

        val events: mutable.Buffer[LedgerEvent] = mutable.Buffer.empty

        override def receive: Receive[IO, JointLedger.Requests.Request] = {
            case e: LedgerEvent =>
                // IO.println(s"LedgerEvent: $e") >>
                IO { events.append(e) }
            case s: StartBlock           => IO.println(s"StartBlock: $s")
            case c: CompleteBlockRegular => IO.println(s"CompleteBlockRegular: ${c.referenceBlock}")
            case f: CompleteBlockFinal   => IO.println("CompleteBlockFinal")
        }
}
