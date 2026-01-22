package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import com.suprnation.actor.test.TestKit
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.protocol.types.LedgerEventId.ValidityFlag.Valid
import hydrozoa.multisig.protocol.types.{Block, LedgerEvent, Peer}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.genVersion
import java.time.Instant
import org.scalacheck.{Arbitrary, Gen, Properties, PropertyBuilder, Test}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.util.Random
import test.Generators.Hydrozoa.ArbitraryInstances.given
import test.{genTestPeers, testTxBuilderEnvironment}

object BlockWeaverTest extends Properties("Block weaver test"), TestKit {
    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p.withMinSuccessfulTests(100)
            .withWorkers(1)
    }

    val _ = property(
      "Weaver leads when turn comes, feeding events from recovered/residual mempool"
    ) = PropertyBuilder.property { p =>

        val peers = p.pick(genTestPeers(), "test peers")
        val peer = p.pick(Gen.oneOf(peers.toList))
        val peerId = Peer.Id(peer.ordinal, peers.size)

        // Either the initialization block or any arbitrary block
        val lastKnownBlock =
            Block.Number(
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
          slotConfig = testTxBuilderEnvironment.slotConfig
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
            val peer = peers.head
            val peerId = Peer.Id(peer.ordinal, peers.size)
            val lastKnownBlock = 0

            // Joint ledger mock
            val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
            val jointLedgerMock = JointLedgerMock()
            val jointLedgerMockActor =
                p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))

            // Weaver's config
            val config = BlockWeaver.Config(
              lastKnownBlock = Block.Number(lastKnownBlock),
              peerId = peerId,
              recoveredMempool = BlockWeaver.Mempool.empty,
              slotConfig = testTxBuilderEnvironment.slotConfig
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
                        // The first block cannot be final
                        case CompleteBlockRegular(None, _) => ()
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
            val peerId = Peer.Id(peer.ordinal, peers.size)

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
              peerId = peerId,
              recoveredMempool = BlockWeaver.Mempool.empty,
              slotConfig = testTxBuilderEnvironment.slotConfig
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
        val peers = p.pick(genTestPeers(4), "test peers")
        val peer = p.pick(Gen.oneOf(peers.toList), "own peer")
        val peerId = Peer.Id(peer.ordinal, peers.size)
        // See comment in the BlockWeaver.Config
        val turn = p.pick(Gen.choose(2, peers.size), "block lead turn")

        // Joint ledger mock
        val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
        val jointLedgerMock = JointLedgerMock()
        val jointLedgerMockActor =
            p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))

        val roundsCompleted = p.pick(Gen.choose(100, 1000), "rounds completed")

        val lastKnownBlock = Block.Number(turn + roundsCompleted * peers.size)
        val config = BlockWeaver.Config(
          // This is next block after a peer's turn, so we have at least 3 blocks
          // following that the SUT weaver is guaranteed to handle as a follower.
          lastKnownBlock = lastKnownBlock,
          peerId = peerId,
          recoveredMempool = BlockWeaver.Mempool.empty,
          slotConfig = testTxBuilderEnvironment.slotConfig
        )

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
          genVersion.map((maj, min) => Block.Version.Full.apply(maj.toInt, min.toInt)),
          "first block version"
        )

        p.assert(
          p.runIO(
            handleBoolean(
              for {
                  // Pass all immediate events
                  _ <- IO.traverse_(immediateEvents)(weaverActor ! _)

                  // First block
                  now <- realTimeQuantizedInstant(testTxBuilderEnvironment.slotConfig)
                  firstBlock: Block.Next = Block.Minor(
                    Block.Header.Minor(
                      blockNum = lastKnownBlock.increment,
                      blockVersion = version,
                      timeCreation = now,
                      commitment = KzgCommitment.empty
                    ),
                    Block.Body.Minor(
                      events = firstBlockEvents.map(e => (e.eventId, Valid)).toList,
                      depositsRefunded = List.empty
                    )
                  )

                  // Second block
                  newTime <- realTimeQuantizedInstant(testTxBuilderEnvironment.slotConfig)
                  secondBlock: Block.Next = firstBlock.nextBlock(
                    Block.Body.Minor(
                      events = secondBlockEvents.map(e => (e.eventId, Valid)).toList,
                      depositsRefunded = List.empty
                    ),
                    newTime = newTime,
                    newCommitment = KzgCommitment.empty
                  )

                  _ <- (for {
                      _ <- IO.sleep(50.millis)
                      _ <- weaverActor ! firstBlock
                      _ <- IO.whenA(eventsDelayed.nonEmpty)(
                        IO.traverse_(eventsDelayed)(weaverActor ! _)
                      )
                      _ <- weaverActor ! secondBlock
                  } yield ()).start.void

                  _ <- expectMsgs(jointLedgerMockActor, 10.seconds)(
                    List(
                      StartBlock(firstBlock.header.blockNum, firstBlock.header.timeCreation)
                    )
                        ++ firstBlockEvents
                        ++ List(
                          CompleteBlockRegular(Some(firstBlock), Set.empty)
                        )
                        ++ List(
                          StartBlock(secondBlock.header.blockNum, secondBlock.header.timeCreation)
                        )
                        ++ secondBlockEvents
                        ++ List(CompleteBlockRegular(Some(secondBlock), Set.empty))*
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
