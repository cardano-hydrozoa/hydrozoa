package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import com.suprnation.actor.test.TestKit
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, LedgerEvent, StartBlock}
import hydrozoa.multisig.protocol.ConsensusProtocol.NewLedgerEvent
import hydrozoa.multisig.protocol.types.{Block, Peer}
import java.util.concurrent.TimeUnit
import org.scalacheck.{Arbitrary, Gen, Prop, Properties, PropertyBuilder, Test}
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

        val peers = p.pick(genTestPeers, "test peers")
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

        // Random ledger events for the recovery pool for non-initial block
        val events: Seq[LedgerEvent] = p.pick(
          if lastKnownBlock > 0 then Gen.nonEmptyListOf(Arbitrary.arbitrary[LedgerEvent])
          else Gen.const(Seq.empty)
        )

        // Discard samples with duplicates. So far we decided not to have a separate test
        // for duplicates in the residual/recovered mempool, since it will always make
        // the mempool panic.
        p.pre {
            val ids = events.map(_.eventId)
            ids.distinct == events
        }

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

    // val _ = property("Weaver finishes first block immediately after first event") = Prop.falsified

    val _ = property("Being leader for non-first block weaver passes-through all ledger events") =
        PropertyBuilder.property { p =>

            val peers = p.pick(genTestPeers, "test peers")
            val peer = p.pick(Gen.oneOf(peers.toList))

            // See comment in the BlockWeaver.Config
            val turn = p.pick(Gen.choose(1, peers.size), "block lead turn")

            // Joint ledger mock
            val system = p.runIO(ActorSystem[IO]("Weaver SUT").allocated)._1
            val jointLedgerMock = JointLedgerMock()
            val jointLedgerMockActor =
                p.runIO(system.actorOf(jointLedgerMock.trackWithCache("joint-ledger-mock")))

            // Weaver's config such that the peer is goin to be the leader of the next non-first block
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

            val _ = p.runIO(system.waitForIdle())

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

    // val _ = property("Knowing all events, new block is fed immediately") = Prop.falsified
    //
    // val _ = property("W/aits till all events are known and finishes block") = Prop.falsified

    class JointLedgerMock extends Actor[IO, JointLedger.Requests.Request]:

        val events: mutable.Buffer[LedgerEvent] = mutable.Buffer.empty

        override def receive: Receive[IO, JointLedger.Requests.Request] = {
            case e: LedgerEvent => IO.println(s"LedgerEvent: $e") >> IO { events.append(e) }
            case s: StartBlock  => IO.println(s"StartBlock: $s")
            case c: CompleteBlockRegular => IO.println("CompleteBlockRegular")
            case f: CompleteBlockFinal   => IO.println("CompleteBlockFinal")
        }
}
