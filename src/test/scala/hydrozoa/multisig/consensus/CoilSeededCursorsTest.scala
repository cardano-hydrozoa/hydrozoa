package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import cats.syntax.contravariant.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.NoopActor
import hydrozoa.multisig.consensus.ack.{HardAckNumber, HubHardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.Population
import hydrozoa.multisig.consensus.liaison.{BatchNumber, LiaisonProtocol, PeerLiaisonCoilToHub, PeerLiaisonEventFormat}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.l2.L2CommandNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{AdoptedStartPoint, InMemoryBackendStore, JournalKey, JournalValue, Persistence, PersistenceEventFormat, StoreKey}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt
import test.MinorBlocks

/** What a seeded coil peer actually asks its hub for.
  *
  * ⚠️ **This is the assertion the adoption work was missing.** Persisting the offer's cursor set
  * proves nothing on its own: a lane cursor is normally `max(journal) + 1`, a seeded coil's
  * journals are empty, and so every lane restored cold and the coil opened its pull chain at the
  * beginning of a history its hub has very likely pruned. The offer's cursors were computed,
  * shipped, verified — and then ignored. Nothing caught it, because nothing looked at the first
  * `Population.Get` off a seeded store.
  */
class CoilSeededCursorsTest extends AnyFunSuite {

    private val env: MultiNodeConfig =
        MultiNodeConfig
            .generateWithCoil(nCoil = 2, quorum = 1)
            .pureApply(Gen.Parameters.default, Seed(0L))

    private val coilConfig: NodeConfig = env.mkCoilNodeConfigs(env.coilWallets).head

    private given CardanoNetwork.Section = coilConfig

    private val h0 = HeadPeerNumber(0)

    /** Every head peer, because the hub sends every head peer. A cursor set that names only some of
      * them leaves the rest restoring cold, which is the defect in miniature.
      */
    private val headPeers: List[HeadPeerNumber] = coilConfig.headPeerNums.toList

    private val adoptedCursors = Population.Get(
      batchNum = BatchNumber.zero,
      block = BlockNumber(13),
      stack = StackNumber(8),
      requests = headPeers.map(h => h -> RequestNumber(41 + (h: Int))).toMap,
      softAcks = headPeers.map(h => h -> SoftAckNumber(13)).toMap,
      // The indices that cannot be derived from the start point at all: a stack yields one ack per
      // peer when it is sole and two when it is 2-phase.
      headHardAcks = headPeers.map(h => h -> HardAckNumber(9 + (h: Int))).toMap,
      coilHardAcks = Map(h0 -> HubHardAckNumber(6))
    )

    private val startPoint = AdoptedStartPoint(
      startStack = StackNumber(7),
      lastBlockNum = BlockNumber(12),
      commandNumber = L2CommandNumber.zero,
      ownHardAckStart = HardAckNumber(5),
      cursors = adoptedCursors
    )

    private class Recorder(seen: Ref[IO, Vector[LiaisonProtocol.HubToCoilRequest]])
        extends Actor[IO, LiaisonProtocol.HubToCoilRequest] {
        override def receive: Receive[IO, LiaisonProtocol.HubToCoilRequest] =
            PartialFunction.fromFunction(r => seen.update(_ :+ r))
    }

    /** Poll until the liaison has sent its opening `Population.Get`, or give up loudly. */
    private def awaitFirstPull(
        seen: Ref[IO, Vector[LiaisonProtocol.HubToCoilRequest]]
    ): IO[Population.Get] =
        def go: IO[Population.Get] =
            seen.get.flatMap(_.collectFirst { case g: Population.Get => g } match {
                case Some(g) => IO.pure(g)
                case None    => IO.sleep(20.millis) >> go
            })
        go.timeoutTo(
          20.seconds,
          IO.raiseError(new AssertionError("liaison never opened its pull chain"))
        )

    /** Boot one coil liaison over a store and return its opening pull. `blocksTo` writes a block
      * spine, standing in for a coil that has already pulled some way past its start point.
      */
    private def firstPull(seeded: Boolean, blocksTo: Option[Int] = None): Population.Get = {
        val tracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(tracer)
            .use(backend =>
                Persistence.fromBackend(backend, tracer).flatMap { p =>
                    ActorSystem[IO]("coil-cursor-test").use { system =>
                        for {
                            _ <- IO.whenA(seeded)(p.put(StoreKey.StartPoint)(startPoint))
                            _ <- blocksTo.traverse_(n =>
                                (1 to n).toList.traverse_(i =>
                                    for {
                                        stamp <- p.arrivalStamp
                                        b <- MinorBlocks.brief(coilConfig.headConfig, i)
                                        _ <- p.put(JournalKey.Block(BlockNumber(i)))(
                                          JournalValue(stamp, b)
                                        )
                                    } yield ()
                                )
                            )
                            seen <- Ref[IO].of(Vector.empty[LiaisonProtocol.HubToCoilRequest])
                            remote <- system.actorOf(new Recorder(seen))
                            blockWeaver <- system.actorOf(NoopActor[Any])
                            consensus <- system.actorOf(NoopActor[Any])
                            stackComposer <- system.actorOf(NoopActor[Any])
                            slow <- system.actorOf(NoopActor[Any])
                            _ <- system.actorOf(
                              PeerLiaisonCoilToHub(
                                coilConfig,
                                PeerLiaisonCoilToHub.Connections(
                                  blockWeaver.asInstanceOf,
                                  consensus.asInstanceOf,
                                  stackComposer.asInstanceOf,
                                  slow.asInstanceOf,
                                  remote
                                ),
                                Slf4jTracer.sink.contramap(
                                  PeerLiaisonEventFormat.humanFormat(
                                    PeerId.Coil(CoilPeerNumber(0)),
                                    PeerId.Head(h0)
                                  )
                                ),
                                p
                              )
                            )
                            // Wait for the opening pull rather than guessing at a duration: a
                            // fixed sleep passes alone and fails under a loaded suite, which is a
                            // test that reports load as a bug.
                            out <- awaitFirstPull(seen)
                        } yield out
                    }
                }
            )
            .unsafeRunSync()
    }

    test("a seeded coil opens its pull chain at the cursors it was given") {
        val pull = firstPull(seeded = true)
        assert(pull.block == adoptedCursors.block, "block lane")
        assert(pull.stack == adoptedCursors.stack, "stack lane")
        assert(pull.requests == adoptedCursors.requests, "request lanes")
        assert(pull.softAcks == adoptedCursors.softAcks, "soft-ack lanes")
        assert(pull.headHardAcks == adoptedCursors.headHardAcks, "head hard-ack lanes")
        assert(pull.coilHardAcks == adoptedCursors.coilHardAcks, "coil hard-ack lanes")
    }

    test("a seeded coil that has already pulled past its start point keeps its own progress") {
        // The floor property. Were the start point an override rather than a floor, every
        // reconnect would drag the coil back to where it was first seeded and re-pull everything
        // it has taken since — silently, because the cursors would still look plausible.
        val pull = firstPull(seeded = true, blocksTo = Some(20))
        assert(
          pull.block == BlockNumber(21),
          s"journal reaches 20, so the next block is 21, not ${pull.block}"
        )
    }

    test("an unseeded coil still opens cold — the start point is a floor, not a rewrite") {
        val pull = firstPull(seeded = false)
        assert(pull.block == BlockNumber(1))
        assert(pull.stack == StackNumber(1))
        assert(pull.headHardAcks.values.forall(_ == HardAckNumber.zero))
    }
}
