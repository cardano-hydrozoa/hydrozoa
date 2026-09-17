package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.contravariant.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.NoopActor
import hydrozoa.multisig.consensus.ack.HardAckNumber
import hydrozoa.multisig.consensus.liaison.BatchMessages.{Join, Population}
import hydrozoa.multisig.consensus.liaison.{BatchNumber, LiaisonProtocol, PeerLiaisonEventFormat, PeerLiaisonHubToCoil}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.tx.{SettlementTx, genSettlementTxSeqBuilder}
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2StateExport}
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{InMemoryBackendStore, Persistence, PersistenceEventFormat}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt
import test.MinorBlocks

/** The hub's half of the join exchange: what [[PeerLiaisonHubToCoil]] does when a coil peer's link
  * comes up and announces where it stands.
  *
  * ⚠️ **Why this suite exists at all.** `LiaisonProtocol.HubToCoilRequest` is a union type, and
  * Scala does not check a match over one for exhaustiveness. Adding `Join.Connected` to the union
  * therefore compiled clean — including under `-Werror` — while leaving `receiveTotal` with no arm
  * for it, so the first coil to connect would have killed the liaison with a `MatchError`. Nothing
  * else in the suite caught that: the in-process transport sends no handshake. These tests pin the
  * arm down so it cannot be dropped again.
  */
class HubJoinHandoffTest extends AnyFunSuite {

    private val env: MultiNodeConfig =
        MultiNodeConfig
            .generateWithCoil(nCoil = 2, quorum = 1)
            .pureApply(Gen.Parameters.default, Seed(0L))

    private val hubNum = HeadPeerNumber(0)
    private val coilNum = CoilPeerNumber(0)
    private val hubConfig: NodeConfig = env.nodeConfigs(hubNum)

    private given CardanoNetwork.Section = hubConfig

    private val settlement: SettlementTx =
        val seq = genSettlementTxSeqBuilder(env.headConfig)()
            .pureApply(Gen.Parameters.default, Seed(1L))
            .result match {
            case Left(e)  => throw RuntimeException(s"settlement build failed: $e")
            case Right(s) => s
        }
        val unsigned = seq.settlementTx
        unsigned.txLens.replace(env.multisignTx(unsigned.tx))(unsigned)

    private val offeredAck = HardAckNumber(4)

    private val offer = Join.Offer(
      startStack = StackNumber(3),
      cursors = Population.Get(
        batchNum = BatchNumber.zero,
        block = BlockNumber(7),
        stack = StackNumber(4),
        requests = Map.empty,
        softAcks = Map.empty,
        headHardAcks = Map.empty,
        coilHardAcks = Map.empty
      ),
      ownHardAck = offeredAck,
      settlement = settlement,
      sec = None,
      state = L2StateExport(L2CommandNumber.zero, IArray.emptyByteIArray),
      block = MinorBlocks.brief(env.headConfig, 6).unsafeRunSync(),
      deposits = DepositsMap.empty
    )

    /** Records everything the hub sends down the link. */
    private class Recorder(seen: Ref[IO, Vector[LiaisonProtocol.CoilToHubRequest]])
        extends Actor[IO, LiaisonProtocol.CoilToHubRequest] {
        override def receive: Receive[IO, LiaisonProtocol.CoilToHubRequest] =
            PartialFunction.fromFunction(r => seen.update(_ :+ r))
    }

    /** Stand up one hub→coil liaison with `decide` as its start-point decision, hand it
      * `Join.Connected`, and return everything the hub sent down the link.
      */
    private def onConnected(
        decide: Join.Connected => IO[CoilStartPoint],
        connected: Join.Connected = Join.Connected(Some(BlockNumber(1)), Some(StackNumber(1)))
    ): Vector[LiaisonProtocol.CoilToHubRequest] = {
        val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(persistenceTracer)
            .use { backend =>
                Persistence.fromBackend(backend, persistenceTracer).flatMap { persistence =>
                    ActorSystem[IO]("hub-join-test").use { system =>
                        for {
                            seen <- Ref[IO].of(Vector.empty[LiaisonProtocol.CoilToHubRequest])
                            remote <- system.actorOf(new Recorder(seen))
                            slow <- system.actorOf(NoopActor[SlowConsensusActor.Request])
                            sequencer <- system.actorOf(NoopActor[CoilAckSequencer.Request])
                            liaison <- system.actorOf(
                              PeerLiaisonHubToCoil(
                                hubConfig,
                                coilNum,
                                PeerLiaisonHubToCoil.Connections(slow, sequencer, remote),
                                Slf4jTracer.sink.contramap(
                                  PeerLiaisonEventFormat
                                      .humanFormat(PeerId.Head(hubNum), PeerId.Coil(coilNum))
                                ),
                                persistence,
                                decide
                              )
                            )
                            _ <- liaison ! connected
                            // The liaison's own pre-start pull races this; settle before reading.
                            _ <- IO.sleep(300.millis)
                            out <- seen.get
                        } yield out
                    }
                }
            }
            .unsafeRunSync()
    }

    private def offers(
        out: Vector[LiaisonProtocol.CoilToHubRequest]
    ): Vector[Join.Offer] = out.collect { case o: Join.Offer => o }

    test("a connected coil that needs seeding is sent the offer") {
        val out = onConnected(_ => IO.pure(CoilStartPoint.Offer(offer)))
        assert(offers(out).map(_.startStack) == Vector(StackNumber(3)))
        assert(offers(out).map(_.ownHardAck) == Vector(offeredAck))
    }

    test("the coil's own marks reach the decision unchanged") {
        val marks = Join.Connected(Some(BlockNumber(42)), Some(StackNumber(9)))
        val seenMarks = Ref.unsafe[IO, Option[Join.Connected]](None)
        val _ = onConnected(
          c => seenMarks.set(Some(c)) >> IO.pure(CoilStartPoint.CatchUp),
          connected = marks
        )
        assert(seenMarks.get.unsafeRunSync().contains(marks))
    }

    test("a coil within catch-up range is sent no offer") {
        assert(offers(onConnected(_ => IO.pure(CoilStartPoint.CatchUp))).isEmpty)
    }

    test("a hub with no start point sends no offer") {
        val out = onConnected(_ =>
            IO.pure(CoilStartPoint.Unavailable(CoilStartPoint.Reason.HeadAtStackZero))
        )
        assert(offers(out).isEmpty)
    }

    test("the hub pulls the coil from the index it offered, not from zero") {
        // The whole point of the exchange. If the hub kept its own cold cursor here it would ask a
        // freshly-seeded coil for hard-acks below the start point — acks that coil never produced
        // and cannot reconstruct, which is the stall this ticket exists to fix.
        val out = onConnected(_ => IO.pure(CoilStartPoint.Offer(offer)))
        val gets = out.collect {
            case g: hydrozoa.multisig.consensus.liaison.BatchMessages.OwnHardAck.Get => g
        }
        assert(gets.nonEmpty, "the hub must keep pulling after seeding")
        assert(
          gets.last.hardAck == offeredAck,
          s"hub is pulling from ${gets.last.hardAck}, not the offered $offeredAck"
        )
    }
}
