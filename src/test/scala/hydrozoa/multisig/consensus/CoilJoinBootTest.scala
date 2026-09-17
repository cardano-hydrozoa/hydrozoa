package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.Join
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonCoilToHub}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.CoilTransport
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.eutxol2.store.InMemoryL2Store
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{InMemoryBackendStore, JournalKey, JournalValue, Persistence, PersistenceEvent, StoreKey}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** When a coil peer boots and when it refuses to — [[CoilJoin.settleStartPoint]].
  *
  * ⚠️ **The cold case is the whole ticket.** A coil with an empty store that boots without an
  * answer re-derives stack 0 from config, and a head long past stack 0 can never reconcile with it:
  * the node reports healthy and is permanently useless. Blocking is the deliberate alternative, so
  * the test that matters here is the one asserting a cold coil does *not* return.
  */
class CoilJoinBootTest extends AnyFunSuite {

    private val env: MultiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))

    private val nodeConfig: NodeConfig = env.nodeConfigs(HeadPeerNumber.zero)

    private given CoilJoin.Config = nodeConfig

    private val silentTracer: ContraTracer[IO, CoilJoinEvent] =
        ContraTracer(_ => IO.unit)

    private val quietPersistence: ContraTracer[IO, PersistenceEvent] =
        ContraTracer(_ => IO.unit)

    /** A transport whose only job is to hand back one prepared answer, or never answer at all. */
    private class StubTransport(answer: IO[Join.Answer]) extends CoilTransport {
        override def register(localLiaison: PeerLiaisonCoilToHub.Handle): IO[Unit] = IO.unit
        override def send(request: LiaisonProtocol.HubToCoilRequest): IO[Unit] = IO.unit
        override def joinAnswer: IO[Join.Answer] = answer
    }

    private val ownPeerId: PeerId = nodeConfig.ownPeerId

    /** Give the store one own hard-ack, which is what makes a peer read as warm. */
    private def warmUp(p: Persistence[IO]): IO[Unit] =
        p.arrivalStamp.flatMap(stamp =>
            p.put(JournalKey.HardAck(ownPeerId, HardAckNumber(0)))(
              JournalValue(
                stamp,
                HardAck(
                  ackId = HardAckId(ownPeerId, HardAckNumber(0)),
                  stackNum = StackNumber(3),
                  payload = HardAck.Round2Payload.Regular(
                    TxSignature(IArray.from(Array.fill[Byte](64)(0)))
                  )
                )
              )
            )
        )

    /** A store that was seeded earlier: it has a start point and still no own hard-ack, because a
      * seeded coil authors none until it acks its first stack.
      */
    private def seed(p: Persistence[IO]): IO[Unit] =
        p.put(StoreKey.StartPoint)(
          hydrozoa.multisig.persistence.AdoptedStartPoint(
            startStack = StackNumber(7),
            lastBlockNum = BlockNumber(12),
            commandNumber = hydrozoa.multisig.ledger.l2.L2CommandNumber.zero,
            ownHardAckStart = HardAckNumber(5)
          )
        )

    private def settle(
        answer: IO[Join.Answer],
        warm: Boolean = false,
        seeded: Boolean = false
    ): IO[Unit] =
        InMemoryBackendStore
            .open(quietPersistence)
            .use(backend =>
                for {
                    p <- Persistence.fromBackend(backend, quietPersistence)
                    _ <- IO.whenA(warm)(warmUp(p))
                    _ <- IO.whenA(seeded)(seed(p))
                    store <- InMemoryL2Store.create
                    ledger <- EutxoL2Ledger(nodeConfig, store)
                    _ <- CoilJoin
                        .settleStartPoint(new StubTransport(answer), p, ledger, silentTracer)
                } yield ()
            )

    test("a cold coil whose hub never answers does not boot") {
        // Left to itself it would bootstrap stack 0 and be unrecoverable. `TestControl` advances
        // the clock past every timeout in the code, so a non-terminating result here is the
        // assertion: nothing in the cold path gives up.
        val outcome = TestControl
            .execute(settle(IO.never))
            .flatMap(control => control.tick >> control.advanceAndTick(1.hour) >> control.results)
            .unsafeRunSync()
        assert(outcome.isEmpty, s"a cold coil booted without an answer: $outcome")
    }

    test("a warm coil whose hub never answers boots anyway") {
        // It has history to walk forward from, so blocking a working node on an unreachable hub
        // would be the wrong trade.
        val outcome = TestControl
            .execute(settle(IO.never, warm = true))
            .flatMap(control =>
                control.tick >> control.advanceAndTick(CoilJoin.warmJoinWait + 1.second) >>
                    control.results
            )
            .unsafeRunSync()
        assert(outcome.exists(_.isSuccess), s"a warm coil failed to boot: $outcome")
    }

    test("a coil that was already seeded does not wait again on reboot") {
        // It has no own hard-ack — a seeded coil authors none until it acks its first stack — so
        // the own-ack journal alone still reads as cold here. The start point is what says this
        // peer已 has somewhere to boot from; miss it and every restart of a seeded coil hangs.
        val outcome = TestControl
            .execute(settle(IO.never, seeded = true))
            .flatMap(control =>
                control.tick >> control.advanceAndTick(CoilJoin.warmJoinWait + 1.second) >>
                    control.results
            )
            .unsafeRunSync()
        assert(outcome.exists(_.isSuccess), s"a seeded coil blocked on reboot: $outcome")
    }

    test("a cold coil boots as soon as its hub says there is nothing to seed from") {
        // The ordinary bring-up: every coil is cold and the head is at stack 0. None of them may
        // block, or a fresh head never starts.
        val outcome = TestControl
            .execute(settle(IO.pure(Join.NoOffer("head at stack 0"))))
            .flatMap(control => control.tick >> control.results)
            .unsafeRunSync()
        assert(outcome.exists(_.isSuccess), s"a cold coil blocked on a NoOffer: $outcome")
    }
}
