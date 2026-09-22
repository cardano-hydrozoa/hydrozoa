package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.Population
import hydrozoa.multisig.consensus.liaison.BatchNumber
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{InMemoryBackendStore, JournalKey, JournalValue, Persistence, PersistenceEvent, StoreKey}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite

/** How long a coil liaison may stay in join mode — [[CoilJoin.marksAndWait]].
  *
  * ⚠️ **The cold case is the whole ticket.** A coil with an empty store that leaves join mode
  * without an answer re-derives stack 0 from config, and a head long past stack 0 can never
  * reconcile with it: the node reports healthy and is permanently useless. Waiting is the
  * deliberate alternative, so the test that matters here is the one asserting a cold store yields
  * [[CoilJoin.JoinWait.Forever]].
  */
class CoilJoinBootTest extends AnyFunSuite {

    private val env: MultiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))

    private val nodeConfig: NodeConfig = env.nodeConfigs(HeadPeerNumber.zero)

    private given CardanoNetwork.Section = nodeConfig

    private val quietPersistence: ContraTracer[IO, PersistenceEvent] =
        ContraTracer(_ => IO.unit)

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
            ownHardAckStart = HardAckNumber(5),
            cursors = Population.Get(
              batchNum = BatchNumber.zero,
              block = BlockNumber(13),
              blockCeiling = BlockNumber(13),
              stack = StackNumber(8),
              stackCeiling = StackNumber(8),
              requests = Map.empty,
              requestCeilings = Map.empty,
              softAcks = Map.empty,
              headHardAcks = Map.empty,
              coilHardAcks = Map.empty,
              coilHardAckCeiling = StackNumber(99)
            )
          )
        )

    private def waitFor(
        warm: Boolean = false,
        seeded: Boolean = false
    ): CoilJoin.JoinWait =
        InMemoryBackendStore
            .open(quietPersistence)
            .use(backend =>
                for {
                    p <- Persistence.fromBackend(backend, quietPersistence)
                    _ <- IO.whenA(warm)(warmUp(p))
                    _ <- IO.whenA(seeded)(seed(p))
                    r <- CoilJoin.marksAndWait(p)(using nodeConfig)
                } yield r._2
            )
            .unsafeRunSync()

    test("a cold coil waits for its hub indefinitely") {
        // Left to itself it would bootstrap stack 0 and be unrecoverable, so there is deliberately
        // no deadline on this path: the liaison stays in join mode until its hub answers.
        assert(waitFor() == CoilJoin.JoinWait.Forever)
    }

    test("a warm coil does not wait indefinitely") {
        // It has history to walk forward from, so blocking a working node on an unreachable hub
        // would be the wrong trade.
        assert(waitFor(warm = true) == CoilJoin.JoinWait.Until(CoilJoin.warmJoinWait))
    }

    test("a coil that was already seeded does not wait again on reboot") {
        // It has no own hard-ack — a seeded coil authors none until it acks its first stack — so
        // the own-ack journal alone still reads as cold here. The start point is what says this
        // peer has somewhere to boot from; miss it and every restart of a seeded coil hangs.
        assert(waitFor(seeded = true) == CoilJoin.JoinWait.Until(CoilJoin.warmJoinWait))
    }
}
