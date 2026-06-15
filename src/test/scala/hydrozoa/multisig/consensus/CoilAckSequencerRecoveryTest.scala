package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, HardAckWithId, HubHardAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{ArrivalStamp, InMemoryBackendStore, FamilyKey, FamilyValue, Persistence}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite

/** Recovery tests for [[CoilAckSequencer.recover]] (§6 CoilAckSequencer): a hub restores its
  * `HubHardAck` relay counter and per-coil idempotency index from its own-hub `HubHardAck` family,
  * so it never re-stamps an already-sequenced coil ack a liaison re-delivers.
  */
class CoilAckSequencerRecoveryTest extends AnyFunSuite:

    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))
    private given CardanoNetwork.Section = config
    private val stamp: ArrivalStamp = ArrivalStamp(generation = 0, monotonicNanos = 1L)

    /** This node's head peer number — it acts as the hub in these tests. */
    private val hub: HeadPeerNumber = config.ownPeerId match {
        case PeerId.Head(n) => n
        case PeerId.Coil(_) => fail("fixture config must be a head node")
    }

    test("recover on an empty store starts the counter at zero with an empty index") {
        val recovered = run(_ => IO.unit)
        assert(
          recovered == CoilAckSequencer.Recovered(HubHardAckNumber.zero, Map.empty),
          s"empty store → zero counter + empty index; got $recovered"
        )
    }

    test("recover restores nextSeq = max + 1 and the per-coil idempotency index") {
        // Three sequenced coil acks on this hub's HubHardAck family, contiguous seq 0..2.
        val recovered = run { p =>
            for {
                _ <- putHubAck(p, seq = 0, coil = 0, hardAckNum = 5)
                _ <- putHubAck(p, seq = 1, coil = 1, hardAckNum = 2)
                _ <- putHubAck(p, seq = 2, coil = 0, hardAckNum = 6)
            } yield ()
        }
        val expected = CoilAckSequencer.Recovered(
          nextSeq = HubHardAckNumber(3),
          index = Map(
            (CoilPeerNumber(0), HardAckNumber(5)) -> HubHardAckNumber(0),
            (CoilPeerNumber(1), HardAckNumber(2)) -> HubHardAckNumber(1),
            (CoilPeerNumber(0), HardAckNumber(6)) -> HubHardAckNumber(2)
          )
        )
        assert(
          recovered == expected,
          s"nextSeq = max + 1 and index keyed by (coil, hardAckNum); got $recovered"
        )
    }

    /** Open a fresh in-memory store, seed it, and run `recover` against this hub. */
    private def run(seed: Persistence[IO] => IO[Unit]): CoilAckSequencer.Recovered =
        InMemoryBackendStore.open
            .use(backend =>
                for {
                    p <- Persistence.fromBackend(backend)
                    _ <- seed(p)
                    recovered <- CoilAckSequencer.recover(p, hub)
                } yield recovered
            )
            .unsafeRunSync()

    /** Persist one sequenced coil ack to this hub's own `HubHardAck` family. */
    private def putHubAck(p: Persistence[IO], seq: Int, coil: Int, hardAckNum: Int): IO[Unit] =
        val ack = coilHardAck(coil, hardAckNum)
        val hubAck = HardAckWithId(hubPeer = hub, seqNum = HubHardAckNumber(seq), ack = ack)
        p.put(FamilyKey.HubHardAck(hub, HubHardAckNumber(seq)))(FamilyValue(stamp, hubAck))

    /** A coil-authored hard-ack fixture (signature contents are irrelevant to sequencing). */
    private def coilHardAck(coil: Int, hardAckNum: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Coil(CoilPeerNumber(coil)), HardAckNumber(hardAckNum)),
          stackNum = StackNumber(0),
          payload = HardAck.Round2Payload.Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
        )
