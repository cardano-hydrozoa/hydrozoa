package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, HardAckWithId, HubHardAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{ArrivalStamp, InMemoryBackendStore, JournalKey, JournalValue, Persistence, PersistenceEventFormat, StoreKey}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite

/** Recovery tests for [[CoilAckSequencer.recover]] (§6 CoilAckSequencer): a hub restores its
  * `HubHardAck` relay counter from its own-hub `HubHardAck` journal and its per-coil
  * stamped-high-water marks from the `CoilStampMark` blob, so it knows which durably-received coil
  * acks a crash left unstamped (the gap it then stamps from the store).
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

    test("recover on an empty store starts the counter at zero with no marks") {
        val recovered = run(_ => IO.unit)
        assert(
          recovered == CoilAckSequencer.Recovered(HubHardAckNumber.zero, Map.empty),
          s"empty store → zero counter + no marks; got $recovered"
        )
    }

    test(
      "recover restores nextSeq = max(HubHardAck) + 1 and the per-coil marks from CoilStampMark"
    ) {
        val marks =
            Map(CoilPeerNumber(0) -> HardAckNumber(6), CoilPeerNumber(1) -> HardAckNumber(2))
        val recovered = run { p =>
            for {
                // Three sequenced coil acks → nextSeq = 3.
                _ <- putHubAck(p, seq = 0, coil = 0, hardAckNum = 5)
                _ <- putHubAck(p, seq = 1, coil = 1, hardAckNum = 2)
                _ <- putHubAck(p, seq = 2, coil = 0, hardAckNum = 6)
                // Marks blob (written atomically with the last stamp of each coil).
                _ <- p.put(StoreKey.CoilStampMark)(marks)
            } yield ()
        }
        assert(
          recovered == CoilAckSequencer.Recovered(HubHardAckNumber(3), marks),
          s"nextSeq = max + 1 and marks from CoilStampMark; got $recovered"
        )
    }

    test("recover with HubHardAck entries but no CoilStampMark blob yields empty marks") {
        val recovered = run(putHubAck(_, seq = 0, coil = 0, hardAckNum = 5))
        assert(
          recovered == CoilAckSequencer.Recovered(HubHardAckNumber(1), Map.empty),
          s"nextSeq advances; absent blob → empty marks; got $recovered"
        )
    }

    /** Open a fresh in-memory store, seed it, and run `recover` against this hub. */
    private def run(seed: Persistence[IO] => IO[Unit]): CoilAckSequencer.Recovered =
        val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(persistenceTracer)
            .use(backend =>
                for {
                    p <- Persistence.fromBackend(backend, persistenceTracer)
                    _ <- seed(p)
                    recovered <- CoilAckSequencer.recover(p, hub)
                } yield recovered
            )
            .unsafeRunSync()

    /** Persist one sequenced coil ack to this hub's own `HubHardAck` journal. */
    private def putHubAck(p: Persistence[IO], seq: Int, coil: Int, hardAckNum: Int): IO[Unit] =
        val ack = coilHardAck(coil, hardAckNum)
        val hubAck = HardAckWithId(hubPeer = hub, seqNum = HubHardAckNumber(seq), ack = ack)
        p.put(JournalKey.HubHardAck(hub, HubHardAckNumber(seq)))(JournalValue(stamp, hubAck))

    /** A coil-authored hard-ack fixture (signature contents are irrelevant to sequencing). */
    private def coilHardAck(coil: Int, hardAckNum: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Coil(CoilPeerNumber(coil)), HardAckNumber(hardAckNum)),
          stackNum = StackNumber(0),
          payload = HardAck.Round2Payload.Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
        )
