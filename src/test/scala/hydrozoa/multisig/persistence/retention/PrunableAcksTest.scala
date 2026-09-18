package hydrozoa.multisig.persistence.retention

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, SoftAck, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{AckRetention, ArchiveWatermarks, ArrivalStamp, Cf, InMemoryBackendStore, JournalKey, JournalValue, Persistence, PersistenceEventFormat}
import java.time.Instant
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite

/** [[PrunableAcks]]: which ack keys a confirmation has made redundant and retention allows
  * deleting.
  *
  * These are the reads behind every deletion the node performs, so the cases that matter are the
  * ones where returning too much is destructive: an entry above the bound, an entry the archive has
  * not taken, or the last row of a family whose highest key is a recovery marker.
  */
class PrunableAcksTest extends AnyFunSuite:

    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))
    private given CardanoNetwork.Section = config

    private val stamp: ArrivalStamp = ArrivalStamp(generation = 0, monotonicNanos = 1L)
    private val peer0: HeadPeerNumber = HeadPeerNumber(0)
    private val peer1: HeadPeerNumber = HeadPeerNumber(1)
    private val author0: PeerId = PeerId.Head(peer0)
    private val t0 = Instant.parse("2026-09-17T00:00:00Z")

    /** Declared above the tests that capture it: Scala 3's initialization checker cannot prove a
      * `val` defined lower down is set by the time a `test(...)` body registered during
      * construction reads it, and under CI's `-Werror` that warning fails the build.
      */
    private val unconstrained: AckRetention = AckRetention.unconstrained

    // ---- soft acks -------------------------------------------------------------------------

    test("soft acks at or below the bound are prunable, and nothing above it is") {
        val keys = withStore { p =>
            (0 to 5).toList.traverseSoftAcks(p, peer0) *>
                PrunableAcks.softAcks(p.backend, List(peer0), BlockNumber(3), unconstrained)
        }
        assert(softNums(keys) == List(0, 1, 2, 3), s"expected 0..3, got ${softNums(keys)}")
    }

    test("every head peer's journal is swept, not only this node's own") {
        val keys = withStore { p =>
            (0 to 2).toList.traverseSoftAcks(p, peer0) *>
                (0 to 2).toList.traverseSoftAcks(p, peer1) *>
                PrunableAcks.softAcks(p.backend, List(peer0, peer1), BlockNumber(1), unconstrained)
        }
        assert(keys.size == 4, s"two peers x blocks 0..1; got ${keys.size}"): Unit
        assert(keys.collect { case k: JournalKey.SoftAck => k.peer }.distinct.size == 2)
    }

    test("an empty journal yields nothing rather than failing") {
        val keys = withStore { p =>
            PrunableAcks.softAcks(p.backend, List(peer0), BlockNumber(10), unconstrained)
        }
        assert(keys.isEmpty)
    }

    /** The archive keeps the per-peer signatures, so an ack it has not taken is not ours to delete.
      */
    test("soft acks the archive has not reached are withheld") {
        val watermarks = ArchiveWatermarks.empty()
        watermarks.record(Map(Cf.SoftAck(peer0) -> 2L), t0): Unit

        val keys = withStore { p =>
            (0 to 5).toList.traverseSoftAcks(p, peer0) *>
                PrunableAcks.softAcks(
                  p.backend,
                  List(peer0),
                  BlockNumber(5),
                  AckRetention.forArchiver(Some(watermarks))
                )
        }
        assert(
          softNums(keys) == List(0, 1, 2),
          s"only what the archive holds; got ${softNums(keys)}"
        )
    }

    test("a declared archiver that has reported nothing withholds everything") {
        val keys = withStore { p =>
            (0 to 3).toList.traverseSoftAcks(p, peer0) *>
                PrunableAcks.softAcks(
                  p.backend,
                  List(peer0),
                  BlockNumber(3),
                  AckRetention.forArchiver(Some(ArchiveWatermarks.empty()))
                )
        }
        assert(keys.isEmpty, "an unaccounted-for archiver must hold everything back")
    }

    // ---- hard acks -------------------------------------------------------------------------

    /** A hard-ack's number cannot be computed from the stack number, so the walk reads each entry's
      * value. Two acks per stack here — the round-1 / round-2 pair a peer emits.
      */
    test("hard acks are matched by the stackNum in their value, not by their key") {
        val keys = withStore { p =>
            putHardAcks(p, author0, (0 to 5).toList.flatMap(s => List(s, s))) *>
                PrunableAcks.hardAcks(
                  p.backend,
                  List(author0),
                  StackNumber(2),
                  unconstrained,
                  protectedFamilies = Set.empty
                )
        }
        // Stacks 0,1,2 with two acks each: ack numbers 0..5.
        assert(hardNums(keys) == (0 to 5).toList, s"got ${hardNums(keys)}")
    }

    test("the walk stops at the first stack above the bound") {
        val keys = withStore { p =>
            putHardAcks(p, author0, List(0, 1, 2, 3, 4)) *>
                PrunableAcks.hardAcks(
                  p.backend,
                  List(author0),
                  StackNumber(1),
                  unconstrained,
                  protectedFamilies = Set.empty
                )
        }
        assert(hardNums(keys) == List(0, 1), s"stacks 0..1 only; got ${hardNums(keys)}")
    }

    /** ⛔ The family's highest key IS `hardAcked`. Emptying it does not lose history — it makes the
      * next boot read the store as cold and re-bootstrap from stack 0.
      */
    test("a marker family keeps its last row even when every entry is prunable") {
        val cf = Cf.HardAck(author0)
        val keys = withStore { p =>
            putHardAcks(p, author0, List(0, 1, 2)) *>
                PrunableAcks.hardAcks(
                  p.backend,
                  List(author0),
                  StackNumber(9),
                  unconstrained,
                  protectedFamilies = Set(cf)
                )
        }
        assert(hardNums(keys) == List(0, 1), s"the highest key must survive; got ${hardNums(keys)}")
    }

    test("the same family with no marker protection prunes to empty") {
        val keys = withStore { p =>
            putHardAcks(p, author0, List(0, 1, 2)) *>
                PrunableAcks.hardAcks(
                  p.backend,
                  List(author0),
                  StackNumber(9),
                  unconstrained,
                  protectedFamilies = Set.empty
                )
        }
        assert(hardNums(keys) == List(0, 1, 2))
    }

    /** Withholding is by key, so a family whose highest entry sits above the bound was never a
      * candidate and nothing extra is held back.
      */
    test("protection costs nothing when the highest key is above the bound anyway") {
        val cf = Cf.HardAck(author0)
        val keys = withStore { p =>
            putHardAcks(p, author0, List(0, 1, 2, 3)) *>
                PrunableAcks.hardAcks(
                  p.backend,
                  List(author0),
                  StackNumber(1),
                  unconstrained,
                  protectedFamilies = Set(cf)
                )
        }
        assert(hardNums(keys) == List(0, 1), s"got ${hardNums(keys)}")
    }

    test("hard acks the archive has not reached are withheld") {
        val watermarks = ArchiveWatermarks.empty()
        watermarks.record(Map(Cf.HardAck(author0) -> 1L), t0): Unit

        val keys = withStore { p =>
            putHardAcks(p, author0, List(0, 1, 2, 3)) *>
                PrunableAcks.hardAcks(
                  p.backend,
                  List(author0),
                  StackNumber(3),
                  AckRetention.forArchiver(Some(watermarks)),
                  protectedFamilies = Set.empty
                )
        }
        assert(hardNums(keys) == List(0, 1), s"only what the archive holds; got ${hardNums(keys)}")
    }

    // ---- fixtures --------------------------------------------------------------------------

    private def softNums(keys: List[JournalKey]): List[Int] =
        keys.collect { case k: JournalKey.SoftAck => k.num: Int }.sorted

    private def hardNums(keys: List[JournalKey]): List[Int] =
        keys.collect { case k: JournalKey.HardAck => k.num: Int }.sorted

    extension (blocks: List[Int])
        /** Write one soft-ack per block for `peer`; the ack number is the block number. */
        private def traverseSoftAcks(p: Persistence[IO], peer: HeadPeerNumber): IO[Unit] =
            blocks.foldLeft(IO.unit) { (acc, n) =>
                val ack = SoftAck(
                  peerNum = peer,
                  blockNum = BlockNumber(n),
                  signature = SoftAck.Signature(IArray.from(Array.fill[Byte](64)(0))),
                  finalizationRequested = false
                )
                acc *> p.put(JournalKey.SoftAck(peer, SoftAckNumber(n)))(JournalValue(stamp, ack))
            }

    /** Write one hard-ack per entry of `stacks`, numbered 0,1,2,… in order — so the list's shape
      * says which stack each ack number belongs to, including a stack that emits two rounds.
      */
    private def putHardAcks(p: Persistence[IO], peer: PeerId, stacks: List[Int]): IO[Unit] =
        stacks.zipWithIndex.foldLeft(IO.unit) { case (acc, (stack, ackNum)) =>
            val ack = HardAck(
              ackId = HardAckId(peer, HardAckNumber(ackNum)),
              stackNum = StackNumber(stack),
              payload =
                  HardAck.Round2Payload.Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
            )
            acc *> p.put(JournalKey.HardAck(peer, HardAckNumber(ackNum)))(JournalValue(stamp, ack))
        }

    /** Open a fresh in-memory store and run `body` against it. */
    private def withStore[A](body: Persistence[IO] => IO[A]): A =
        val tracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(tracer)
            .use(backend => Persistence.fromBackend(backend, tracer).flatMap(body))
            .unsafeRunSync()

end PrunableAcksTest
