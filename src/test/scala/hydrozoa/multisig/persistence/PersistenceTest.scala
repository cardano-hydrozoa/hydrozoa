package hydrozoa.multisig.persistence

import cats.effect.{IO, IOLocal}
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.consensus.ack.SoftAckNumber
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import java.nio.file.{Files, Path}
import java.util.Comparator
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

/** Tests for the typed actor-facing [[Persistence]] API and [[WriteBatch]] — no `Cf.*` or
  * `Array[Byte]` at the call sites, just typed [[StoreKey]] + path-dependent `key.Value`.
  *
  * Under today's scaffolding every `StoreKey.Value` is `Array[Byte]` (codecs land per CF as the
  * first actor wires them); these tests use bytes accordingly. Once a CF gains a real `Value` type
  * and codec, the corresponding test will switch to constructing typed values.
  */
class PersistenceTest extends AnyFunSuite:

    given CardanoNetwork.Section = CardanoNetwork.Preview

    test("typed put/get round-trips a lane key") {
        withTypedStore { p =>
            val key = LaneKey.Block(BlockNumber(7))
            for
                _ <- p.put(key)(Array[Byte](0xab.toByte))
                got <- p.get(key)
            yield assert(got.map(_.head) == Some(0xab.toByte))
        }
    }

    test("typed delete removes a typed entry") {
        withTypedStore { p =>
            val key = StoreKey.DepositMap
            for
                _ <- p.put(key)(Array[Byte](1, 2, 3))
                _ <- p.delete(key)
                got <- p.get(key)
            yield assert(got.isEmpty)
        }
    }

    test("typed WriteBatch lands a 4-CF per-soft-ack bundle atomically") {
        // Mirrors §6 JointLedger's per-soft-ack write — only typed keys at the call site.
        withTypedStore { p =>
            val blockNum = BlockNumber(1)
            val ownPeer = HeadPeerNumber(0)
            val softNum = SoftAckNumber(1)
            val batch = WriteBatch.start
                .put(LaneKey.Block(blockNum))(Array[Byte](0xaa.toByte))
                .put(LaneKey.SoftAck(ownPeer, softNum))(Array[Byte](0xbb.toByte))
                .put(StoreKey.BlockResult(blockNum))(Array[Byte](0xcc.toByte))
                .put(StoreKey.DepositMap)(Array[Byte](0xdd.toByte))
            for
                _ <- p.write(batch)
                a <- p.get(LaneKey.Block(blockNum))
                b <- p.get(LaneKey.SoftAck(ownPeer, softNum))
                c <- p.get(StoreKey.BlockResult(blockNum))
                d <- p.get(StoreKey.DepositMap)
            yield assert(
              a.map(_.head) == Some(0xaa.toByte) &&
                  b.map(_.head) == Some(0xbb.toByte) &&
                  c.map(_.head) == Some(0xcc.toByte) &&
                  d.map(_.head) == Some(0xdd.toByte)
            )
        }
    }

    test("typed WriteBatch + delete in the same batch") {
        withTypedStore { p =>
            val key = StoreKey.DepositMap
            for
                _ <- p.put(key)(Array[Byte](1))
                _ <- p.write(WriteBatch.start.delete(key))
                got <- p.get(key)
            yield assert(got.isEmpty)
        }
    }

    test("the typed mirrors the slow-side close — 4 CFs in one WriteBatch") {
        // Mirrors §6 StackComposer's per-hard-ack stack-close write.
        withTypedStore { p =>
            val stackNum = StackNumber(0)
            val ownPeer = HeadPeerNumber(1)
            val hardNum = hydrozoa.multisig.consensus.ack.HardAckNumber(0)
            val emptyEvac = hydrozoa.multisig.ledger.joint.EvacuationMap.empty
            val treasury = hydrozoa.multisig.persistence.codec.TreasuryFixture.sampleTreasury
            val batch = WriteBatch.start
                .put(LaneKey.Stack(stackNum))(Array[Byte](1))
                .put(LaneKey.HardAck(ownPeer, hardNum))(Array[Byte](2))
                .put(StoreKey.Treasury)(treasury)
                .put(StoreKey.EvacuationMap)(emptyEvac)
            for
                _ <- p.write(batch)
                a <- p.get(LaneKey.Stack(stackNum))
                b <- p.get(LaneKey.HardAck(ownPeer, hardNum))
                c <- p.get(StoreKey.Treasury)
                d <- p.get(StoreKey.EvacuationMap)
            yield assert(
              a.map(_.head) == Some(1.toByte) &&
                  b.map(_.head) == Some(2.toByte) &&
                  c == Some(treasury) &&
                  d == Some(emptyEvac)
            )
        }
    }

    // ---- helpers ----

    private def withTypedStore(prog: Persistence[IO] => IO[Assertion]): Assertion =
        val tempDir = newTempDir()
        try
            (for
                tracerLocal <- Tracer.makeLocal
                result <- {
                    given IOLocal[Tracer] = tracerLocal
                    RocksDbBackendStore
                        .open(tempDir)
                        .use(backend => prog(Persistence.fromBackend(backend)))
                }
            yield result).unsafeRunSync()
        finally recursivelyDelete(tempDir)

    private def newTempDir(): Path =
        Files.createTempDirectory("hydrozoa-typed-persistence-test-")

    private def recursivelyDelete(root: Path): Unit =
        if Files.exists(root) then
            try
                val it = Files.walk(root)
                try
                    it.sorted(Comparator.reverseOrder())
                        .forEach { p =>
                            val _: Boolean =
                                try Files.deleteIfExists(p)
                                catch case _: Throwable => false
                        }
                finally it.close()
            catch case _: Throwable => ()
