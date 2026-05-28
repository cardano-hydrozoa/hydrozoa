package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse.*
import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.rocksdb.RocksDbPersistence
import java.nio.file.{Files, Path}
import java.util.Comparator
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

/** Smoke tests for the RocksDB persistence skeleton (P1).
  *
  * Each test gets its own fresh temp directory so they don't share state. Tests focus on the
  * skeleton's contract — open/close, put/get, atomic batch across CFs, range-scan, durability
  * across reopen, and the version-mismatch refusal.
  */
class RocksDbPersistenceTest extends AnyFunSuite:

    test("put then get returns the same bytes in the same CF") {
        withFreshStore { p =>
            val key = LaneKey.Block(BlockNumber(7)).encode
            val value = "hello".getBytes("UTF-8")
            for
                _ <- p.put(Cf.Block, key, value)
                got <- p.get(Cf.Block, key)
            yield assert(got.map(new String(_, "UTF-8")) == Some("hello"))
        }
    }

    test("get returns None for an absent key") {
        withFreshStore { p =>
            p.get(Cf.Block, LaneKey.Block(BlockNumber(42)).encode).map(g => assert(g.isEmpty))
        }
    }

    test("delete removes the entry") {
        withFreshStore { p =>
            val key = LaneKey.Stack(StackNumber(3)).encode
            for
                _ <- p.put(Cf.Stack, key, Array[Byte](1, 2, 3))
                _ <- p.delete(Cf.Stack, key)
                got <- p.get(Cf.Stack, key)
            yield assert(got.isEmpty)
        }
    }

    test("WriteBatch lands atomically across multiple CFs") {
        withFreshStore { p =>
            val blockKey = LaneKey.Block(BlockNumber(1)).encode
            val softKey = LaneKey.SoftAck(HeadPeerNumber(2), SoftAckNumber(1)).encode
            val batch =
                WriteBatch.empty
                    .put(Cf.Block, blockKey, Array[Byte](0xaa.toByte))
                    .put(Cf.SoftAck, softKey, Array[Byte](0xbb.toByte))
                    .put(Cf.DepositMap, "snap-key".getBytes("UTF-8"), Array[Byte](0xcc.toByte))
            for
                _ <- p.write(batch)
                a <- p.get(Cf.Block, blockKey)
                b <- p.get(Cf.SoftAck, softKey)
                c <- p.get(Cf.DepositMap, "snap-key".getBytes("UTF-8"))
            yield assert(
              a.map(_.head) == Some(0xaa.toByte) &&
                  b.map(_.head) == Some(0xbb.toByte) &&
                  c.map(_.head) == Some(0xcc.toByte)
            )
        }
    }

    test("cursor scans a satellite lane in ascending index order from the seek point") {
        withFreshStore { p =>
            val peer = HeadPeerNumber(0)
            val nums = List(0, 1, 2, 5, 10).map(HardAckNumber(_))
            val keys = nums.map(n => LaneKey.HardAck(peer, n))
            val seekFrom = LaneKey.HardAck(peer, HardAckNumber(1)).encode
            for
                _ <- keys.traverse(k => p.put(k, Array[Byte](1)))
                read <- p.cursor(Cf.HardAck, seekFrom).use { c =>
                    val drainAll: IO[List[(Array[Byte], Array[Byte])]] =
                        IO.unit.flatMap { _ =>
                            def loop(
                                acc: List[(Array[Byte], Array[Byte])]
                            ): IO[List[(Array[Byte], Array[Byte])]] =
                                c.next.flatMap {
                                    case Some(kv) => loop(kv :: acc)
                                    case None     => IO.pure(acc.reverse)
                                }
                            loop(Nil)
                        }
                    drainAll
                }
                got = read.map(kv => LaneKey.decode(Cf.HardAck, kv._1))
            yield
                val expected = List(1, 2, 5, 10)
                    .map(n => LaneKey.HardAck(peer, HardAckNumber(n)))
                assert(got == expected, s"got $got, expected $expected")
        }
    }

    test("data persists across close + reopen") {
        val tempDir = newTempDir()
        try
            val k = LaneKey.Block(BlockNumber(99)).encode
            val value = "durable".getBytes("UTF-8")
            // First session: write.
            RocksDbPersistence
                .open(tempDir)
                .use { p =>
                    p.put(Cf.Block, k, value)
                }
                .unsafeRunSync()
            // Second session: read back.
            val got = RocksDbPersistence
                .open(tempDir)
                .use { p =>
                    p.get(Cf.Block, k)
                }
                .unsafeRunSync()
            assert(got.map(new String(_, "UTF-8")) == Some("durable"))
        finally recursivelyDelete(tempDir)
    }

    test("lastKey returns None on an empty CF and the highest key after puts") {
        withFreshStore { p =>
            for
                emptyResult <- p.lastKey(Cf.SoftConfirmation)
                _ <- List(1, 5, 2, 42, 7).traverse { n =>
                    p.put(Cf.SoftConfirmation, LaneKey.intBytes(n), Array[Byte](n.toByte))
                }
                last <- p.lastKey(Cf.SoftConfirmation)
            yield assert(
              emptyResult.isEmpty &&
                  last.map(b => java.nio.ByteBuffer.wrap(b).getInt) == Some(42),
              s"empty=$emptyResult, last=$last"
            )
        }
    }

    test("lastKeyWithPrefix returns the highest key under the prefix") {
        withFreshStore { p =>
            val peer0 = HeadPeerNumber(0)
            val peer1 = HeadPeerNumber(1)
            val peer2 = HeadPeerNumber(2)
            val keys = List(
              LaneKey.SoftAck(peer0, SoftAckNumber(3)),
              LaneKey.SoftAck(peer1, SoftAckNumber(1)),
              LaneKey.SoftAck(peer1, SoftAckNumber(99)),
              LaneKey.SoftAck(peer1, SoftAckNumber(7)),
              LaneKey.SoftAck(peer2, SoftAckNumber(5))
            )
            for
                _ <- keys.traverse(k => p.put(k, Array[Byte](1)))
                got1 <- p.lastKeyWithPrefix(Cf.SoftAck, LaneKey.peerByte(peer1))
            yield
                val expected = LaneKey.SoftAck(peer1, SoftAckNumber(99)).encode
                assert(
                  got1.exists(java.util.Arrays.equals(_, expected)),
                  s"expected last(peer1)=SoftAck(peer1, 99), got $got1"
                )
        }
    }

    test("lastKeyWithPrefix returns None when no key matches the prefix") {
        withFreshStore { p =>
            val peer0 = HeadPeerNumber(0)
            val peer5 = HeadPeerNumber(5)
            for
                // Populate only peer-0 entries.
                _ <- List(1, 2, 3).traverse(n =>
                    p.put(LaneKey.SoftAck(peer0, SoftAckNumber(n)), Array[Byte](1))
                )
                missing <- p.lastKeyWithPrefix(Cf.SoftAck, LaneKey.peerByte(peer5))
            yield assert(missing.isEmpty, s"expected None, got $missing")
        }
    }

    test("opening with a wrong stored version is refused") {
        val tempDir = newTempDir()
        try
            // First open seeds the current version.
            RocksDbPersistence.open(tempDir).use(_ => IO.unit).unsafeRunSync()
            // Tamper: rewrite the version key with a bogus value.
            RocksDbPersistence
                .open(tempDir)
                .use { p =>
                    p.put(
                      Cf.Meta,
                      StoreVersion.key,
                      StoreVersion.encode(StoreVersion.current + 1)
                    )
                }
                .unsafeRunSync()
            // Reopen — must fail.
            val outcome =
                RocksDbPersistence.open(tempDir).use(_ => IO.unit).attempt.unsafeRunSync()
            assert(
              outcome.left.toOption
                  .exists(_.getMessage.contains("schema version mismatch")),
              s"expected version-mismatch failure, got $outcome"
            )
        finally recursivelyDelete(tempDir)
    }

    // ---- helpers ----

    /** Run `prog(persistence)` against a fresh temp-dir store; clean up afterward. */
    private def withFreshStore(prog: Persistence[IO] => IO[Assertion]): Assertion =
        val tempDir = newTempDir()
        try RocksDbPersistence.open(tempDir).use(prog).unsafeRunSync()
        finally recursivelyDelete(tempDir)

    private def newTempDir(): Path =
        Files.createTempDirectory("hydrozoa-rocksdb-test-")

    /** Recursively delete `root`, ignoring failures (test teardown is best-effort). */
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
