package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import java.nio.file.{Files, Path}
import java.util.Comparator
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

/** Byte-level smoke tests for the RocksDB [[BackendStore]] implementation (P1).
  *
  * Each test gets its own fresh temp directory. Tests focus on the backend contract — open/close,
  * put/get, atomic `RawWriteBatch` across CFs, range-scan, `lastKey`, per-author CF isolation,
  * durability across reopen, and the version-mismatch refusal. The typed actor-facing API
  * ([[Persistence]] + [[WriteBatch]]) is exercised in `PersistenceTest`.
  */
class RocksDbBackendStoreTest extends AnyFunSuite:

    private val tracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)

    test("put then get returns the same bytes in the same CF") {
        withFreshStore { p =>
            val key = JournalKey.Block(BlockNumber(7)).encode
            val value = "hello".getBytes("UTF-8")
            for
                _ <- p.put(Cf.Block, key, value)
                got <- p.get(Cf.Block, key)
            yield assert(got.map(new String(_, "UTF-8")) == Some("hello"))
        }
    }

    test("get returns None for an absent key") {
        withFreshStore { p =>
            p.get(Cf.Block, JournalKey.Block(BlockNumber(42)).encode).map(g => assert(g.isEmpty))
        }
    }

    test("delete removes the entry") {
        withFreshStore { p =>
            val key = JournalKey.Stack(StackNumber(3)).encode
            for
                _ <- p.put(Cf.Stack, key, Array[Byte](1, 2, 3))
                _ <- p.delete(Cf.Stack, key)
                got <- p.get(Cf.Stack, key)
            yield assert(got.isEmpty)
        }
    }

    test("RawWriteBatch lands atomically across multiple CFs") {
        withFreshStore { p =>
            val blockKey = JournalKey.Block(BlockNumber(1)).encode
            val softPeer = HeadPeerNumber(2)
            val softKey = JournalKey.SoftAck(softPeer, SoftAckNumber(1)).encode
            val batch =
                RawWriteBatch.start
                    .put(Cf.Block, blockKey, Array[Byte](0xaa.toByte))
                    .put(Cf.SoftAck(softPeer), softKey, Array[Byte](0xbb.toByte))
                    .put(Cf.DepositMap, "snap-key".getBytes("UTF-8"), Array[Byte](0xcc.toByte))
            for
                _ <- p.write(batch)
                a <- p.get(Cf.Block, blockKey)
                b <- p.get(Cf.SoftAck(softPeer), softKey)
                c <- p.get(Cf.DepositMap, "snap-key".getBytes("UTF-8"))
            yield assert(
              a.map(_.head) == Some(0xaa.toByte) &&
                  b.map(_.head) == Some(0xbb.toByte) &&
                  c.map(_.head) == Some(0xcc.toByte)
            )
        }
    }

    test("cursor scans a satellite journal in ascending index order from the seek point") {
        withFreshStore { p =>
            val peer = PeerId.Head(HeadPeerNumber(0))
            val nums = List(0, 1, 2, 5, 10).map(HardAckNumber(_))
            val keys = nums.map(n => JournalKey.HardAck(peer, n))
            val seekFrom = JournalKey.HardAck(peer, HardAckNumber(1)).encode
            for
                _ <- keys.traverse(k => p.put(Cf.HardAck(peer), k.encode, Array[Byte](1)))
                read <- p.cursor(Cf.HardAck(peer), seekFrom).use { c =>
                    def loop(
                        acc: List[(Array[Byte], Array[Byte])]
                    ): IO[List[(Array[Byte], Array[Byte])]] =
                        c.next.flatMap {
                            case Some(kv) => loop(kv :: acc)
                            case None     => IO.pure(acc.reverse)
                        }
                    loop(Nil)
                }
                got = read.map(kv => JournalKey.decode(Cf.HardAck(peer), kv._1))
            yield
                val expected = List(1, 2, 5, 10)
                    .map(n => JournalKey.HardAck(peer, HardAckNumber(n)))
                assert(got == expected, s"got $got, expected $expected")
        }
    }

    test("data persists across close + reopen") {
        val tempDir = newTempDir()
        try
            val k = JournalKey.Block(BlockNumber(99)).encode
            val value = "durable".getBytes("UTF-8")
            // First session: write.
            RocksDbBackendStore
                .open(tempDir, testCfs, tracer)
                .use { p =>
                    p.put(Cf.Block, k, value)
                }
                .unsafeRunSync()
            // Second session: read back.
            val got = RocksDbBackendStore
                .open(tempDir, testCfs, tracer)
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
                    p.put(Cf.SoftConfirmation, JournalKey.intBytes(n), Array[Byte](n.toByte))
                }
                last <- p.lastKey(Cf.SoftConfirmation)
            yield assert(
              emptyResult.isEmpty &&
                  last.map(b => java.nio.ByteBuffer.wrap(b).getInt) == Some(42),
              s"empty=$emptyResult, last=$last"
            )
        }
    }

    test("per-author CFs isolate each author's journal; an unwritten author's CF is empty") {
        // The per-author split (§7.1) replaces prefix scoping: each author's SoftAck journal is its own
        // CF, so `lastKey(Cf.SoftAck(peer))` returns only that author's high-water, and an author
        // never written to has an empty CF.
        withFreshStore { p =>
            val peer0 = HeadPeerNumber(0)
            val peer1 = HeadPeerNumber(1)
            val peer5 = HeadPeerNumber(5)
            for
                _ <- List(SoftAckNumber(3)).traverse(n =>
                    p.put(Cf.SoftAck(peer0), JournalKey.SoftAck(peer0, n).encode, Array[Byte](1))
                )
                _ <- List(SoftAckNumber(1), SoftAckNumber(99), SoftAckNumber(7)).traverse(n =>
                    p.put(Cf.SoftAck(peer1), JournalKey.SoftAck(peer1, n).encode, Array[Byte](1))
                )
                got1 <- p.lastKey(Cf.SoftAck(peer1))
                got0 <- p.lastKey(Cf.SoftAck(peer0))
                missing <- p.lastKey(Cf.SoftAck(peer5))
            yield assert(
              got1.exists(
                java.util.Arrays.equals(_, JournalKey.SoftAck(peer1, SoftAckNumber(99)).encode)
              ) &&
                  got0.exists(
                    java.util.Arrays.equals(_, JournalKey.SoftAck(peer0, SoftAckNumber(3)).encode)
                  ) &&
                  missing.isEmpty,
              s"got1=$got1, got0=$got0, missing=$missing"
            )
        }
    }

    test("opening with a wrong stored version is refused") {
        val tempDir = newTempDir()
        try
            // First open seeds the current version.
            RocksDbBackendStore.open(tempDir, testCfs, tracer).use(_ => IO.unit).unsafeRunSync()
            // Tamper: rewrite the version key with a bogus value.
            RocksDbBackendStore
                .open(tempDir, testCfs, tracer)
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
                RocksDbBackendStore
                    .open(tempDir, testCfs, tracer)
                    .use(_ => IO.unit)
                    .attempt
                    .unsafeRunSync()
            assert(
              outcome.left.toOption
                  .exists(_.getMessage.contains("schema version mismatch")),
              s"expected version-mismatch failure, got $outcome"
            )
        finally recursivelyDelete(tempDir)
    }

    // ---- helpers ----

    /** The config-derived CF set these tests open over (§7.1, the per-author split): the fixed CFs
      * plus per-author satellites for head peers 0..5 — covering every author the tests touch. The
      * reopen tests must use the same list (RocksDB matches the descriptor set on reopen).
      */
    private val testCfs: List[Cf] =
        Cf.mkAll((0 to 5).map(HeadPeerNumber(_)).toList, Nil, Nil)

    /** Run `prog(backend)` against a fresh temp-dir store; clean up afterward. */
    private def withFreshStore(prog: BackendStore[IO] => IO[Assertion]): Assertion =
        val tempDir = newTempDir()
        try RocksDbBackendStore.open(tempDir, testCfs, tracer).use(prog).unsafeRunSync()
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
