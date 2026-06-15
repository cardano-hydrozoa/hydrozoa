package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.ack.SoftAckNumber
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import java.nio.file.{Files, Path}
import java.util.Comparator
import org.scalatest.funsuite.AnyFunSuite

/** Smoke tests for [[StoreDump]] over a populated store — checks per-CF entry counts in the stats
  * table and the presence of expected decoded keys in the dump.
  */
class StoreDumpTest extends AnyFunSuite:

    test("stats counts entries per CF after a mixed-CF write") {
        val tempDir = newTempDir()
        try
            val ownPeer = HeadPeerNumber(0)
            val populate = (b: BackendStore[IO]) =>
                b.write(
                  RawWriteBatch.start
                      .put(Cf.Block, LaneKey.Block(BlockNumber(1)).encode, Array[Byte](1, 2, 3))
                      .put(Cf.Block, LaneKey.Block(BlockNumber(2)).encode, Array[Byte](1, 2, 3, 4))
                      .put(
                        Cf.SoftAck,
                        LaneKey.SoftAck(ownPeer, SoftAckNumber(1)).encode,
                        Array[Byte](9)
                      )
                      .put(
                        Cf.BlockResult,
                        StoreKey.BlockResult(BlockNumber(1)).encode,
                        Array[Byte](7, 7)
                      )
                      .put(
                        Cf.SoftConfirmation,
                        StoreKey.SoftConfirmation(BlockNumber(1)).encode,
                        Array[Byte](8, 8, 8)
                      )
                      .put(
                        Cf.HardConfirmation,
                        StoreKey.HardConfirmation(StackNumber(0)).encode,
                        Array[Byte](5)
                      )
                      .put(
                        Cf.DepositMap,
                        StoreKey.DepositMap.encode,
                        Array.fill[Byte](32)(0xab.toByte)
                      )
                      .put(
                        Cf.Treasury,
                        StoreKey.Treasury.encode,
                        Array.fill[Byte](16)(0xcd.toByte)
                      )
                )
            val stats = RocksDbBackendStore
                .open(tempDir, ContraTracer.nullTracer)
                .use(b => populate(b) *> StoreDump.stats(b))
                .unsafeRunSync()

            // Build a name → entries map to assert key counts independent of CF ordering.
            val byCf = stats.perCf.map(s => s.cf -> s.entries).toMap
            val rendered = stats.render
            // 8 we wrote + the schema-version entry seeded by `RocksDbBackendStore.open` = 9.
            assert(
              byCf(Cf.Block) == 2 &&
                  byCf(Cf.SoftAck) == 1 &&
                  byCf(Cf.BlockResult) == 1 &&
                  byCf(Cf.SoftConfirmation) == 1 &&
                  byCf(Cf.HardConfirmation) == 1 &&
                  byCf(Cf.DepositMap) == 1 &&
                  byCf(Cf.Treasury) == 1 &&
                  byCf(Cf.Meta) == 1 &&
                  stats.total.entries == 9 &&
                  rendered.contains("TOTAL") &&
                  rendered.contains("Block"),
              s"stats=$stats\nrendered:\n$rendered"
            )
        finally recursivelyDelete(tempDir)
    }

    test("dump pretty-prints decoded keys per CF (lanes, spine-indexed, singletons, meta)") {
        val tempDir = newTempDir()
        try
            val ownPeer = HeadPeerNumber(3)
            val rendered = RocksDbBackendStore
                .open(tempDir, ContraTracer.nullTracer)
                .use { b =>
                    b.write(
                      RawWriteBatch.start
                          .put(Cf.Block, LaneKey.Block(BlockNumber(7)).encode, Array[Byte](1))
                          .put(
                            Cf.SoftAck,
                            LaneKey.SoftAck(ownPeer, SoftAckNumber(2)).encode,
                            Array[Byte](1)
                          )
                          .put(
                            Cf.SoftConfirmation,
                            StoreKey.SoftConfirmation(BlockNumber(7)).encode,
                            Array[Byte](1)
                          )
                          .put(
                            Cf.HardConfirmation,
                            StoreKey.HardConfirmation(StackNumber(0)).encode,
                            Array[Byte](1)
                          )
                          .put(Cf.DepositMap, StoreKey.DepositMap.encode, Array[Byte](1, 2, 3))
                    ) *> StoreDump.dump(b)
                }
                .unsafeRunSync()
            // Each entry should print its decoded form somewhere in the dump. Note that opaque-Int
            // newtypes (BlockNumber, SoftAckNumber, …) unwrap to their underlying value in toString.
            assert(
              rendered.contains("Block(7)") &&
                  rendered.contains("SoftAck(3,2)") &&
                  rendered.contains("SoftConfirmation(7)") &&
                  rendered.contains("HardConfirmation(0)") &&
                  rendered.contains("(singleton)") &&
                  rendered.contains("Meta("),
              rendered
            )
        finally recursivelyDelete(tempDir)
    }

    private def newTempDir(): Path =
        Files.createTempDirectory("hydrozoa-rocksdb-dump-test-")

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
