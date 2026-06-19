package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l2.L2CommandNumber
import hydrozoa.multisig.persistence.codec.TreasuryFixture
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import java.nio.file.{Files, Path}
import java.util.Comparator
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

/** Tests for the typed actor-facing [[Persistence]] API and [[WriteBatch]] — no `Cf.*` or
  * `Array[Byte]` at the call sites, just typed [[StoreKey]] + path-dependent `key.Value`.
  *
  * CFs with a real `Value` type round-trip their typed payload (e.g. `DepositMap` →
  * [[DepositsMap]], `Treasury` → `MultisigTreasuryUtxo`, `EvacuationMap`); journal CFs carry a
  * [[JournalValue]] (`StoreCodec.journalValue` framing, exercised end-to-end by stage1/stage4) and
  * only `Meta` stays on the `Array[Byte]` passthrough.
  */
class PersistenceTest extends AnyFunSuite:

    given CardanoNetwork.Section = CardanoNetwork.Preview

    /** The config-derived CF set (§7.1) — head peers 0..3, no coil; covers every CF these tests
      * touch (fixed CFs plus a few per-author satellites).
      */
    private val testCfs: List[Cf] =
        Cf.mkAll((0 to 3).map(HeadPeerNumber(_)).toList, Nil, Nil)

    test("typed put/get round-trips a typed value") {
        withTypedStore { p =>
            val key = StoreKey.Treasury
            val treasury = TreasuryFixture.sampleTreasury
            for
                _ <- p.put(key)(treasury)
                got <- p.get(key)
            yield assert(got == Some(treasury))
        }
    }

    test("RequestHighWater round-trips per-block high-water maps under distinct keys") {
        withTypedStore { p =>
            val atBlock3 = Map(HeadPeerNumber(0) -> RequestNumber(3))
            val atBlock4 =
                Map(HeadPeerNumber(0) -> RequestNumber(3), HeadPeerNumber(2) -> RequestNumber(40))
            for
                _ <- p.put(StoreKey.RequestHighWater(BlockNumber(3)))(atBlock3)
                _ <- p.put(StoreKey.RequestHighWater(BlockNumber(4)))(atBlock4)
                got3 <- p.get(StoreKey.RequestHighWater(BlockNumber(3)))
                got4 <- p.get(StoreKey.RequestHighWater(BlockNumber(4)))
            yield assert(got3 == Some(atBlock3) && got4 == Some(atBlock4))
        }
    }

    test("L2CommandNumber round-trips per-block command numbers under distinct keys") {
        withTypedStore { p =>
            for
                _ <- p.put(StoreKey.L2CommandNumber(BlockNumber(3)))(L2CommandNumber(7L))
                _ <- p.put(StoreKey.L2CommandNumber(BlockNumber(4)))(L2CommandNumber(8L))
                got3 <- p.get(StoreKey.L2CommandNumber(BlockNumber(3)))
                got4 <- p.get(StoreKey.L2CommandNumber(BlockNumber(4)))
            yield assert(got3 == Some(L2CommandNumber(7L)) && got4 == Some(L2CommandNumber(8L)))
        }
    }

    test("typed delete removes a typed entry") {
        withTypedStore { p =>
            val key = StoreKey.DepositMap
            for
                _ <- p.put(key)(DepositsMap.empty)
                _ <- p.delete(key)
                got <- p.get(key)
            yield assert(got.isEmpty)
        }
    }

    test("typed WriteBatch lands a 4-CF bundle atomically across distinct CFs") {
        // Atomic multi-CF write, using the CFs with easy fixtures (typed snapshots + Meta bytes).
        // Journal-CF (`JournalValue`) round-trips are exercised end-to-end by the stage1/stage4 runs;
        // the framing itself is covered below.
        withTypedStore { p =>
            val treasury = TreasuryFixture.sampleTreasury
            val evacKey = StoreKey.EvacuationMap(BlockNumber(1))
            val batch = WriteBatch.start
                .put(StoreKey.DepositMap)(DepositsMap.empty)
                .put(StoreKey.Treasury)(treasury)
                .put(evacKey)(EvacuationMap.empty)
                .put(StoreKey.Meta("schema-version"))(Array[Byte](0xcc.toByte))
            for
                _ <- p.write(batch)
                a <- p.get(StoreKey.DepositMap)
                b <- p.get(StoreKey.Treasury)
                c <- p.get(evacKey)
                d <- p.get(StoreKey.Meta("schema-version"))
            yield assert(
              a == Some(DepositsMap.empty) &&
                  b == Some(treasury) &&
                  c == Some(EvacuationMap.empty) &&
                  d.map(_.head) == Some(0xcc.toByte)
            )
        }
    }

    test("typed WriteBatch + delete in the same batch") {
        withTypedStore { p =>
            val key = StoreKey.DepositMap
            for
                _ <- p.put(key)(DepositsMap.empty)
                _ <- p.write(WriteBatch.start.delete(key))
                got <- p.get(key)
            yield assert(got.isEmpty)
        }
    }

    test("the typed mirrors the slow-side snapshot — Treasury + EvacuationMap atomically") {
        // Mirrors §6 StackComposer's per-hard-ack snapshot write (the Treasury + per-block
        // EvacuationMap pair landed in one batch).
        withTypedStore { p =>
            val evacKey = StoreKey.EvacuationMap(BlockNumber(0))
            val emptyEvac = EvacuationMap.empty
            val treasury = TreasuryFixture.sampleTreasury
            val batch = WriteBatch.start
                .put(StoreKey.Treasury)(treasury)
                .put(evacKey)(emptyEvac)
            for
                _ <- p.write(batch)
                c <- p.get(StoreKey.Treasury)
                d <- p.get(evacKey)
            yield assert(c == Some(treasury) && d == Some(emptyEvac))
        }
    }

    test("JournalValue framing round-trips ArrivalStamp + payload bytes") {
        val stamp = ArrivalStamp(generation = 7, monotonicNanos = 0x0102030405060708L)
        val payload = Array[Byte](0xaa.toByte, 0xbb.toByte, 0xcc.toByte)
        val framed = JournalValue.frame(stamp, payload)
        assert(
          JournalValue.stamp(framed) == stamp &&
              java.util.Arrays.equals(JournalValue.payload(framed), payload) &&
              framed.length == JournalValue.stampWidth + payload.length
        )
    }

    // ---- helpers ----

    private def withTypedStore(prog: Persistence[IO] => IO[Assertion]): Assertion =
        val tempDir = newTempDir()
        try
            (for
                tracerLocal <- Slf4jTracer.makeLocal
                result <- {
                    RocksDbBackendStore
                        .open(tempDir, testCfs)
                        .use(backend => Persistence.fromBackend(backend).flatMap(prog))
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
