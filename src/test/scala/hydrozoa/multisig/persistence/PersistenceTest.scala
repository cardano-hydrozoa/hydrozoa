package hydrozoa.multisig.persistence

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOLocal}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
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
  * [[DepositsMap]], `Treasury` → `MultisigTreasuryUtxo`, `EvacuationMap`); lane CFs carry a
  * [[LaneValue]] (`StoreCodec.laneValue` framing, exercised end-to-end by stage1/stage4) and only
  * `Meta` stays on the `Array[Byte]` passthrough.
  */
class PersistenceTest extends AnyFunSuite:

    given CardanoNetwork.Section = CardanoNetwork.Preview

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
        // Lane-CF (`LaneValue`) round-trips are exercised end-to-end by the stage1/stage4 runs;
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

    test("LaneValue framing round-trips stamp + payload bytes") {
        val stamp = 0x0102030405060708L
        val payload = Array[Byte](0xaa.toByte, 0xbb.toByte, 0xcc.toByte)
        val framed = LaneValue.frame(stamp, payload)
        assert(
          LaneValue.stamp(framed) == stamp &&
              java.util.Arrays.equals(LaneValue.payload(framed), payload) &&
              framed.length == LaneValue.stampWidth + payload.length
        )
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
