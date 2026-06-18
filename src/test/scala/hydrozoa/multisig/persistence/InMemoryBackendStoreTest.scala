package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse.*
import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

/** Smoke tests for the in-memory [[BackendStore]] — mirrors the RocksDB test set to assert the two
  * implementations agree on the contract.
  */
class InMemoryBackendStoreTest extends AnyFunSuite:

    test("put then get returns the same bytes in the same CF") {
        withStore { p =>
            val key = JournalKey.Block(BlockNumber(7)).encode
            val value = "hello".getBytes("UTF-8")
            for
                _ <- p.put(Cf.Block, key, value)
                got <- p.get(Cf.Block, key)
            yield assert(got.map(new String(_, "UTF-8")) == Some("hello"))
        }
    }

    test("get returns None for an absent key") {
        withStore { p =>
            p.get(Cf.Block, JournalKey.Block(BlockNumber(42)).encode).map(g => assert(g.isEmpty))
        }
    }

    test("delete removes the entry") {
        withStore { p =>
            val key = JournalKey.Stack(StackNumber(3)).encode
            for
                _ <- p.put(Cf.Stack, key, Array[Byte](1, 2, 3))
                _ <- p.delete(Cf.Stack, key)
                got <- p.get(Cf.Stack, key)
            yield assert(got.isEmpty)
        }
    }

    test("RawWriteBatch lands atomically across multiple CFs") {
        withStore { p =>
            val blockKey = JournalKey.Block(BlockNumber(1)).encode
            val softPeer = HeadPeerNumber(2)
            val softKey = JournalKey.SoftAck(softPeer, SoftAckNumber(1)).encode
            val batch = RawWriteBatch.start
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
        withStore { p =>
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

    test("lastKey returns None on an empty CF and the highest key after puts") {
        withStore { p =>
            for
                emptyResult <- p.lastKey(Cf.SoftConfirmation)
                _ <- List(1, 5, 2, 42, 7).traverse { n =>
                    p.put(Cf.SoftConfirmation, JournalKey.intBytes(n), Array[Byte](n.toByte))
                }
                last <- p.lastKey(Cf.SoftConfirmation)
            yield assert(
              emptyResult.isEmpty &&
                  last.map(b => java.nio.ByteBuffer.wrap(b).getInt) == Some(42)
            )
        }
    }

    test("per-author CFs isolate each author's journal (lastKey scoped to one author's CF)") {
        // The per-author split (§7.1) replaces prefix scoping: each author's SoftAck journal is its own
        // CF, so `lastKey(Cf.SoftAck(peer))` returns only that author's high-water.
        withStore { p =>
            val peer0 = HeadPeerNumber(0)
            val peer1 = HeadPeerNumber(1)
            for
                _ <- List(SoftAckNumber(3)).traverse(n =>
                    p.put(Cf.SoftAck(peer0), JournalKey.SoftAck(peer0, n).encode, Array[Byte](1))
                )
                _ <- List(SoftAckNumber(1), SoftAckNumber(99), SoftAckNumber(7)).traverse(n =>
                    p.put(Cf.SoftAck(peer1), JournalKey.SoftAck(peer1, n).encode, Array[Byte](1))
                )
                got1 <- p.lastKey(Cf.SoftAck(peer1))
                got0 <- p.lastKey(Cf.SoftAck(peer0))
            yield assert(
              got1.exists(
                java.util.Arrays.equals(_, JournalKey.SoftAck(peer1, SoftAckNumber(99)).encode)
              ) &&
                  got0.exists(
                    java.util.Arrays.equals(_, JournalKey.SoftAck(peer0, SoftAckNumber(3)).encode)
                  )
            )
        }
    }

    private def withStore(prog: BackendStore[IO] => IO[Assertion]): Assertion =
        InMemoryBackendStore.open.use(prog).unsafeRunSync()
