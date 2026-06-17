package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse.*
import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.*
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

/** Unit tests for [[FamilyScan]] — that it returns a family's tail in index order from the cursor,
  * recovers the arrival stamp, and stops at the satellite peer boundary (the §7.1 multiplexing).
  */
class FamilyScanTest extends AnyFunSuite:

    private def stampN(n: Int): ArrivalStamp = ArrivalStamp(1, n.toLong)

    /** Put a framed family value (`[stamp : 12][payload …]`) at `key` with an arbitrary payload. */
    private def putLane(backend: BackendStore[IO], key: FamilyKey, stamp: ArrivalStamp): IO[Unit] =
        val framed = FamilyValue.frame(stamp, s"payload-$key".getBytes("UTF-8"))
        backend.put(key.cf, key.encode, framed)

    test("scan returns a spine family from the cursor (inclusive) to the end, in index order") {
        withStore { backend =>
            val keys = List(0, 1, 2, 3).map(n => FamilyKey.Block(BlockNumber(n)))
            for
                _ <- keys.traverse(k => putLane(backend, k, stampN(k.num)))
                entries <- FamilyScan.scan(backend, FamilyKey.Block(BlockNumber(1)))
            yield
                assert(entries.map(_.key) == keys.drop(1))
                assert(entries.map(_.stamp) == List(1, 2, 3).map(stampN))
        }
    }

    test("scan stops at the satellite peer boundary — does not bleed into the next author") {
        withStore { backend =>
            val peer0 = PeerId.Head(HeadPeerNumber(0))
            val peer1 = PeerId.Head(HeadPeerNumber(1))
            val p0Keys = List(0, 1, 2).map(n => FamilyKey.HardAck(peer0, HardAckNumber(n)))
            val p1Keys = List(0, 1).map(n => FamilyKey.HardAck(peer1, HardAckNumber(n)))
            for
                _ <- (p0Keys ++ p1Keys).traverse(k => putLane(backend, k, stampN(0)))
                fromP0 <- FamilyScan.scan(backend, FamilyKey.HardAck(peer0, HardAckNumber(0)))
                fromP1 <- FamilyScan.scan(backend, FamilyKey.HardAck(peer1, HardAckNumber(0)))
            yield
                assert(fromP0.map(_.key) == p0Keys, "peer 0 scan must stop before peer 1")
                assert(fromP1.map(_.key) == p1Keys)
        }
    }

    test("scan from a mid-family cursor skips earlier indices") {
        withStore { backend =>
            val peer = PeerId.Head(HeadPeerNumber(2))
            val keys = List(0, 1, 5, 9).map(n => FamilyKey.HardAck(peer, HardAckNumber(n)))
            for
                _ <- keys.traverse(k => putLane(backend, k, stampN(0)))
                entries <- FamilyScan.scan(backend, FamilyKey.HardAck(peer, HardAckNumber(2)))
            yield assert(
              entries.map(_.key) == List(5, 9).map(n => FamilyKey.HardAck(peer, HardAckNumber(n)))
            )
        }
    }

    test("scan of an empty family is empty") {
        withStore { backend =>
            FamilyScan.scan(backend, FamilyKey.Block(BlockNumber(0))).map(e => assert(e.isEmpty))
        }
    }

    test("payloadBytes round-trips the framed wire payload") {
        withStore { backend =>
            val key = FamilyKey.Block(BlockNumber(0))
            for
                _ <- putLane(backend, key, stampN(7))
                entries <- FamilyScan.scan(backend, key)
            yield
                assert(entries.size == 1)
                assert(new String(entries.head.payloadBytes, "UTF-8") == s"payload-$key")
                assert(entries.head.stamp == stampN(7))
        }
    }

    test("round-trip: derive -> scanFamilies -> merge over a full multi-peer multi-family store") {
        withStore { backend =>
            // Three peers, including a high byte (200 = 0xC8, negative as a signed byte) to catch
            // any signed-vs-unsigned peer-byte slip at the satellite scan boundary.
            val peers = List(HeadPeerNumber(0), HeadPeerNumber(1), HeadPeerNumber(200))
            val markers = Markers(
              softConfirmed = Some(BlockNumber(2)), // block floor 3, soft-ack floor 3
              fastBlockMark = None,
              hardConfirmed = Some(StackNumber(1)), // stack floor 2
              hardAcked = Some(HardAckNumber(4))
            )
            val highWater = Map(
              HeadPeerNumber(0) -> RequestNumber(4), // request floor 5
              HeadPeerNumber(200) -> RequestNumber(1) // request floor 2; peer 1 absent -> floor 0
            )
            // scanFamilies uses the lower (confirmed) spine floors, so the fast anchor and the acked
            // stack (the ledger/composer floors) are irrelevant to this round-trip; pass arbitrary
            // values.
            val cursors =
                ReplayCursors.derive(
                  markers,
                  peers,
                  Nil,
                  highWater,
                  hardAckedStack = Some(StackNumber(1)),
                  own = PeerId.Head(HeadPeerNumber(0))
                )

            // For each family: a few indices below its floor (must be skipped) and a few at/above it
            // (must be returned). Stamp generation alternates per entry so the merge's cross-process
            // ordering (generation dominates monotonicNanos) is exercised on real scanned entries.
            val below: List[FamilyKey] = List(
              FamilyKey.Block(BlockNumber(0)),
              FamilyKey.Block(BlockNumber(2)),
              FamilyKey.Stack(StackNumber(0)),
              FamilyKey.Request(HeadPeerNumber(0), RequestNumber(4)),
              FamilyKey.Request(HeadPeerNumber(200), RequestNumber(1)),
              FamilyKey.SoftAck(HeadPeerNumber(0), SoftAckNumber(2)),
              FamilyKey.SoftAck(HeadPeerNumber(200), SoftAckNumber(2))
            )
            val above: List[FamilyKey] = List(
              FamilyKey.Block(BlockNumber(3)),
              FamilyKey.Block(BlockNumber(4)),
              FamilyKey.Stack(StackNumber(2)),
              FamilyKey.Stack(StackNumber(3)),
              FamilyKey.Request(HeadPeerNumber(0), RequestNumber(5)),
              FamilyKey.Request(HeadPeerNumber(1), RequestNumber(0)),
              FamilyKey.Request(HeadPeerNumber(200), RequestNumber(2)),
              FamilyKey.SoftAck(HeadPeerNumber(0), SoftAckNumber(3)),
              FamilyKey.SoftAck(HeadPeerNumber(1), SoftAckNumber(3)),
              FamilyKey.SoftAck(HeadPeerNumber(200), SoftAckNumber(4)),
              FamilyKey.HardAck(PeerId.Head(HeadPeerNumber(0)), HardAckNumber(0)),
              FamilyKey.HardAck(PeerId.Head(HeadPeerNumber(1)), HardAckNumber(1)),
              FamilyKey.HardAck(PeerId.Head(HeadPeerNumber(200)), HardAckNumber(0))
            )
            // Stamp gen alternates 1/2 across the combined seed list while monotonic increases, so
            // some gen-1 entries get a higher monotonic than some gen-2 ones — a mono-only sort
            // would mis-order, pinning that generation is the high-order key.
            val seed = (below ++ above).zipWithIndex.map { case (k, i) =>
                k -> ArrivalStamp((i % 2) + 1, i.toLong)
            }
            for
                _ <- seed.traverse { case (k, stamp) =>
                    backend.put(k.cf, k.encode, FamilyValue.frame(stamp, s"v-$k".getBytes("UTF-8")))
                }
                perLane <- FamilyScan.scanFamilies(backend, cursors)
                merged = ArrivalOrderedMerge.merge(perLane)
            yield
                val gotKeys = merged.map(_.key)
                // (a) exactly the above-floor keys: no below-floor leak, no cross-peer bleed.
                assert(gotKeys.toSet == above.toSet, s"got ${gotKeys.toSet}")
                // (b)+(d) nothing dropped or duplicated: count matches both the expected set and the
                // sum of per-family tail sizes.
                assert(gotKeys.size == above.size)
                assert(merged.size == perLane.map(_.size).sum)
                // (c) merged stream is exactly arrival-stamp ordered (generation, then monotonic).
                val stamps = merged.map(e => (e.stamp.generation, e.stamp.monotonicNanos))
                assert(stamps == stamps.sorted)
        }
    }

    private def withStore(prog: BackendStore[IO] => IO[Assertion]): Assertion =
        InMemoryBackendStore.open.use(prog).unsafeRunSync()
