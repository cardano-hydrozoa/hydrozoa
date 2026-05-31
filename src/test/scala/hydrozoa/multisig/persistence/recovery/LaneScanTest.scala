package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse.*
import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.*
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

/** Unit tests for [[LaneScan]] — that it returns a lane's tail in index order from the cursor,
  * recovers the arrival stamp, and stops at the satellite peer boundary (the §7.1 multiplexing).
  */
class LaneScanTest extends AnyFunSuite:

    private def stampN(n: Int): ArrivalStamp = ArrivalStamp(1, n.toLong)

    /** Put a framed lane value (`[stamp : 12][payload …]`) at `key` with an arbitrary payload. */
    private def putLane(backend: BackendStore[IO], key: LaneKey, stamp: ArrivalStamp): IO[Unit] =
        val framed = LaneValue.frame(stamp, s"payload-$key".getBytes("UTF-8"))
        backend.put(key.cf, key.encode, framed)

    test("scan returns a spine lane from the cursor (inclusive) to the end, in index order") {
        withStore { backend =>
            val keys = List(0, 1, 2, 3).map(n => LaneKey.Block(BlockNumber(n)))
            for
                _ <- keys.traverse(k => putLane(backend, k, stampN(k.num)))
                entries <- LaneScan.scan(backend, LaneKey.Block(BlockNumber(1)))
            yield
                assert(entries.map(_.key) == keys.drop(1))
                assert(entries.map(_.stamp) == List(1, 2, 3).map(stampN))
        }
    }

    test("scan stops at the satellite peer boundary — does not bleed into the next author") {
        withStore { backend =>
            val peer0 = HeadPeerNumber(0)
            val peer1 = HeadPeerNumber(1)
            val p0Keys = List(0, 1, 2).map(n => LaneKey.HardAck(peer0, HardAckNumber(n)))
            val p1Keys = List(0, 1).map(n => LaneKey.HardAck(peer1, HardAckNumber(n)))
            for
                _ <- (p0Keys ++ p1Keys).traverse(k => putLane(backend, k, stampN(0)))
                fromP0 <- LaneScan.scan(backend, LaneKey.HardAck(peer0, HardAckNumber(0)))
                fromP1 <- LaneScan.scan(backend, LaneKey.HardAck(peer1, HardAckNumber(0)))
            yield
                assert(fromP0.map(_.key) == p0Keys, "peer 0 scan must stop before peer 1")
                assert(fromP1.map(_.key) == p1Keys)
        }
    }

    test("scan from a mid-lane cursor skips earlier indices") {
        withStore { backend =>
            val peer = HeadPeerNumber(2)
            val keys = List(0, 1, 5, 9).map(n => LaneKey.HardAck(peer, HardAckNumber(n)))
            for
                _ <- keys.traverse(k => putLane(backend, k, stampN(0)))
                entries <- LaneScan.scan(backend, LaneKey.HardAck(peer, HardAckNumber(2)))
            yield assert(
              entries.map(_.key) == List(5, 9).map(n => LaneKey.HardAck(peer, HardAckNumber(n)))
            )
        }
    }

    test("scan of an empty lane is empty") {
        withStore { backend =>
            LaneScan.scan(backend, LaneKey.Block(BlockNumber(0))).map(e => assert(e.isEmpty))
        }
    }

    test("payloadBytes round-trips the framed wire payload") {
        withStore { backend =>
            val key = LaneKey.Block(BlockNumber(0))
            for
                _ <- putLane(backend, key, stampN(7))
                entries <- LaneScan.scan(backend, key)
            yield
                assert(entries.size == 1)
                assert(new String(entries.head.payloadBytes, "UTF-8") == s"payload-$key")
                assert(entries.head.stamp == stampN(7))
        }
    }

    test("round-trip: derive -> scanLanes -> merge over a full multi-peer multi-lane store") {
        withStore { backend =>
            // Three peers, including a high byte (200 = 0xC8, negative as a signed byte) to catch
            // any signed-vs-unsigned peer-byte slip at the satellite scan boundary.
            val peers = List(HeadPeerNumber(0), HeadPeerNumber(1), HeadPeerNumber(200))
            val markers = Markers(
              softConfirmed = Some(BlockNumber(2)), // block floor 3, soft-ack floor 3
              hardConfirmed = Some(StackNumber(1)), // stack floor 2
              softAcked = Some(SoftAckNumber(4)),
              hardAcked = Some(HardAckNumber(4))
            )
            val highWater = Map(
              HeadPeerNumber(0) -> RequestNumber(4), // request floor 5
              HeadPeerNumber(200) -> RequestNumber(1) // request floor 2; peer 1 absent -> floor 0
            )
            // scanLanes uses the lower (confirmed) spine floors, so the acked stack is irrelevant
            // to this round-trip; pass an arbitrary present value.
            val cursors =
                ReplayCursors.derive(
                  markers,
                  peers,
                  highWater,
                  hardAckedStack = Some(StackNumber(1))
                )

            // For each lane: a few indices below its floor (must be skipped) and a few at/above it
            // (must be returned). Stamp generation alternates per entry so the merge's cross-process
            // ordering (generation dominates monotonicNanos) is exercised on real scanned entries.
            val below: List[LaneKey] = List(
              LaneKey.Block(BlockNumber(0)),
              LaneKey.Block(BlockNumber(2)),
              LaneKey.Stack(StackNumber(0)),
              LaneKey.Request(HeadPeerNumber(0), RequestNumber(4)),
              LaneKey.Request(HeadPeerNumber(200), RequestNumber(1)),
              LaneKey.SoftAck(HeadPeerNumber(0), SoftAckNumber(2)),
              LaneKey.SoftAck(HeadPeerNumber(200), SoftAckNumber(2))
            )
            val above: List[LaneKey] = List(
              LaneKey.Block(BlockNumber(3)),
              LaneKey.Block(BlockNumber(4)),
              LaneKey.Stack(StackNumber(2)),
              LaneKey.Stack(StackNumber(3)),
              LaneKey.Request(HeadPeerNumber(0), RequestNumber(5)),
              LaneKey.Request(HeadPeerNumber(1), RequestNumber(0)),
              LaneKey.Request(HeadPeerNumber(200), RequestNumber(2)),
              LaneKey.SoftAck(HeadPeerNumber(0), SoftAckNumber(3)),
              LaneKey.SoftAck(HeadPeerNumber(1), SoftAckNumber(3)),
              LaneKey.SoftAck(HeadPeerNumber(200), SoftAckNumber(4)),
              LaneKey.HardAck(HeadPeerNumber(0), HardAckNumber(0)),
              LaneKey.HardAck(HeadPeerNumber(1), HardAckNumber(1)),
              LaneKey.HardAck(HeadPeerNumber(200), HardAckNumber(0))
            )
            // Stamp gen alternates 1/2 across the combined seed list while monotonic increases, so
            // some gen-1 entries get a higher monotonic than some gen-2 ones — a mono-only sort
            // would mis-order, pinning that generation is the high-order key.
            val seed = (below ++ above).zipWithIndex.map { case (k, i) =>
                k -> ArrivalStamp((i % 2) + 1, i.toLong)
            }
            for
                _ <- seed.traverse { case (k, stamp) =>
                    backend.put(k.cf, k.encode, LaneValue.frame(stamp, s"v-$k".getBytes("UTF-8")))
                }
                perLane <- LaneScan.scanLanes(backend, cursors)
                merged = ArrivalOrderedMerge.merge(perLane)
            yield
                val gotKeys = merged.map(_.key)
                // (a) exactly the above-floor keys: no below-floor leak, no cross-peer bleed.
                assert(gotKeys.toSet == above.toSet, s"got ${gotKeys.toSet}")
                // (b)+(d) nothing dropped or duplicated: count matches both the expected set and the
                // sum of per-lane tail sizes.
                assert(gotKeys.size == above.size)
                assert(merged.size == perLane.map(_.size).sum)
                // (c) merged stream is exactly arrival-stamp ordered (generation, then monotonic).
                val stamps = merged.map(e => (e.stamp.generation, e.stamp.monotonicNanos))
                assert(stamps == stamps.sorted)
        }
    }

    private def withStore(prog: BackendStore[IO] => IO[Assertion]): Assertion =
        InMemoryBackendStore.open.use(prog).unsafeRunSync()
