package hydrozoa.multisig.consensus.liaison

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.multisig.consensus.liaison.LaneOutbound.*
import org.scalatest.funsuite.AnyFunSuite

/** Unit tests for the outbound half ([[LaneOutbound]]) — append + reply. Items are plain `Int`s
  * that are their own item number (`numberOf = identity`), so the sequencing logic is exercised in
  * isolation.
  */
class LaneOutboundTest extends AnyFunSuite {

    // A lane with an empty durable backing: a reply below the in-memory floor finds nothing.
    private val emptyBackfill: (Int, Int) => IO[List[Int]] = (_, _) => IO.pure(Nil)

    private def contiguousFrom(first: Int, maxPerReply: Int = 1): LaneOutbound[Int, Int] =
        LaneOutbound.contiguous[Int, Int](
          numberOf = identity,
          first = first,
          increment = _ + 1,
          maxPerReply,
          backfill = emptyBackfill
        )

    // Sparse own-led schedule: the lane's zero (0) is the bootstrap sentinel, never led. This side
    // leads the even numbers >= 2.
    private def sparseOwn: LaneOutbound[Int, Int] =
        LaneOutbound.sparse[Int, Int](
          numberOf = identity,
          zero = 0,
          next = after => Some(if after % 2 == 0 then after + 2 else after + 1),
          backfill = emptyBackfill
        )

    test("contiguous append enforces gap-free order from the first number") {
        val lane = contiguousFrom(0)
        lane.append(0).unsafeRunSync()
        lane.append(1).unsafeRunSync()
        lane.append(2).unsafeRunSync()
        assert(intercept[AppendOutOfOrder](lane.append(4).unsafeRunSync()).attempted == "4")
    }

    test("contiguous append rejects a wrong first number") {
        val lane = contiguousFrom(1)
        val _ = assertThrows[AppendOutOfOrder](lane.append(0).unsafeRunSync())
        lane.append(1).unsafeRunSync() // correct first
    }

    test("reply prunes below the remote cursor and keeps the head (retransmit-safe)") {
        val lane = contiguousFrom(0)
        (0 to 3).foreach(n => lane.append(n).unsafeRunSync())
        val _ = assert(lane.reply(2).unsafeRunSync() == Items(List(2)))
        // Re-asking from the same cursor still returns the head — not dropped on send.
        val _ = assert(lane.reply(2).unsafeRunSync() == Items(List(2)))
        // Moving the cursor forward drops the old head.
        val _ = assert(lane.reply(3).unsafeRunSync() == Items(List(3)))
        assert(lane.reply(4).unsafeRunSync() == Items(Nil))
    }

    test("reply flags out-of-bounds when the remote cursor is ahead of producible") {
        val lane = contiguousFrom(0)
        lane.append(0).unsafeRunSync()
        lane.append(1).unsafeRunSync()
        // Highest appended is 1, so the remote may legitimately ask up to 2 (next-producible).
        val _ = assert(lane.reply(2).unsafeRunSync() == Items(Nil))
        // Asking past the bound is out of bounds, carrying the diagnostic indices.
        assert(
          lane.reply(3).unsafeRunSync() == OutOfBounds(asked = "3", bound = "2", lastAppended = "1")
        )
    }

    test("request-style lane batches up to maxPerReply") {
        val lane = contiguousFrom(0, maxPerReply = 2)
        (0 to 4).foreach(n => lane.append(n).unsafeRunSync())
        assert(lane.reply(1).unsafeRunSync() == Items(List(1, 2)))
    }

    test("reply hot-loads from the store when the cursor is below the in-memory outbox floor") {
        // A stub store holding 0..4; on recovery the lane restores only its high-water (empty
        // outbox), so a pull below the in-memory floor is served from the store via `backfill`.
        val store = (0 to 4).toList
        val lane = LaneOutbound.contiguous[Int, Int](
          numberOf = identity,
          first = 0,
          increment = _ + 1,
          maxPerReply = 2,
          backfill = (from, limit) => IO.pure(store.filter(_ >= from).take(limit))
        )
        lane.seedHighWater(Some(4)).unsafeRunSync()
        // Outbox empty → served from the store.
        val _ = assert(lane.reply(1).unsafeRunSync() == Items(List(1, 2)))
        // A live tail item lands in memory; its own number is served from memory, older from store.
        lane.append(5).unsafeRunSync()
        val _ = assert(lane.reply(5).unsafeRunSync() == Items(List(5)))
        assert(lane.reply(2).unsafeRunSync() == Items(List(2, 3)))
    }

    test("append at or below the high-water is a no-op (absorbs a replay re-broadcast)") {
        // The store holds 0..3; on recovery the lane restores high-water = 3 (e.g. round-2), queue
        // empty. SlowConsensusActor then re-broadcasts the in-flight stack's round-1 (= 2, below the
        // high-water) — this must be a no-op, not an out-of-order error.
        val store = (0 to 3).toList
        val lane = LaneOutbound.contiguous[Int, Int](
          numberOf = identity,
          first = 0,
          increment = _ + 1,
          maxPerReply = 1,
          backfill = (from, limit) => IO.pure(store.filter(_ >= from).take(limit))
        )
        lane.seedHighWater(Some(3)).unsafeRunSync()
        lane.append(2).unsafeRunSync() // re-broadcast of an already-durable ack: no throw
        // It is still servable — hot-loaded from the store, since the queue stayed empty.
        val _ = assert(lane.reply(2).unsafeRunSync() == Items(List(2)))
        // A genuinely new item (high-water + 1) still appends; a gap above it still raises.
        lane.append(4).unsafeRunSync()
        val _ = assert(lane.reply(4).unsafeRunSync() == Items(List(4)))
        assert(intercept[AppendOutOfOrder](lane.append(6).unsafeRunSync()).attempted == "6")
    }

    test("seedHighWater sets the append baseline without populating the outbox") {
        val lane = contiguousFrom(0)
        lane.seedHighWater(Some(3)).unsafeRunSync()
        // The next *new* item must continue from the high-water (4), not the cold `first`; a gap
        // above the high-water still raises (the baseline is 3, not None).
        val _ = assertThrows[AppendOutOfOrder](lane.append(5).unsafeRunSync())
        lane.append(4).unsafeRunSync()
        // Empty backing → the restored prefix is not in memory and the store has nothing to
        // backfill; a pull below the tail is empty.
        assert(lane.reply(2).unsafeRunSync() == Items(Nil))
    }

    test("sparse outbound: own-led append, sentinel and remote-led numbers rejected") {
        val lane = sparseOwn
        // This side leads evens >= 2: 2, 4 ... (0 is the bootstrap sentinel, never appended).
        val _ = assertThrows[AppendOutOfOrder](lane.append(0).unsafeRunSync())
        lane.append(2).unsafeRunSync()
        lane.append(4).unsafeRunSync()
        assertThrows[AppendOutOfOrder](lane.append(5).unsafeRunSync()) // 5 is remote-led
    }

    test("reply backfill never serves a store entry above the released high-water") {
        // The store durably holds 0..3, but only 0..1 are released (appended) — e.g. #2 is a
        // postponed own soft-ack the producer persisted at block time but has not yet announced
        // onto the lane. seedHighWater(1) models that: high-water 1, empty outbox, so every serve
        // is a backfill from the store.
        val store = (0 to 3).toList
        val lane = LaneOutbound.contiguous[Int, Int](
          numberOf = identity,
          first = 0,
          increment = _ + 1,
          maxPerReply = 2,
          backfill = (from, limit) => IO.pure(store.filter(_ >= from).take(limit))
        )
        lane.seedHighWater(Some(1)).unsafeRunSync()
        // The released prefix is served from the store, capped at the high-water (no #2/#3 leak)...
        val _ = assert(lane.reply(0).unsafeRunSync() == Items(List(0, 1)))
        // ...and a pull for the not-yet-released #2 comes back empty, not hot-loaded — otherwise the
        // remote's cursor would run past our bound and the next pull would be a false OutOfBounds.
        val _ = assert(lane.reply(2).unsafeRunSync() == Items(Nil))
        // Once #2 is released (announced), it serves; #3 (still unreleased) stays withheld.
        lane.append(2).unsafeRunSync()
        assert(lane.reply(2).unsafeRunSync() == Items(List(2)))
    }
}
