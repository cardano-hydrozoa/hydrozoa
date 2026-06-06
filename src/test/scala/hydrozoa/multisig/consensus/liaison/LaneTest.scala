package hydrozoa.multisig.consensus.liaison

import cats.effect.unsafe.implicits.global
import hydrozoa.multisig.consensus.liaison.Lane.*
import org.scalatest.funsuite.AnyFunSuite

/** Unit tests for the reusable [[Lane]] next-expected protocol unit. Items are plain `Int`s that
  * are their own lane number (`extract = identity`), so the sequencing logic is exercised in
  * isolation.
  */
class LaneTest extends AnyFunSuite {

    private def contiguousFrom(first: Int, maxPerReply: Int = 1): Lane[Int, Int] =
        Lane.contiguous[Int, Int](extract = identity, first = first, incr = _ + 1, maxPerReply)

    // Sparse schedule: the lane's zero (0) is the bootstrap sentinel, never led. This side leads the
    // even numbers >= 2, the remote the odd ones >= 1.
    private def sparseEvenOdd: Lane[Int, Int] =
        Lane.sparse[Int, Int](
          extract = identity,
          zero = 0,
          ownNext = after => Some(if after % 2 == 0 then after + 2 else after + 1),
          remoteNext = after => Some(if after % 2 == 1 then after + 2 else after + 1)
        )

    test("contiguous append enforces gap-free order from the first number") {
        val lane = contiguousFrom(0)
        lane.append(0).unsafeRunSync()
        lane.append(1).unsafeRunSync()
        lane.append(2).unsafeRunSync()
        assert(intercept[Lane.AppendOutOfOrder](lane.append(4).unsafeRunSync()).attempted == "4")
    }

    test("contiguous append rejects a wrong first number") {
        val lane = contiguousFrom(1)
        assertThrows[Lane.AppendOutOfOrder](lane.append(0).unsafeRunSync())
        lane.append(1).unsafeRunSync() // correct first
    }

    test("reply prunes below the remote cursor and keeps the head (retransmit-safe)") {
        val lane = contiguousFrom(0)
        (0 to 3).foreach(n => lane.append(n).unsafeRunSync())
        assert(lane.reply(2).unsafeRunSync() == Items(List(2)))
        // Re-asking from the same cursor still returns the head — not dropped on send.
        assert(lane.reply(2).unsafeRunSync() == Items(List(2)))
        // Moving the cursor forward drops the old head.
        assert(lane.reply(3).unsafeRunSync() == Items(List(3)))
        assert(lane.reply(4).unsafeRunSync() == Items(Nil))
    }

    test("reply flags out-of-bounds when the remote cursor is ahead of producible") {
        val lane = contiguousFrom(0)
        lane.append(0).unsafeRunSync()
        lane.append(1).unsafeRunSync()
        // Highest appended is 1, so the remote may legitimately ask up to 2 (next-producible).
        assert(lane.reply(2).unsafeRunSync() == Items(Nil))
        assert(lane.reply(3).unsafeRunSync() == OutOfBounds)
    }

    test("request-style lane batches up to maxPerReply") {
        val lane = contiguousFrom(0, maxPerReply = 2)
        (0 to 4).foreach(n => lane.append(n).unsafeRunSync())
        assert(lane.reply(1).unsafeRunSync() == Items(List(1, 2)))
    }

    test("verify matches the cursor and reports the next; mismatch is reported") {
        val lane = contiguousFrom(0)
        assert(lane.cursor.unsafeRunSync() == 0)
        assert(lane.verify(List(0), 0) == Right(1))
        assert(lane.verify(Nil, 0) == Right(0)) // empty leaves the cursor
        assert(lane.verify(List(1), 0) == Left(Mismatch(0, 1)))
        // Consecutive multi-item slice advances to the last + 1.
        assert(lane.verify(List(0, 1, 2), 0) == Right(3))
        // Non-consecutive multi-item slice is rejected at the gap.
        assert(lane.verify(List(0, 2), 0) == Left(Mismatch(0, 2)))
    }

    test("advanceTo commits a verified cursor") {
        val lane = contiguousFrom(0)
        lane.verify(List(0), 0).foreach(lane.advanceTo(_).unsafeRunSync())
        assert(lane.cursor.unsafeRunSync() == 1)
    }

    test("sparse lane: own-led append, remote-led cursor schedule") {
        val lane = sparseEvenOdd
        // Initial inbound cursor is the remote's first led item (odd >= 1) => 1.
        assert(lane.cursor.unsafeRunSync() == 1)
        // This side leads evens >= 2: 2, 4 ... (0 is the bootstrap sentinel, never appended).
        assertThrows[Lane.AppendOutOfOrder](lane.append(0).unsafeRunSync())
        lane.append(2).unsafeRunSync()
        lane.append(4).unsafeRunSync()
        assertThrows[Lane.AppendOutOfOrder](lane.append(5).unsafeRunSync()) // 5 is remote-led
        // Inbound: receiving the remote's 1 advances the cursor to its next led item, 3.
        assert(lane.verify(List(1), 1) == Right(3))
    }
}
