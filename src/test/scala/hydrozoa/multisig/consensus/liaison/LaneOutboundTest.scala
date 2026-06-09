package hydrozoa.multisig.consensus.liaison

import cats.effect.unsafe.implicits.global
import hydrozoa.multisig.consensus.liaison.LaneOutbound.*
import org.scalatest.funsuite.AnyFunSuite

/** Unit tests for the outbound half ([[LaneOutbound]]) — append + reply. Items are plain `Int`s
  * that are their own lane number (`extract = identity`), so the sequencing logic is exercised in
  * isolation.
  */
class LaneOutboundTest extends AnyFunSuite {

    private def contiguousFrom(first: Int, maxPerReply: Int = 1): LaneOutbound[Int, Int] =
        LaneOutbound.contiguous[Int, Int](
          extract = identity,
          first = first,
          incr = _ + 1,
          maxPerReply
        )

    // Sparse own-led schedule: the lane's zero (0) is the bootstrap sentinel, never led. This side
    // leads the even numbers >= 2.
    private def sparseOwn: LaneOutbound[Int, Int] =
        LaneOutbound.sparse[Int, Int](
          extract = identity,
          zero = 0,
          ownNext = after => Some(if after % 2 == 0 then after + 2 else after + 1)
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
        assertThrows[AppendOutOfOrder](lane.append(0).unsafeRunSync())
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

    test("sparse outbound: own-led append, sentinel and remote-led numbers rejected") {
        val lane = sparseOwn
        // This side leads evens >= 2: 2, 4 ... (0 is the bootstrap sentinel, never appended).
        assertThrows[AppendOutOfOrder](lane.append(0).unsafeRunSync())
        lane.append(2).unsafeRunSync()
        lane.append(4).unsafeRunSync()
        assertThrows[AppendOutOfOrder](lane.append(5).unsafeRunSync()) // 5 is remote-led
    }
}
