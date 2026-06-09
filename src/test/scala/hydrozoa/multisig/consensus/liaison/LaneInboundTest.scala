package hydrozoa.multisig.consensus.liaison

import cats.effect.unsafe.implicits.global
import hydrozoa.multisig.consensus.liaison.LaneInbound.*
import org.scalatest.funsuite.AnyFunSuite

/** Unit tests for the inbound half ([[LaneInbound]]) — cursor + verify + advance. Items are plain
  * `Int`s that are their own item number (`numberOf = identity`).
  */
class LaneInboundTest extends AnyFunSuite {

    private def contiguousFrom(first: Int): LaneInbound[Int, Int] =
        LaneInbound.contiguous[Int, Int](numberOf = identity, first = first, incr = _ + 1)

    // Sparse remote-led schedule: zero (0) is the bootstrap sentinel; the remote leads the odd
    // numbers >= 1.
    private def sparseRemote: LaneInbound[Int, Int] =
        LaneInbound.sparse[Int, Int](
          numberOf = identity,
          zero = 0,
          remoteNext = after => Some(if after % 2 == 1 then after + 2 else after + 1)
        )

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

    test(
      "sparse inbound: initial cursor is the remote's first led item; successor follows schedule"
    ) {
        val lane = sparseRemote
        // Initial inbound cursor is the remote's first led item (odd >= 1) => 1.
        assert(lane.cursor.unsafeRunSync() == 1)
        // Receiving the remote's 1 advances the cursor to its next led item, 3.
        assert(lane.verify(List(1), 1) == Right(3))
    }
}
