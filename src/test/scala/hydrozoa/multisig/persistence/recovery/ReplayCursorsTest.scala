package hydrozoa.multisig.persistence.recovery

import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestId.*
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{LaneKey, Markers}
import org.scalatest.funsuite.AnyFunSuite

/** Unit tests for the §5.3 indices algorithm in [[ReplayCursors]] — pure derivation of the `2 + 3N`
  * lane scan floors from the recovery [[Markers]].
  */
class ReplayCursorsTest extends AnyFunSuite:

    private val peers = List(HeadPeerNumber(0), HeadPeerNumber(1), HeadPeerNumber(2))

    private def rid(peer: Int, num: Long): RequestId =
        RequestId(HeadPeerNumber(peer), RequestNumber(num))

    test("maxRequestNumberPerPeer takes the max request number per author") {
        val events = List(
          rid(0, 3),
          rid(0, 1),
          rid(0, 7), // peer 0 high-water = 7
          rid(1, 2),
          rid(1, 5), // peer 1 high-water = 5
          rid(2, 0) // peer 2 high-water = 0
        )
        val hw = ReplayCursors.maxRequestNumberPerPeer(events)
        assert(hw(HeadPeerNumber(0)) == RequestNumber(7))
        assert(hw(HeadPeerNumber(1)) == RequestNumber(5))
        assert(hw(HeadPeerNumber(2)) == RequestNumber(0))
        assert(hw.size == 3)
    }

    test("maxRequestNumberPerPeer on no events is empty") {
        assert(ReplayCursors.maxRequestNumberPerPeer(Nil).isEmpty)
    }

    test("derive: each spine carries both floors and each satellite its floor, fully populated") {
        val markers = Markers(
          softConfirmed = Some(BlockNumber(5)),
          hardConfirmed = Some(StackNumber(3)),
          softAcked = Some(SoftAckNumber(6)),
          hardAcked =
              Some(HardAckNumber(4)) // the counter — derive ignores it; acked stack is a param
        )
        val hw = Map(HeadPeerNumber(0) -> RequestNumber(7), HeadPeerNumber(1) -> RequestNumber(2))
        val cursors =
            ReplayCursors.derive(markers, peers, hw, hardAckedStack = Some(StackNumber(4)))

        // BlockSpine: aggregator floor = softConfirmed + 1; ledger floor = softAcked + 1.
        assert(
          cursors.blockSpineForAggregator == LaneKey.Block(BlockNumber(6)),
          "softConfirmed 5 + 1"
        )
        assert(cursors.blockSpineForLedger == LaneKey.Block(BlockNumber(7)), "softAcked 6 + 1")
        // StackSpine: aggregator floor = hardConfirmed + 1; composer floor = hardAckedStack + 1.
        assert(
          cursors.stackSpineForAggregator == LaneKey.Stack(StackNumber(4)),
          "hardConfirmed 3 + 1"
        )
        assert(
          cursors.stackSpineForComposer == LaneKey.Stack(StackNumber(5)),
          "hardAckedStack 4 + 1"
        )
        // Soft-ack floor coincides with the fast-side confirmed mark + 1, for every peer.
        peers.foreach(p => assert(cursors.softAcks(p) == LaneKey.SoftAck(p, SoftAckNumber(6))))
        // Request floor = per-peer high-water + 1; peers with no included request start at 0.
        assert(
          cursors
              .requests(HeadPeerNumber(0)) == LaneKey.Request(HeadPeerNumber(0), RequestNumber(8))
        )
        assert(
          cursors
              .requests(HeadPeerNumber(1)) == LaneKey.Request(HeadPeerNumber(1), RequestNumber(3))
        )
        assert(
          cursors
              .requests(HeadPeerNumber(2)) == LaneKey.Request(HeadPeerNumber(2), RequestNumber(0))
        )
        // Hard-ack lane has no derivable band floor (HardAckNumber-indexed) — scan from 0.
        peers.foreach(p => assert(cursors.hardAcks(p) == LaneKey.HardAck(p, HardAckNumber(0))))
    }

    test(
      "derive: confirmed None with acked work present — confirmed floor 0, acked floor at mark+1"
    ) {
        // confirmed (the scan floor) drops to 0 when nothing is confirmed; the acked floor still
        // tracks the acked marks, so the two spine floors genuinely differ here.
        val markers = Markers(
          softConfirmed = None,
          hardConfirmed = None,
          softAcked = Some(SoftAckNumber(5)),
          hardAcked = Some(HardAckNumber(5))
        )
        val cursors =
            ReplayCursors.derive(markers, peers, Map.empty, hardAckedStack = Some(StackNumber(2)))
        assert(cursors.blockSpineForAggregator == LaneKey.Block(BlockNumber(0)))
        assert(cursors.blockSpineForLedger == LaneKey.Block(BlockNumber(6)), "softAcked 5 + 1")
        assert(cursors.stackSpineForAggregator == LaneKey.Stack(StackNumber(0)))
        assert(
          cursors.stackSpineForComposer == LaneKey.Stack(StackNumber(3)),
          "hardAckedStack 2 + 1"
        )
        peers.foreach(p => assert(cursors.softAcks(p) == LaneKey.SoftAck(p, SoftAckNumber(0))))
    }

    test("derive: empty store (all None, no acked stack) yields index-0 floors everywhere") {
        val markers = Markers(None, None, None, None)
        val cursors = ReplayCursors.derive(markers, peers, Map.empty, hardAckedStack = None)

        assert(cursors.blockSpineForAggregator == LaneKey.Block(BlockNumber(0)))
        assert(cursors.blockSpineForLedger == LaneKey.Block(BlockNumber(0)))
        assert(cursors.stackSpineForAggregator == LaneKey.Stack(StackNumber(0)))
        assert(cursors.stackSpineForComposer == LaneKey.Stack(StackNumber(0)))
        peers.foreach { p =>
            assert(cursors.requests(p) == LaneKey.Request(p, RequestNumber(0)))
            assert(cursors.softAcks(p) == LaneKey.SoftAck(p, SoftAckNumber(0)))
            assert(cursors.hardAcks(p) == LaneKey.HardAck(p, HardAckNumber(0)))
        }
    }

    test("scanFloors enumerates exactly 2 + 3N lanes (spines collapse to the confirmed floor)") {
        val cursors =
            ReplayCursors.derive(Markers(None, None, None, None), peers, Map.empty, None)
        assert(cursors.scanFloors.size == 2 + 3 * peers.size)
        // The spines in scanFloors are the lower (aggregator) floor, not the ledger one.
        assert(cursors.scanFloors.contains(cursors.blockSpineForAggregator))
        assert(cursors.scanFloors.contains(cursors.stackSpineForAggregator))
    }
