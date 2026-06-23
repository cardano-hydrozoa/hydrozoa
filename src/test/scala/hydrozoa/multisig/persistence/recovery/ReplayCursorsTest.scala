package hydrozoa.multisig.persistence.recovery

import hydrozoa.multisig.consensus.ack.{HardAckNumber, HubHardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestId.*
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{JournalKey, Markers}
import org.scalatest.funsuite.AnyFunSuite

/** Unit tests for the §5.3 indices algorithm in [[ReplayCursors]] — pure derivation of the `2 + 3N`
  * journal scan floors from the recovery [[Markers]].
  */
class ReplayCursorsTest extends AnyFunSuite:

    private val peers = List(HeadPeerNumber(0), HeadPeerNumber(1), HeadPeerNumber(2))

    private val ownHead: PeerId = PeerId.Head(HeadPeerNumber(0))

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
        val _ = assert(hw(HeadPeerNumber(0)) == RequestNumber(7))
        val _ = assert(hw(HeadPeerNumber(1)) == RequestNumber(5))
        val _ = assert(hw(HeadPeerNumber(2)) == RequestNumber(0))
        assert(hw.size == 3)
    }

    test("maxRequestNumberPerPeer on no events is empty") {
        assert(ReplayCursors.maxRequestNumberPerPeer(Nil).isEmpty)
    }

    test("mergeHighWater folds new ids into a prior map and never lowers a counter") {
        val prior = Map(
          HeadPeerNumber(0) -> RequestNumber(7),
          HeadPeerNumber(1) -> RequestNumber(5)
        )
        // peer 0 sees only older ids (≤ 7 → unchanged); peer 1 advances; peer 2 is new.
        val merged = ReplayCursors.mergeHighWater(prior, List(rid(0, 2), rid(1, 9), rid(2, 0)))
        val _ = assert(merged(HeadPeerNumber(0)) == RequestNumber(7))
        val _ = assert(merged(HeadPeerNumber(1)) == RequestNumber(9))
        val _ = assert(merged(HeadPeerNumber(2)) == RequestNumber(0))
        assert(merged.size == 3)
    }

    test("mergeHighWater onto the empty map equals maxRequestNumberPerPeer") {
        val ids = List(rid(0, 3), rid(0, 7), rid(1, 2))
        assert(
          ReplayCursors.mergeHighWater(Map.empty, ids) == ReplayCursors.maxRequestNumberPerPeer(ids)
        )
    }

    test("derive: each spine carries both floors and each satellite its floor, fully populated") {
        val markers = Markers(
          softConfirmed = Some(BlockNumber(5)),
          fastBlockMark = Some(BlockNumber(6)),
          hardConfirmed = Some(StackNumber(3)),
          hardAcked =
              Some(HardAckNumber(4)), // the counter — derive ignores it; acked stack is a param
          nextRequestNumber = RequestNumber(0)
        )
        val hw = Map(HeadPeerNumber(0) -> RequestNumber(7), HeadPeerNumber(1) -> RequestNumber(2))
        val cursors =
            ReplayCursors.derive(
              markers,
              peers,
              Nil,
              hw,
              hardAckedStack = Some(StackNumber(4)),
              own = ownHead
            )

        // BlockSpine: aggregator floor = softConfirmed + 1; ledger floor = fastBlockMark + 1.
        val _ = assert(
          cursors.blockSpineForAggregator == JournalKey.Block(BlockNumber(6)),
          "softConfirmed 5 + 1"
        )
        val _ = assert(
          cursors.blockSpineForLedger == JournalKey.Block(BlockNumber(7)),
          "fastBlockMark 6 + 1"
        )
        // StackSpine: aggregator floor = hardConfirmed + 1; composer floor = hardAckedStack + 1.
        val _ = assert(
          cursors.stackSpineForAggregator == JournalKey.Stack(StackNumber(4)),
          "hardConfirmed 3 + 1"
        )
        val _ = assert(
          cursors.stackSpineForComposer == JournalKey.Stack(StackNumber(5)),
          "hardAckedStack 4 + 1"
        )
        // Soft-ack floor coincides with the fast-side confirmed mark + 1, for every peer.
        peers.foreach(p => assert(cursors.softAcks(p) == JournalKey.SoftAck(p, SoftAckNumber(6))))
        // Request floor = per-peer high-water + 1; peers with no included request start at 0.
        val _ = assert(
          cursors
              .requests(HeadPeerNumber(0)) == JournalKey.Request(
            HeadPeerNumber(0),
            RequestNumber(8)
          )
        )
        val _ = assert(
          cursors
              .requests(HeadPeerNumber(1)) == JournalKey.Request(
            HeadPeerNumber(1),
            RequestNumber(3)
          )
        )
        val _ = assert(
          cursors
              .requests(HeadPeerNumber(2)) == JournalKey.Request(
            HeadPeerNumber(2),
            RequestNumber(0)
          )
        )
        // Hard-ack journal has no derivable band floor (HardAckNumber-indexed) — scan from 0.
        peers.foreach(p =>
            assert(cursors.hardAcks(p) == JournalKey.HardAck(PeerId.Head(p), HardAckNumber(0)))
        )
    }

    test(
      "derive: confirmed None with acked work present — confirmed floor 0, acked floor at mark+1"
    ) {
        // confirmed (the scan floor) drops to 0 when nothing is confirmed; the acked floor still
        // tracks the acked marks, so the two spine floors genuinely differ here.
        val markers = Markers(
          softConfirmed = None,
          fastBlockMark = Some(BlockNumber(5)),
          hardConfirmed = None,
          hardAcked = Some(HardAckNumber(5)),
          nextRequestNumber = RequestNumber(0)
        )
        val cursors =
            ReplayCursors.derive(
              markers,
              peers,
              Nil,
              Map.empty,
              hardAckedStack = Some(StackNumber(2)),
              own = ownHead
            )
        val _ = assert(cursors.blockSpineForAggregator == JournalKey.Block(BlockNumber(0)))
        val _ = assert(
          cursors.blockSpineForLedger == JournalKey.Block(BlockNumber(6)),
          "fastBlockMark 5 + 1"
        )
        val _ = assert(cursors.stackSpineForAggregator == JournalKey.Stack(StackNumber(0)))
        val _ = assert(
          cursors.stackSpineForComposer == JournalKey.Stack(StackNumber(3)),
          "hardAckedStack 2 + 1"
        )
        peers.foreach(p => assert(cursors.softAcks(p) == JournalKey.SoftAck(p, SoftAckNumber(0))))
    }

    test("derive: empty store (all None, no acked stack) yields index-0 floors everywhere") {
        val markers = Markers(None, None, None, None, RequestNumber(0))
        val cursors = ReplayCursors.derive(
          markers,
          peers,
          Nil,
          Map.empty,
          hardAckedStack = None,
          own = ownHead
        )

        val _ = assert(cursors.blockSpineForAggregator == JournalKey.Block(BlockNumber(0)))
        val _ = assert(cursors.blockSpineForLedger == JournalKey.Block(BlockNumber(0)))
        val _ = assert(cursors.stackSpineForAggregator == JournalKey.Stack(StackNumber(0)))
        val _ = assert(cursors.stackSpineForComposer == JournalKey.Stack(StackNumber(0)))
        peers.foreach { p =>
            val _ = assert(cursors.requests(p) == JournalKey.Request(p, RequestNumber(0)))
            val _ = assert(cursors.softAcks(p) == JournalKey.SoftAck(p, SoftAckNumber(0)))
            assert(cursors.hardAcks(p) == JournalKey.HardAck(PeerId.Head(p), HardAckNumber(0)))
        }
    }

    test("scanFloors enumerates exactly 2 + 3N journals (no hubs; spines collapse to confirmed)") {
        val cursors =
            ReplayCursors.derive(
              Markers(None, None, None, None, RequestNumber(0)),
              peers,
              Nil,
              Map.empty,
              None,
              own = ownHead
            )
        val _ = assert(cursors.scanFloors.size == 2 + 3 * peers.size)
        // The spines in scanFloors are the lower (aggregator) floor, not the ledger one.
        val _ = assert(cursors.scanFloors.contains(cursors.blockSpineForAggregator))
        assert(cursors.scanFloors.contains(cursors.stackSpineForAggregator))
    }

    test("derive: hub HubHardAck cursors scan from 0; scanFloors grows to 2 + 3N + H") {
        val hubs = List(HeadPeerNumber(0), HeadPeerNumber(1))
        val cursors =
            ReplayCursors.derive(
              Markers(None, None, None, None, RequestNumber(0)),
              peers,
              hubs,
              Map.empty,
              None,
              own = ownHead
            )
        hubs.foreach(h =>
            assert(cursors.hubHardAcks(h) == JournalKey.HubHardAck(h, HubHardAckNumber.zero))
        )
        val _ = assert(cursors.hubHardAcks.size == hubs.size)
        val _ = assert(cursors.scanFloors.size == 2 + 3 * peers.size + hubs.size)
        hubs.foreach(h => assert(cursors.scanFloors.contains(cursors.hubHardAcks(h))))
        // A head peer carries no own coil HardAck floor.
        assert(cursors.ownCoilHardAck.isEmpty)
    }

    test("derive: a coil peer adds its own coil HardAck floor; scanFloors grows by one") {
        val hubs = List(HeadPeerNumber(1))
        val own = PeerId.Coil(CoilPeerNumber(0))
        val cursors =
            ReplayCursors.derive(
              Markers(None, None, None, None, RequestNumber(0)),
              peers,
              hubs,
              Map.empty,
              None,
              own
            )
        val _ = assert(
          cursors.ownCoilHardAck.contains(
            JournalKey.HardAck(PeerId.Coil(CoilPeerNumber(0)), HardAckNumber.zero)
          )
        )
        // 2 spines + 3N satellites + H hubs + 1 own coil HardAck.
        val _ = assert(cursors.scanFloors.size == 2 + 3 * peers.size + hubs.size + 1)
        assert(cursors.scanFloors.contains(cursors.ownCoilHardAck.get))
    }
