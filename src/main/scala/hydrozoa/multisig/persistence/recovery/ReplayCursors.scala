package hydrozoa.multisig.persistence.recovery

import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestId.*
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{LaneKey, Markers}

/** The recovery scan / feed cursors — `4 + 3N` (§5.3): the two spines carry **two cursors each**
  * (one per consumer role), and there are three **single-cursor satellites** per head peer.
  *
  * **Why two cursors per spine.** Each spine (BlockSpine, StackSpine) is read by **two** roles at
  * **two** different floors:
  *
  *   - the **aggregator** (FastConsensusActor for blocks, SlowConsensusActor for stacks) resumes at
  *     the `*Confirmed` mark `+ 1`, to rebuild the in-flight confirmation cells; and
  *   - the **re-deriving ledger** (BlockWeaver → JointLedger for blocks, StackComposer for stacks)
  *     resumes at the `*Acked` mark `+ 1`, to re-produce / re-close entries it has not yet signed.
  *
  * Carrying both floors here is what lets the `ReplayActor` (R3) own the confirmed-vs-acked resume
  * rule in **one place** — it scans each spine once from the lower (aggregator) floor and feeds the
  * ledger consumer only the suffix `≥` its acked floor. The "don't re-sign `≤ *Acked`" rule (CR2)
  * is then **structural**: a re-deriving ledger is never handed a spine entry below its acked
  * floor, so **no consensus actor needs CR2 re-filtering logic of its own**. (`confirmed ≤ acked`
  * always — we confirm only what we have acked.) The satellites each have exactly one consumer, so
  * one cursor each.
  *
  * Per-lane floors (from [[Markers]] + the acked stack + the request high-water; §5.3):
  *
  *   - **BlockSpine** — `blockSpineForAggregator = softConfirmed + 1` (FastConsensusActor),
  *     `blockSpineForLedger = softAcked + 1` (BlockWeaver → JointLedger). `softAcked` is a
  *     `SoftAckNumber`, which coincides with the block number (one ack per block, §3.1), so it
  *     converts straight to a `BlockNumber`.
  *   - **StackSpine** — `stackSpineForAggregator = hardConfirmed + 1` (SlowConsensusActor),
  *     `stackSpineForComposer = hardAckedStack + 1` (StackComposer). The acked **stack** is *not*
  *     in [[Markers]] (`Markers.hardAcked` is the author's `HardAckNumber` counter, not a
  *     `StackNumber`); the caller supplies it, sourced by unpacking the stack id from the last own
  *     `HardAck` entry's value — the same slow-side hard-ack-lane indexing gap as the HardAck
  *     satellite floor below (§10 Q9).
  *   - **Request[p]** — high-water `+ 1` (BlockWeaver), from the persisted per-peer counter
  *     (maintained with [[maxRequestNumberPerPeer]]; not brief-folded — see that method). Single
  *     consumer.
  *   - **SoftAck[p]** — `softConfirmed + 1` for every peer (FastConsensusActor aggregates them).
  *     The soft-ack index coincides with the block number (§3.1), so the fast-side confirmed mark
  *     is the floor.
  *   - **HardAck[p]** — `0` for every peer (SlowConsensusActor aggregates them). ⚠ no derivable
  *     floor: the hard-ack lane is `HardAckNumber`-indexed, not `StackNumber`, and no marker gives
  *     the `StackNumber → HardAckNumber` correspondence. Scanning from 0 is correct, not minimal —
  *     defer a tight floor to R3 (§10 Q9 / §6 StackComposer).
  *
  * See `design/persistence-and-crash-recovery.md` §5.3 and `design/recovery-implementation-plan.md`
  * R1.
  */
final case class ReplayCursors(
    blockSpineForAggregator: LaneKey.Block,
    blockSpineForLedger: LaneKey.Block,
    stackSpineForAggregator: LaneKey.Stack,
    stackSpineForComposer: LaneKey.Stack,
    requests: Map[HeadPeerNumber, LaneKey.Request],
    softAcks: Map[HeadPeerNumber, LaneKey.SoftAck],
    hardAcks: Map[HeadPeerNumber, LaneKey.HardAck]
):
    /** The lanes to **scan**, each from its lowest floor (the widest tail any consumer needs): the
      * two spines from their aggregator (`*Confirmed + 1`) cursor — the lower of each spine's two,
      * since `confirmed ≤ acked` — then the satellites. Exactly `2 + 3N` entries (one scan per
      * lane). The ledger consumer's `acked` slicing happens at feed time in the `ReplayActor` (R3),
      * not at scan time. Spine order is fixed; satellite order is unspecified (hashed by
      * `HeadPeerNumber`) and washed out by [[ArrivalOrderedMerge]]'s stamp re-sort — do not rely on
      * it.
      */
    def scanFloors: List[LaneKey] =
        List[LaneKey](blockSpineForAggregator, stackSpineForAggregator) ++
            requests.values ++ softAcks.values ++ hardAcks.values

object ReplayCursors:

    /** Derive all `4 + 3N` cursors. Pure — the caller (R3) supplies the three values it must read
      * itself: the recovery [[Markers]] (CF scans), the per-peer request high-water (the persisted
      * counter), and `hardAckedStack` (the acked stack, unpacked from the last own `HardAck` value;
      * see the class docstring).
      */
    def derive(
        markers: Markers,
        peers: List[HeadPeerNumber],
        highestIncludedRequest: Map[HeadPeerNumber, RequestNumber],
        hardAckedStack: Option[StackNumber]
    ): ReplayCursors =
        val softConfirmedFloor: BlockNumber =
            markers.softConfirmed.map(_.increment).getOrElse(BlockNumber(0))
        // softAcked is a SoftAckNumber == blockNum (§3.1), so it converts straight to a BlockNumber.
        val softAckedFloor: BlockNumber =
            markers.softAcked.map(a => BlockNumber((a: Int) + 1)).getOrElse(BlockNumber(0))
        val hardConfirmedFloor: StackNumber =
            markers.hardConfirmed.map(_.increment).getOrElse(StackNumber(0))
        val hardAckedFloor: StackNumber =
            hardAckedStack.map(_.increment).getOrElse(StackNumber(0))
        // Soft-ack index coincides with the block number (§3.1: one ack per block, SoftAckNumber ==
        // blockNum), so the fast-side confirmed mark + 1 is the soft-ack floor too.
        val softAckFloor: SoftAckNumber =
            markers.softConfirmed.map(b => SoftAckNumber((b: Int) + 1)).getOrElse(SoftAckNumber(0))

        ReplayCursors(
          blockSpineForAggregator = LaneKey.Block(softConfirmedFloor),
          blockSpineForLedger = LaneKey.Block(softAckedFloor),
          stackSpineForAggregator = LaneKey.Stack(hardConfirmedFloor),
          stackSpineForComposer = LaneKey.Stack(hardAckedFloor),
          requests = peers.map { p =>
              val floor = highestIncludedRequest.get(p).map(_.increment).getOrElse(RequestNumber(0))
              p -> LaneKey.Request(p, floor)
          }.toMap,
          softAcks = peers.map(p => p -> LaneKey.SoftAck(p, softAckFloor)).toMap,
          hardAcks = peers.map(p => p -> LaneKey.HardAck(p, HardAckNumber(0))).toMap
        )

    /** Fold a stream of `RequestId`s to the max `RequestNumber` per author. The kernel that
      * maintains the persisted per-peer request high-water counter: as each block is produced, fold
      * its included request ids in and keep the running max per author (the next RequestLane cursor
      * is this `+ 1`, since requests are monotonic per author — §5.3 consecutive-request rule).
      *
      * Counts **every** request id passed, valid or invalid. A `RequestId` is assigned at
      * sequencing time (before validation; CR1), so an invalid request still consumes its author's
      * monotonic request-number slot. Excluding it would set the cursor too low and re-feed an
      * already-included request, so the `ValidityFlag` is irrelevant here.
      */
    def maxRequestNumberPerPeer(
        requestIds: Iterable[RequestId]
    ): Map[HeadPeerNumber, RequestNumber] =
        mergeHighWater(Map.empty, requestIds)

    /** Fold `requestIds` into an existing high-water map, keeping the per-author max — the
      * write-side maintenance step JointLedger runs on each own soft-ack: read the prior persisted
      * blob, merge this block's included ids, rewrite. Monotone in `into`, so it never lowers a
      * counter (a peer whose ids are all `≤` its current high-water leaves the entry unchanged).
      */
    def mergeHighWater(
        into: Map[HeadPeerNumber, RequestNumber],
        requestIds: Iterable[RequestId]
    ): Map[HeadPeerNumber, RequestNumber] =
        requestIds.foldLeft(into) { (acc, rid) =>
            acc.updatedWith(rid.peerNum) {
                case Some(cur) if Ordering[RequestNumber].gteq(cur, rid.requestNum) => Some(cur)
                case _ => Some(rid.requestNum)
            }
        }
