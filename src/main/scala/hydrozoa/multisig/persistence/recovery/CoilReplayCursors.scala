package hydrozoa.multisig.persistence.recovery

import hydrozoa.multisig.consensus.ack.{HardAckNumber, HubHardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.LaneKey

/** A coil peer's recovery scan / feed cursors — the coil counterpart of [[ReplayCursors]] (§5.3,
  * §10 Q10). The head primitives are rigidly `HeadPeerNumber`-typed and anchor the fast/slow sides
  * on `softAcked` / `hardAcked`; a coil peer differs in three ways, so it gets a parallel type
  * rather than a `PeerId`-widened one:
  *
  *   - **fast-side ledger floor** is `coilBlockMark + 1`, not `softAcked + 1` (a coil peer authors
  *     no soft-ack; its fast anchor is `max(BlockResult)`, §6 `JointLedger`);
  *   - **slow-side composer floor** is `coilHardAckedStack + 1`, sourced from the last own
  *     `CoilHardAck` value (not `HardAck`, §6 `StackComposer`);
  *   - it additionally replays the **`HubHardAck`** families (one per hub) — the coil quorum its
  *     `SlowConsensusActor` aggregates (§3.1, the `+ H` of `2 + 3N + H`) — and its **own**
  *     `CoilHardAck` family.
  *
  * The shared spines + head satellites carry the same dual-floor / single-floor structure as
  * [[ReplayCursors]] (the aggregator floors `softConfirmed + 1` / `hardConfirmed + 1` are shared —
  * a coil peer's `FastConsensusActor` aggregates head soft-acks and its `SlowConsensusActor`
  * aggregates head hard-acks just like a head peer's).
  */
final case class CoilReplayCursors(
    blockSpineForAggregator: LaneKey.Block,
    blockSpineForLedger: LaneKey.Block,
    stackSpineForAggregator: LaneKey.Stack,
    stackSpineForComposer: LaneKey.Stack,
    requests: Map[HeadPeerNumber, LaneKey.Request],
    softAcks: Map[HeadPeerNumber, LaneKey.SoftAck],
    hardAcks: Map[HeadPeerNumber, LaneKey.HardAck],
    hubHardAcks: Map[HeadPeerNumber, LaneKey.HubHardAck],
    ownCoilHardAck: LaneKey.CoilHardAck
):
    /** The lanes to scan, each from its lowest floor — the spines from their aggregator
      * (`*Confirmed + 1`) cursor, then the head satellites, the hub `HubHardAck` families, and the
      * coil peer's own `CoilHardAck` family. The composer's `acked` slicing happens at feed time in
      * [[ReplayActor]], not at scan time.
      */
    def scanFloors: List[LaneKey] =
        List[LaneKey](blockSpineForAggregator, stackSpineForAggregator) ++
            requests.values ++ softAcks.values ++ hardAcks.values ++ hubHardAcks.values ++
            List(ownCoilHardAck)

object CoilReplayCursors:

    /** Derive a coil peer's cursors. Pure — the caller ([[ReplayActor.replayCoil]]) supplies the
      * coil markers it reads itself: `softConfirmed` / `hardConfirmed` (shared CF scans),
      * `coilBlockMark` = `max(BlockResult)` (the fast anchor), `coilHardAckedStack` (unpacked from
      * the last own `CoilHardAck` value), and the per-head-peer request high-water. `peers` is
      * every head peer; `hubs` every hub head peer; `own` this coil peer.
      */
    def deriveCoil(
        softConfirmed: Option[BlockNumber],
        hardConfirmed: Option[StackNumber],
        coilBlockMark: Option[BlockNumber],
        coilHardAckedStack: Option[StackNumber],
        peers: List[HeadPeerNumber],
        hubs: List[HeadPeerNumber],
        own: CoilPeerNumber,
        highestIncludedRequest: Map[HeadPeerNumber, RequestNumber]
    ): CoilReplayCursors =
        val softConfirmedFloor: BlockNumber =
            softConfirmed.map(_.increment).getOrElse(BlockNumber(0))
        val coilBlockFloor: BlockNumber =
            coilBlockMark.map(_.increment).getOrElse(BlockNumber(0))
        val hardConfirmedFloor: StackNumber =
            hardConfirmed.map(_.increment).getOrElse(StackNumber(0))
        val coilComposerFloor: StackNumber =
            coilHardAckedStack.map(_.increment).getOrElse(StackNumber(0))
        // The soft-ack index coincides with the block number (§3.1), so the fast-side confirmed mark
        // is the soft-ack floor too — a coil peer's FastConsensusActor still aggregates head acks.
        val softAckFloor: SoftAckNumber =
            softConfirmed.map(b => SoftAckNumber((b: Int) + 1)).getOrElse(SoftAckNumber(0))

        CoilReplayCursors(
          blockSpineForAggregator = LaneKey.Block(softConfirmedFloor),
          blockSpineForLedger = LaneKey.Block(coilBlockFloor),
          stackSpineForAggregator = LaneKey.Stack(hardConfirmedFloor),
          stackSpineForComposer = LaneKey.Stack(coilComposerFloor),
          requests = peers.map { p =>
              val floor = highestIncludedRequest.get(p).map(_.increment).getOrElse(RequestNumber(0))
              p -> LaneKey.Request(p, floor)
          }.toMap,
          softAcks = peers.map(p => p -> LaneKey.SoftAck(p, softAckFloor)).toMap,
          hardAcks = peers.map(p => p -> LaneKey.HardAck(p, HardAckNumber(0))).toMap,
          hubHardAcks = hubs.map(h => h -> LaneKey.HubHardAck(h, HubHardAckNumber.zero)).toMap,
          ownCoilHardAck = LaneKey.CoilHardAck(own, HardAckNumber.zero)
        )
