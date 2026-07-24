package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.peer.PeerId.toWireInt
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}

/** A column family the persistence layer opens.
  *
  * **Satellite journals are split one CF per author** (§3.2, §7.1): the CF *is* the author
  * discriminant, so the on-disk key carries no author prefix and each author's CF is a single
  * monotonic append stream (non-overlapping L0 → near-zero compaction — decisive at the request
  * rate). The set of CFs is therefore **config-derived** (`Cf.mkAll`): the fixed-shape CFs (the two
  * spines, the spine-indexed working/confirmation CFs, the snapshots, `Meta`) plus one CF per
  * satellite author derived from the head's membership. Membership changes by closing and
  * re-opening a fresh head, so the set is constant for a store's lifetime.
  *
  * A `Cf` is used as a `Map` key (the backend's handle map), so the cases rely on case object /
  * case class structural equality.
  *
  * See `design/persistence-and-crash-recovery.md` §7 / §7.1.
  */
sealed trait Cf:
    /** Stable on-disk column-family name (UTF-8). Per-author satellite names embed the author. */
    def name: String

object Cf:
    // ---- Fixed CFs — one each. ------------------------------------------------------------------
    /** The single block spine (round-robin-authored briefs, keyed by `blockNum`). */
    case object Block extends Cf:
        def name = "Block"

    /** The single stack spine (round-robin-authored briefs, keyed by `stackNum`). */
    case object Stack extends Cf:
        def name = "Stack"

    /** JL per-block output, keyed by `blockNum`. */
    case object BlockResult extends Cf:
        def name = "BlockResult"

    /** FCA aggregate (header + multisig), keyed by `blockNum`; `softConfirmed = max(key)`. */
    case object SoftConfirmation extends Cf:
        def name = "SoftConfirmation"

    /** SCA multisigned effects / SECs / fallbacks, keyed by `stackNum`; `hardConfirmed = max(key)`.
      * The R10 evacuation floor.
      */
    case object HardConfirmation extends Cf:
        def name = "HardConfirmation"

    /** JL per-peer request high-water: the highest request from each peer **included in any block
      * up to** the key `blockNum` (cumulative, monotone in `blockNum`).
      */
    case object RequestHighWater extends Cf:
        def name = "RequestHighWater"

    /** Singleton CF (hub-only): per-coil-peer stamped-high-water marks for `CoilAckSequencer` — the
      * highest coil `HardAckNumber` this hub has sequenced onto its `HubHardAck` spine, per coil
      * peer. Written in the same atomic batch as each `HubHardAck`, so recovery knows which durable
      * inbound coil hard-acks (the coil's `HardAck` receive copy) still need stamping (§6
      * `CoilAckSequencer`).
      */
    case object CoilStampMark extends Cf:
        def name = "CoilStampMark"

    /** JL L2 commit counter, keyed by `blockNum`. */
    case object L2CommandNumber extends Cf:
        def name = "L2CommandNumber"

    /** SC's closed `Stack.Unsigned` (brief + effects), keyed by `stackNum`. */
    case object UnsignedStack extends Cf:
        def name = "UnsignedStack"

    /** JL fast-side deposits snapshot — single keyed blob. */
    case object DepositMap extends Cf:
        def name = "DepositMap"

    /** SC slow-side treasury snapshot — single keyed blob. */
    case object Treasury extends Cf:
        def name = "Treasury"

    /** SC cumulative evacuation map, keyed per committed `blockNum`. */
    case object EvacuationMap extends Cf:
        def name = "EvacuationMap"

    /** Request → block reverse index, keyed by the opaque request id (its packed i64) → the block
      * that locally processed the request plus its validity verdict. Written by JL in the same
      * atomic bundle as the block.
      */
    case object RequestBlockIndex extends Cf:
        def name = "RequestBlockIndex"

    /** Block → stack reverse index: `blockNum` → the stack that hard-confirmed the block. Written
      * by SCA in the same atomic batch as the stack's `HardConfirmation`.
      */
    case object BlockStackIndex extends Cf:
        def name = "BlockStackIndex"

    /** Effect → stack reverse index: `l1TxId` → the stack whose `HardConfirmation` carries that
      * effect. Written by SCA in the same atomic batch as the stack's `HardConfirmation`. Backs
      * `GET /head/effects/<l1TxId>`.
      */
    case object EffectStackIndex extends Cf:
        def name = "EffectStackIndex"

    /** Deposit-request → absorbing-block reverse index: the deposit request's opaque id (packed
      * i64) → the major `blockNum` that absorbed the deposit into the treasury. Written by JL in
      * the same atomic bundle as that block.
      */
    case object DepositAbsorptionIndex extends Cf:
        def name = "DepositAbsorptionIndex"

    /** Store-level metadata (schema version + arrival-stamp generation). */
    case object Meta extends Cf:
        def name = "Meta"

    // ---- Per-author satellite CFs (one per author). --------------------------------------------
    /** One head peer's request journal, keyed by `requestNum`. */
    final case class Request(peer: HeadPeerNumber) extends Cf:
        def name = s"Request:${peer: Int}"

    /** One head peer's soft-ack journal, keyed by `softAckNum`. */
    final case class SoftAck(peer: HeadPeerNumber) extends Cf:
        def name = s"SoftAck:${peer: Int}"

    /** One peer's hard-ack journal (head peer own; coil peer own + a hub's receive copy), keyed by
      * `hardAckNum`. The author is a [[PeerId]] — head and coil peers share one journal type, one
      * CF per author. The CF name embeds the author's wire int (num shifted left one bit, low bit
      * tagging head vs coil — see [[PeerId.toWireInt]]), so a head peer's and a coil peer's
      * journals never collide on the same number.
      */
    final case class HardAck(peer: PeerId) extends Cf:
        def name = s"HardAck:${peer.toWireInt}"

    /** One hub's re-sequenced coil-ack journal, keyed by `hubHardAckNum`. */
    final case class HubHardAck(hub: HeadPeerNumber) extends Cf:
        def name = s"HubHardAck:${hub: Int}"

    /** The fixed-shape CFs, present on every store regardless of membership. */
    val fixed: List[Cf] = List(
      Block,
      Stack,
      BlockResult,
      SoftConfirmation,
      HardConfirmation,
      RequestHighWater,
      CoilStampMark,
      L2CommandNumber,
      UnsignedStack,
      DepositMap,
      Treasury,
      EvacuationMap,
      RequestBlockIndex,
      DepositAbsorptionIndex,
      BlockStackIndex,
      EffectStackIndex,
      Meta
    )

    /** The full CF set for a head of the given membership: the fixed-shape CFs plus one per-author
      * satellite CF for each head peer (Request / SoftAck / HardAck), each peer's hard-ack journal
      * (`HardAck` per head peer and per coil peer — the author is a [[PeerId]]), and each hub
      * (HubHardAck). This is the descriptor list the backend opens (§7).
      */
    def mkAll(
        headPeers: List[HeadPeerNumber],
        coilPeers: List[CoilPeerNumber],
        hubs: List[HeadPeerNumber]
    ): List[Cf] =
        fixed
            ++ headPeers.map(Request(_))
            ++ headPeers.map(SoftAck(_))
            ++ headPeers.map(p => HardAck(PeerId.Head(p)))
            ++ coilPeers.map(c => HardAck(PeerId.Coil(c)))
            ++ hubs.map(HubHardAck(_))
