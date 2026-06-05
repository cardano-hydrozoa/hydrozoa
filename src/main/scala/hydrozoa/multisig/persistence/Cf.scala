package hydrozoa.multisig.persistence

/** The set of column families the persistence layer opens.
  *
  * Fifteen CFs in total, grouped as:
  *
  *   - **Lane CFs** (5) — one per lane type, with the CF acting as the lane-type discriminant
  *     (replaces a tag byte in the encoded key; see [[LaneKey]]). Lane values carry the 12-byte
  *     arrival-stamp prefix (§5.4).
  *   - **Aggregator outputs + JL working data** (5) — `BlockResult` (JL per-block output, keyed by
  *     `blockNum`, written at ack time so `StackComposer` can rebuild `pending` from disk on
  *     restart); `SoftConfirmation` (`FastConsensusActor` aggregate, keyed by `blockNum`);
  *     `HardConfirmation` (`SlowConsensusActor` multisigned-effects record, keyed by `stackNum`);
  *     `RequestHighWater` (JL, keyed by `blockNum`) — the cumulative per-peer
  *     `Map[HeadPeerNumber, RequestNumber]` high-water as of each block, which the `ReplayActor`
  *     reads at `softAcked` to seed each peer's RequestLane resume cursor (§5.3); and
  *     `L2CommandNumber` (JL, keyed by `blockNum`) — the L2 ledger's commit counter reached after
  *     each block's L2 commits, which JointLedger's own recover reads at `softAcked` to co-anchor
  *     the committed L2 state via `restoreTo` (§R2b); and `UnsignedStack` (StackComposer, keyed by
  *     `stackNum`) — the `Stack.Unsigned` (brief + locally-derived effects) this peer closed,
  *     persisted **before** the handoff to `SlowConsensusActor` so SCA can re-form its in-flight
  *     cell on recovery (a `HardAck` signs the effects, which a `StackBrief` alone does not carry).
  *     Crucially, `softConfirmed` / `hardConfirmed` are **derived** as `max(SoftConfirmation.key)`
  *     / `max(HardConfirmation.key)` — no marker CF.
  *   - **Per-side passive snapshots** (3) — `DepositMap` (JL, per soft-ack); `Treasury` and
  *     `EvacuationMap` (StackComposer, per hard-ack stack-close). `DepositMap` / `Treasury` hold
  *     one keyed blob; `EvacuationMap` is keyed per committed `blockNum`.
  *   - **Store metadata** (1) — `Meta` (schema version).
  *
  * See `design/persistence-and-crash-recovery.md` §7 / §7.1.
  */
enum Cf:
    // Lane CFs — single-writer-per-entry (rotating for spines, per-peer for satellites).
    case Block, Stack, Request, SoftAck, HardAck
    // Aggregator outputs + JL working data — keyed by spine index (blockNum / stackNum).
    case BlockResult, SoftConfirmation, HardConfirmation, RequestHighWater, L2CommandNumber,
        UnsignedStack
    // Per-side passive snapshots — one keyed blob each (EvacuationMap keyed per committed block).
    case DepositMap, Treasury, EvacuationMap
    // Store-level metadata.
    case Meta

object Cf:
    /** All CFs the store opens, in stable order (also the order written into the RocksDB
      * descriptors list).
      */
    val all: List[Cf] = values.toList
