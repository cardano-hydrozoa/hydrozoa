package hydrozoa.multisig.persistence

/** The set of column families the persistence layer opens.
  *
  * Twelve CFs in total, grouped as:
  *
  *   - **Lane CFs** (5) — one per lane type, with the CF acting as the lane-type discriminant
  *     (replaces a tag byte in the encoded key; see [[LaneKey]]). Lane values carry the 8-byte
  *     arrival-stamp prefix (§5.5).
  *   - **Aggregator outputs + JL working data** (3) — `BlockResult` (JL per-block output, keyed by
  *     `blockNum`, written at ack time so `StackComposer` can rebuild `pending` from disk on
  *     restart); `SoftConfirmation` (`FastConsensusActor` aggregate, keyed by `blockNum`);
  *     `HardConfirmation` (`SlowConsensusActor` multisigned-effects record, keyed by `stackNum`).
  *     Crucially, `softConfirmed` / `hardConfirmed` are **derived** as `max(SoftConfirmation.key)`
  *     / `max(HardConfirmation.key)` — no marker CF.
  *   - **Per-side passive snapshots** (3) — `DepositMap` (JL, per soft-ack); `Treasury` and
  *     `EvacuationMap` (StackComposer, per hard-ack stack-close). Each holds one keyed blob.
  *   - **Store metadata** (1) — `Meta` (schema version).
  *
  * See `design/persistence-and-crash-recovery.md` §7 / §7.1.
  */
enum Cf:
    // Lane CFs — single-writer-per-entry (rotating for spines, per-peer for satellites).
    case Block, Stack, Request, SoftAck, HardAck
    // Aggregator outputs + JL working data — keyed by spine index (blockNum / stackNum).
    case BlockResult, SoftConfirmation, HardConfirmation
    // Per-side passive snapshots — one keyed blob each.
    case DepositMap, Treasury, EvacuationMap
    // Store-level metadata.
    case Meta

object Cf:
    /** All CFs the store opens, in stable order (also the order written into the RocksDB
      * descriptors list).
      */
    val all: List[Cf] = values.toList
