package hydrozoa.multisig.persistence

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{Block, BlockNumber, BlockResult as LedgerBlockResult}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.joint.EvacuationMap as JointEvacuationMap
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.utxo.MultisigTreasuryUtxo
import hydrozoa.multisig.ledger.l2.L2CommandNumber as LedgerL2CommandNumber
import hydrozoa.multisig.ledger.stack.{Stack, StackEffects, StackNumber}
import hydrozoa.multisig.persistence.codec.{BlockResultCodec, DepositMapCodec, RequestHighWaterCodec, SoftConfirmationCodec, StackEffectsCodec, TreasuryCodec, UnsignedStackCodec}

/** The typed key surface for the high-level persistence API.
  *
  * Every column family ([[Cf]]) has exactly one **key shape** and exactly one **value type** — a
  * corresponding `StoreKey` subtype captures both, so actor code addresses entries by name and
  * exchanges typed values, never raw bytes. The 5 family CFs are reached through [[FamilyKey]]
  * (which extends `StoreKey`); the 10 non-family CFs are covered by the cases declared in the
  * companion below:
  *
  *   - Spine-indexed metadata CFs (one entry per block / stack): [[StoreKey.BlockResult]] —
  *     `Cf.BlockResult`, keyed by `blockNum`. [[StoreKey.SoftConfirmation]] —
  *     `Cf.SoftConfirmation`, keyed by `blockNum`. [[StoreKey.HardConfirmation]] —
  *     `Cf.HardConfirmation`, keyed by `stackNum`. [[StoreKey.EvacuationMap]] — `Cf.EvacuationMap`,
  *     keyed by `blockNum` (per-block; see the case docstring for why).
  *     [[StoreKey.RequestHighWater]] — `Cf.RequestHighWater`, keyed by `blockNum`.
  *     [[StoreKey.L2CommandNumber]] — `Cf.L2CommandNumber`, keyed by `blockNum`.
  *     [[StoreKey.UnsignedStack]] — `Cf.UnsignedStack`, keyed by `stackNum`.
  *   - Singleton snapshot CFs (one entry total): [[StoreKey.DepositMap]], [[StoreKey.Treasury]].
  *   - Store-level metadata: [[StoreKey.Meta]] — `Cf.Meta`, name-keyed.
  *
  * Each subtype declares its `Value` and a `given codec: StoreCodec[Value]`; the trait's
  * `encodeValue` / `decodeValue` delegate to the codec, so cases never repeat the
  * `asJson`/`decode`/throw boilerplate. CFs whose typed payload hasn't been wired yet keep
  * `type Value = Array[Byte]`; the universal `StoreCodec.passthrough` from the companion supplies
  * the codec, so those cases are zero-line.
  *
  * See `design/persistence-and-crash-recovery.md` §7 / §7.1.
  */
trait StoreKey:
    /** The typed payload stored at this key. */
    type Value

    /** Byte codec for [[Value]]. Concrete cases either declare one explicitly (typed payloads) or
      * inherit `StoreCodec.passthrough` via implicit scope (passthrough byte cases).
      */
    given codec: StoreCodec[Value]

    /** The column family this key addresses. */
    def cf: Cf

    /** The bytes used as the RocksDB key. Big-endian fixed-width for the spine-indexed CFs (so lex
      * order matches numeric order — see §7.1).
      */
    def encode: Array[Byte]

    /** Encode a typed value to its persisted byte form. Delegates to the case's [[codec]]. */
    final def encodeValue(value: Value)(using CardanoNetwork.Section): Array[Byte] =
        codec.encode(value)

    /** Decode bytes read from storage into the typed value. Throws on a malformed payload
      * (interpreted as store corruption — fail-safe). Delegates to the case's [[codec]].
      */
    final def decodeValue(bytes: Array[Byte])(using CardanoNetwork.Section): Value =
        codec.decode(bytes)

object StoreKey:

    /** Key for [[Cf.BlockResult]] — the per-block JL output, keyed by `blockNum`. */
    final case class BlockResult(num: BlockNumber) extends StoreKey:
        type Value = LedgerBlockResult
        import BlockResultCodec.given
        given codec: StoreCodec[Value] = StoreCodec.fromCirce[Value]
        val cf: Cf = Cf.BlockResult
        def encode: Array[Byte] = FamilyKey.intBytes(num)

    /** Key for [[Cf.SoftConfirmation]] — FCA aggregate (header + multisig), keyed by `blockNum`.
      * `softConfirmed` derives as the last key in this CF (§5.2).
      */
    final case class SoftConfirmation(num: BlockNumber) extends StoreKey:
        type Value = Block.SoftConfirmed.Next
        import SoftConfirmationCodec.given
        given codec: StoreCodec[Value] = StoreCodec.fromCirce[Value]
        val cf: Cf = Cf.SoftConfirmation
        def encode: Array[Byte] = FamilyKey.intBytes(num)

    /** Key for [[Cf.HardConfirmation]] — SCA multisigned effects / SECs / fallbacks, keyed by
      * `stackNum`. `hardConfirmed` derives as the last key in this CF (§5.2). The R10 evacuation
      * floor — never deleted.
      *
      * Value type = `hydrozoa.multisig.ledger.stack.StackEffects.HardConfirmed`. Codec routes
      * through `persistence.codec.StackEffectsCodec` (lifted to `StoreCodec` via the universal
      * `StoreCodec.fromCirce`).
      */
    final case class HardConfirmation(num: StackNumber) extends StoreKey:
        type Value = StackEffects.HardConfirmed
        import StackEffectsCodec.given
        given codec: StoreCodec[Value] = StoreCodec.fromCirce[Value]
        val cf: Cf = Cf.HardConfirmation
        def encode: Array[Byte] = FamilyKey.intBytes(num)

    /** Key for [[Cf.DepositMap]] — the single blob holding JL's deposits map at `softAcked`. */
    case object DepositMap extends StoreKey:
        type Value = DepositsMap
        import DepositMapCodec.given
        given codec: StoreCodec[Value] = StoreCodec.fromCirce[Value]
        val cf: Cf = Cf.DepositMap
        def encode: Array[Byte] = singletonKey

    /** Key for [[Cf.Treasury]] — the single blob holding SC's treasury UTXO chain at `hardAcked`.
      *
      * Value type = `hydrozoa.multisig.ledger.l1.utxo.MultisigTreasuryUtxo`. Codec routes through
      * `persistence.codec.TreasuryCodec` (CIP-116 leaves + `HydrozoaLocalCodecs` for `Datum` /
      * `Equity`).
      */
    case object Treasury extends StoreKey:
        type Value = MultisigTreasuryUtxo
        import TreasuryCodec.given
        given codec: StoreCodec[Value] = StoreCodec.fromCirce[Value]
        val cf: Cf = Cf.Treasury
        def encode: Array[Byte] = singletonKey

    /** Key for [[Cf.EvacuationMap]] — the cumulative evacuation map at each block, keyed by
      * `blockNum`. One entry per soft-confirmed block: SC folds the per-block `evacuationMapDiff`s
      * from `BlockResult` onto the running map and persists the result.
      *
      * Why per-block, not a singleton snapshot: the rule-based dispute can land on **any** minor
      * block in the latest major's tail, and evacuation needs the map exactly at that block. The
      * simplest durable shape is to keep every map; pruning is bounded — anything strictly older
      * than the last-hard-confirmed major can be dropped (those minors can never be disputed
      * against, since the next major supersedes them).
      *
      * Value type = `hydrozoa.multisig.ledger.joint.EvacuationMap`. Codec routes through the
      * existing Circe instances declared on `EvacuationMap`'s companion (the decoder needs
      * `CardanoNetwork.Section` for `Payout.Obligation`, threaded via `StoreCodec.fromCirce`).
      */
    final case class EvacuationMap(num: BlockNumber) extends StoreKey:
        type Value = JointEvacuationMap
        import JointEvacuationMap.given
        given codec: StoreCodec[Value] = StoreCodec.fromCirce[Value]
        val cf: Cf = Cf.EvacuationMap
        def encode: Array[Byte] = FamilyKey.intBytes(num)

    /** Key for [[Cf.RequestHighWater]] — JointLedger's cumulative per-peer request high-water
      * `Map[HeadPeerNumber, RequestNumber]` **as of block `num`** (the highest request number from
      * each author included in any block `≤ num`). One entry per own soft-ack, written in the same
      * atomic bundle as that block; entries are monotone-non-decreasing in `num`. Not read by JL's
      * own recover; the `ReplayActor` reads `RequestHighWater(softAcked)` to seed each peer's
      * RequestLane resume cursor (`high-water + 1`, §5.3). Block-keyed (not a singleton) so the
      * high-water at any committed block is recoverable, mirroring the other spine-indexed CFs.
      */
    final case class RequestHighWater(num: BlockNumber) extends StoreKey:
        type Value = Map[HeadPeerNumber, RequestNumber]
        import RequestHighWaterCodec.given
        given codec: StoreCodec[Value] = StoreCodec.fromCirce[Value]
        val cf: Cf = Cf.RequestHighWater
        def encode: Array[Byte] = FamilyKey.intBytes(num)

    /** Key for [[Cf.L2CommandNumber]] — the L2 ledger's commit counter
      * ([[hydrozoa.multisig.ledger.l2.L2CommandNumber]]) reached after block `num`'s L2 commits.
      * One entry per own soft-ack, written in the same atomic bundle as that block. JointLedger's
      * own recover reads `L2CommandNumber(softAcked)` and calls `l2Ledger.restoreTo(it)` to
      * co-anchor the committed L2 state to the fast-side `softAcked` boundary (§R2b). Block-keyed,
      * mirroring the other spine-indexed CFs.
      */
    final case class L2CommandNumber(num: BlockNumber) extends StoreKey:
        type Value = LedgerL2CommandNumber
        import LedgerL2CommandNumber.given
        given codec: StoreCodec[Value] = StoreCodec.fromCirce[Value]
        val cf: Cf = Cf.L2CommandNumber
        def encode: Array[Byte] = FamilyKey.intBytes(num)

    /** Key for [[Cf.UnsignedStack]] — the `Stack.Unsigned` (brief + locally-derived effects)
      * StackComposer closed for `stackNum`, persisted **before** the handoff to
      * `SlowConsensusActor` (CR4/CR8: durable before the own ack crosses the peer boundary via
      * SCA's broadcast). On recovery the `ReplayActor` reads the in-flight stack's value and the
      * own acks (HardAck family) to reconstruct the handoff into SCA — a `HardAck` signs the
      * effects, which a `StackBrief` alone does not carry, so the effects must be durable. Keyed by
      * `stackNum`.
      */
    final case class UnsignedStack(num: StackNumber) extends StoreKey:
        type Value = Stack.Unsigned
        import UnsignedStackCodec.given
        given codec: StoreCodec[Value] = StoreCodec.fromCirce[Value]
        val cf: Cf = Cf.UnsignedStack
        def encode: Array[Byte] = FamilyKey.intBytes(num)

    /** Key for [[Cf.Meta]] — store-level metadata, name-keyed (UTF-8). */
    final case class Meta(name: String) extends StoreKey:
        type Value = Array[Byte]
        given codec: StoreCodec[Value] = StoreCodec.passthrough
        val cf: Cf = Cf.Meta
        def encode: Array[Byte] = name.getBytes("UTF-8")

    /** The fixed key shared by every singleton CF. The bytes are irrelevant (only one key exists
      * per such CF); we use the empty array so it sorts cleanly and costs nothing.
      */
    private val singletonKey: Array[Byte] = Array.emptyByteArray
