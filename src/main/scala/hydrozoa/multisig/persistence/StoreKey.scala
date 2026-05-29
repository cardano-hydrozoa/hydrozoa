package hydrozoa.multisig.persistence

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.joint.EvacuationMap as JointEvacuationMap
import hydrozoa.multisig.ledger.l1.utxo.MultisigTreasuryUtxo
import hydrozoa.multisig.ledger.stack.{StackEffects, StackNumber}
import hydrozoa.multisig.persistence.codec.{StackEffectsCodec, TreasuryCodec}

/** The typed key surface for the high-level persistence API.
  *
  * Every column family ([[Cf]]) has exactly one **key shape** and exactly one **value type** — a
  * corresponding `StoreKey` subtype captures both, so actor code addresses entries by name and
  * exchanges typed values, never raw bytes. The 5 lane CFs are reached through [[LaneKey]] (which
  * extends `StoreKey`); the 7 non-lane CFs are covered by the cases declared in the companion
  * below:
  *
  *   - Spine-indexed metadata CFs (one entry per block / stack): [[StoreKey.BlockResult]] —
  *     `Cf.BlockResult`, keyed by `blockNum`. [[StoreKey.SoftConfirmation]] —
  *     `Cf.SoftConfirmation`, keyed by `blockNum`. [[StoreKey.HardConfirmation]] —
  *     `Cf.HardConfirmation`, keyed by `stackNum`. [[StoreKey.EvacuationMap]] — `Cf.EvacuationMap`,
  *     keyed by `blockNum` (per-block; see the case docstring for why).
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
        type Value = Array[Byte]
        given codec: StoreCodec[Value] = StoreCodec.passthrough
        val cf: Cf = Cf.BlockResult
        def encode: Array[Byte] = LaneKey.intBytes(num)

    /** Key for [[Cf.SoftConfirmation]] — FCA aggregate (header + multisig), keyed by `blockNum`.
      * `softConfirmed` derives as the last key in this CF (§5.2).
      */
    final case class SoftConfirmation(num: BlockNumber) extends StoreKey:
        type Value = Array[Byte]
        given codec: StoreCodec[Value] = StoreCodec.passthrough
        val cf: Cf = Cf.SoftConfirmation
        def encode: Array[Byte] = LaneKey.intBytes(num)

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
        def encode: Array[Byte] = LaneKey.intBytes(num)

    /** Key for [[Cf.DepositMap]] — the single blob holding JL's deposits map at `softAcked`. */
    case object DepositMap extends StoreKey:
        type Value = Array[Byte]
        given codec: StoreCodec[Value] = StoreCodec.passthrough
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
        def encode: Array[Byte] = LaneKey.intBytes(num)

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
