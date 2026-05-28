package hydrozoa.multisig.persistence

import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber

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
  *     `Cf.HardConfirmation`, keyed by `stackNum`.
  *   - Singleton snapshot CFs (one entry total): [[StoreKey.DepositMap]], [[StoreKey.Treasury]],
  *     [[StoreKey.EvacuationMap]].
  *   - Store-level metadata: [[StoreKey.Meta]] — `Cf.Meta`, name-keyed.
  *
  * Each subtype declares its associated [[Value]] type and owns its codec (`encodeValue` /
  * `decodeValue`). **Scaffolding state (today):** every concrete `StoreKey` has
  * `type Value = Array[Byte]` and a passthrough codec — the typed API SHAPE is in place but real
  * payload types + codecs land per CF as the first consumer wires them. When that happens, the
  * specific CF's `type Value` is replaced (e.g. `type Value = BlockBrief`) and the codec body is
  * filled in; the surrounding API does not change.
  *
  * See `design/persistence-and-crash-recovery.md` §7 / §7.1.
  */
trait StoreKey:
    /** The typed payload stored at this key. */
    type Value

    /** The column family this key addresses. */
    def cf: Cf

    /** The bytes used as the RocksDB key. Big-endian fixed-width for the spine-indexed CFs (so lex
      * order matches numeric order — see §7.1).
      */
    def encode: Array[Byte]

    /** Encode a typed value to its persisted byte form. */
    def encodeValue(value: Value): Array[Byte]

    /** Decode bytes read from storage into the typed value. Throws on a malformed payload
      * (interpreted as store corruption — fail-safe).
      */
    def decodeValue(bytes: Array[Byte]): Value

object StoreKey:

    /** Key for [[Cf.BlockResult]] — the per-block JL output, keyed by `blockNum`. */
    final case class BlockResult(num: BlockNumber) extends StoreKey:
        type Value = Array[Byte]
        val cf: Cf = Cf.BlockResult
        def encode: Array[Byte] = LaneKey.intBytes(num)
        def encodeValue(value: Value): Array[Byte] = value
        def decodeValue(bytes: Array[Byte]): Value = bytes

    /** Key for [[Cf.SoftConfirmation]] — FCA aggregate (header + multisig), keyed by `blockNum`.
      * `softConfirmed` derives as the last key in this CF (§5.2).
      */
    final case class SoftConfirmation(num: BlockNumber) extends StoreKey:
        type Value = Array[Byte]
        val cf: Cf = Cf.SoftConfirmation
        def encode: Array[Byte] = LaneKey.intBytes(num)
        def encodeValue(value: Value): Array[Byte] = value
        def decodeValue(bytes: Array[Byte]): Value = bytes

    /** Key for [[Cf.HardConfirmation]] — SCA multisigned effects / SECs / fallbacks, keyed by
      * `stackNum`. `hardConfirmed` derives as the last key in this CF (§5.2). The R10 evacuation
      * floor — never deleted.
      */
    final case class HardConfirmation(num: StackNumber) extends StoreKey:
        type Value = Array[Byte]
        val cf: Cf = Cf.HardConfirmation
        def encode: Array[Byte] = LaneKey.intBytes(num)
        def encodeValue(value: Value): Array[Byte] = value
        def decodeValue(bytes: Array[Byte]): Value = bytes

    /** Key for [[Cf.DepositMap]] — the single blob holding JL's deposits map at `softAcked`. */
    case object DepositMap extends StoreKey:
        type Value = Array[Byte]
        val cf: Cf = Cf.DepositMap
        def encode: Array[Byte] = singletonKey
        def encodeValue(value: Value): Array[Byte] = value
        def decodeValue(bytes: Array[Byte]): Value = bytes

    /** Key for [[Cf.Treasury]] — the single blob holding SC's treasury UTXO chain at `hardAcked`.
      */
    case object Treasury extends StoreKey:
        type Value = Array[Byte]
        val cf: Cf = Cf.Treasury
        def encode: Array[Byte] = singletonKey
        def encodeValue(value: Value): Array[Byte] = value
        def decodeValue(bytes: Array[Byte]): Value = bytes

    /** Key for [[Cf.EvacuationMap]] — the single blob holding SC's evacuation map at `hardAcked`.
      */
    case object EvacuationMap extends StoreKey:
        type Value = Array[Byte]
        val cf: Cf = Cf.EvacuationMap
        def encode: Array[Byte] = singletonKey
        def encodeValue(value: Value): Array[Byte] = value
        def decodeValue(bytes: Array[Byte]): Value = bytes

    /** Key for [[Cf.Meta]] — store-level metadata, name-keyed (UTF-8). */
    final case class Meta(name: String) extends StoreKey:
        type Value = Array[Byte]
        val cf: Cf = Cf.Meta
        def encode: Array[Byte] = name.getBytes("UTF-8")
        def encodeValue(value: Value): Array[Byte] = value
        def decodeValue(bytes: Array[Byte]): Value = bytes

    /** The fixed key shared by every singleton CF. The bytes are irrelevant (only one key exists
      * per such CF); we use the empty array so it sorts cleanly and costs nothing.
      */
    private val singletonKey: Array[Byte] = Array.emptyByteArray
