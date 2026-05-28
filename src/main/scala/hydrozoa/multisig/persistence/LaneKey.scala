package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import java.nio.ByteBuffer

/** A full addressable entry key in the persistence layer — a [[LaneId]] paired with the within-lane
  * index.
  *
  * Encoded byte form (per §7.1, big-endian fixed-width so lexicographic byte order matches numeric
  * index order, the property that makes range scans correct):
  *
  *   - `Block` → `[blockNum : 4]`
  *   - `Stack` → `[stackNum : 4]`
  *   - `Request` → `[peer : 1][requestNum : 8]`
  *   - `SoftAck` → `[peer : 1][softAckNum : 4]`
  *   - `HardAck` → `[peer : 1][hardAckNum : 4]`
  *
  * The lane-type discriminant is the column family ([[Cf]]) — no tag byte in the key.
  */
enum LaneKey extends StoreKey:
    case Block(num: BlockNumber)
    case Stack(num: StackNumber)
    case Request(peer: HeadPeerNumber, num: RequestNumber)
    case SoftAck(peer: HeadPeerNumber, num: SoftAckNumber)
    case HardAck(peer: HeadPeerNumber, num: HardAckNumber)

    /** Lane payload — scaffolding placeholder. Real per-case Value types (e.g. `BlockBrief`,
      * `SoftAck`, …) and wire-codec bodies land per CF as actors wire them; replacing this type
      * member per case will require LaneKey to convert from enum to sealed-trait, which we'll do at
      * that point. See [[StoreKey]] for the rationale.
      */
    type Value = Array[Byte]

    /** The lane this key belongs to (and therefore the column family — see [[LaneId.cf]]). */
    def laneId: LaneId = this match
        case Block(_)      => LaneId.BlockSpine
        case Stack(_)      => LaneId.StackSpine
        case Request(p, _) => LaneId.Request(p)
        case SoftAck(p, _) => LaneId.SoftAck(p)
        case HardAck(p, _) => LaneId.HardAck(p)

    /** The column family this lane key lives in — delegates through [[LaneId.cf]]. Satisfies the
      * [[StoreKey]] contract.
      */
    def cf: Cf = laneId.cf

    /** Encode this key into bytes for the lane's column family. */
    def encode: Array[Byte] = this match
        case Block(num)         => LaneKey.intBytes(num)
        case Stack(num)         => LaneKey.intBytes(num)
        case Request(peer, num) => LaneKey.peerByte(peer) ++ LaneKey.longBytes(num)
        case SoftAck(peer, num) => LaneKey.peerByte(peer) ++ LaneKey.intBytes(num)
        case HardAck(peer, num) => LaneKey.peerByte(peer) ++ LaneKey.intBytes(num)

    /** Passthrough value codec — replaced per case (with the real wire codec, including the 8-byte
      * arrival-stamp prefix from §5.5) once a typed payload is wired.
      */
    def encodeValue(value: Value): Array[Byte] = value
    def decodeValue(bytes: Array[Byte]): Value = bytes

object LaneKey:
    /** Decode a key from its byte form, given the CF the bytes came from. Throws on a malformed
      * payload (interpreted as store corruption — fail safe).
      */
    def decode(cf: Cf, bytes: Array[Byte]): LaneKey = cf match
        case Cf.Block =>
            requireLen(cf, bytes, 4)
            Block(BlockNumber(readIntBE(bytes, 0)))
        case Cf.Stack =>
            requireLen(cf, bytes, 4)
            Stack(StackNumber(readIntBE(bytes, 0)))
        case Cf.Request =>
            requireLen(cf, bytes, 1 + 8)
            Request(HeadPeerNumber(readPeer(bytes, 0)), RequestNumber(readLongBE(bytes, 1)))
        case Cf.SoftAck =>
            requireLen(cf, bytes, 1 + 4)
            SoftAck(HeadPeerNumber(readPeer(bytes, 0)), SoftAckNumber(readIntBE(bytes, 1)))
        case Cf.HardAck =>
            requireLen(cf, bytes, 1 + 4)
            HardAck(HeadPeerNumber(readPeer(bytes, 0)), HardAckNumber(readIntBE(bytes, 1)))
        case Cf.BlockResult | Cf.SoftConfirmation | Cf.HardConfirmation | Cf.DepositMap |
            Cf.Treasury | Cf.EvacuationMap | Cf.Meta =>
            throw new IllegalArgumentException(
              s"$cf is not a lane CF; LaneKey.decode is undefined for it"
            )

    /** Encode a non-negative `Int` as 4 big-endian bytes. */
    private[persistence] def intBytes(n: Int): Array[Byte] =
        ByteBuffer.allocate(4).putInt(n).array()

    /** Encode a non-negative `Long` as 8 big-endian bytes. */
    private[persistence] def longBytes(n: Long): Array[Byte] =
        ByteBuffer.allocate(8).putLong(n).array()

    /** Encode a `HeadPeerNumber` (constrained `< 2⁸`) as a single byte. */
    private[persistence] def peerByte(peer: HeadPeerNumber): Array[Byte] =
        Array(((peer: Int) & 0xff).toByte)

    private def readIntBE(bytes: Array[Byte], offset: Int): Int =
        ByteBuffer.wrap(bytes, offset, 4).getInt

    private def readLongBE(bytes: Array[Byte], offset: Int): Long =
        ByteBuffer.wrap(bytes, offset, 8).getLong

    private def readPeer(bytes: Array[Byte], offset: Int): Int =
        bytes(offset) & 0xff

    private def requireLen(cf: Cf, bytes: Array[Byte], expected: Int): Unit =
        if bytes.length != expected then
            throw new IllegalArgumentException(
              s"$cf key has ${bytes.length} bytes; expected $expected"
            )
