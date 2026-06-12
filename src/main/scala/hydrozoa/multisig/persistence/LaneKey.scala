package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.ack.{HardAck as HardAckMsg, HardAckNumber, HardAckWithId, HubHardAckNumber, SoftAck as SoftAckMsg, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import hydrozoa.multisig.consensus.transport.Codecs.given
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
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
  *   - `CoilHardAck` → `[coil : 1][hardAckNum : 4]`
  *   - `HubHardAck` → `[hub : 1][hubHardAckNum : 4]`
  *
  * The lane-type discriminant is the column family ([[Cf]]) — no tag byte in the key.
  *
  * **Satellite CFs are peer-major.** Because the `peer` byte **leads** the key, the store's
  * unsigned-lex key order groups all of one author's entries together (ascending by index), then
  * the next author's, and so on. So within a satellite CF a single peer's lane is a **contiguous**
  * run — the property [[recovery.LaneScan]] relies on to bound a per-peer scan (stop at the first
  * key whose `peer` differs). The spine CFs carry no peer byte, so the whole CF is one
  * index-ordered lane.
  *
  * Each case's `Value` is a [[LaneValue]] wrapping that lane's wire payload (the wire codec from
  * `consensus.transport.Codecs`, reused under the 12-byte arrival-stamp prefix via
  * [[StoreCodec.laneValue]]; §5.4, §7.1). A sealed trait (not an `enum`) so each case can declare
  * its own path-dependent `Value` + codec.
  */
sealed trait LaneKey extends StoreKey:
    /** The lane this key belongs to (and therefore the column family — see [[LaneId.cf]]). */
    def laneId: LaneId

    /** The column family this lane key lives in — delegates through [[LaneId.cf]]. Satisfies the
      * [[StoreKey]] contract.
      */
    final def cf: Cf = laneId.cf

object LaneKey:

    /** Block spine: the block brief, keyed by `blockNum`. */
    final case class Block(num: BlockNumber) extends LaneKey:
        type Value = LaneValue[BlockBrief.Next]
        given codec: StoreCodec[Value] = StoreCodec.laneValue[BlockBrief.Next]
        def laneId: LaneId = LaneId.BlockSpine
        def encode: Array[Byte] = intBytes(num)

    /** Stack spine: the stack brief, keyed by `stackNum`. */
    final case class Stack(num: StackNumber) extends LaneKey:
        type Value = LaneValue[StackBrief]
        given codec: StoreCodec[Value] = StoreCodec.laneValue[StackBrief]
        def laneId: LaneId = LaneId.StackSpine
        def encode: Array[Byte] = intBytes(num)

    /** Request satellite (per author): the assigned user request, keyed by `(peer, requestNum)`. */
    final case class Request(peer: HeadPeerNumber, num: RequestNumber) extends LaneKey:
        type Value = LaneValue[UserRequestWithId]
        given codec: StoreCodec[Value] = StoreCodec.laneValue[UserRequestWithId]
        def laneId: LaneId = LaneId.Request(peer)
        def encode: Array[Byte] = peerByte(peer) ++ longBytes(num)

    /** Soft-ack satellite (per author): a peer's soft-ack signature, keyed by `(peer, softAckNum)`.
      */
    final case class SoftAck(peer: HeadPeerNumber, num: SoftAckNumber) extends LaneKey:
        type Value = LaneValue[SoftAckMsg]
        given codec: StoreCodec[Value] = StoreCodec.laneValue[SoftAckMsg]
        def laneId: LaneId = LaneId.SoftAck(peer)
        def encode: Array[Byte] = peerByte(peer) ++ intBytes(num)

    /** Hard-ack satellite (per author): a peer's hard-ack signature, keyed by `(peer, hardAckNum)`.
      */
    final case class HardAck(peer: HeadPeerNumber, num: HardAckNumber) extends LaneKey:
        type Value = LaneValue[HardAckMsg]
        given codec: StoreCodec[Value] = StoreCodec.laneValue[HardAckMsg]
        def laneId: LaneId = LaneId.HardAck(peer)
        def encode: Array[Byte] = peerByte(peer) ++ intBytes(num)

    /** Coil-hard-ack receive lane (per coil peer): a hub's raw inbound hard-ack from one of its
      * coil peers, keyed by `(coil, hardAckNum)`. Persisted by `PeerLiaisonHubToCoil` on receipt so
      * a coil peer's ack survives a hub crash before `CoilAckSequencer` re-sequences it.
      */
    final case class CoilHardAck(coil: CoilPeerNumber, num: HardAckNumber) extends LaneKey:
        type Value = LaneValue[HardAckMsg]
        given codec: StoreCodec[Value] = StoreCodec.laneValue[HardAckMsg]
        def laneId: LaneId = LaneId.CoilHardAck(coil)
        def encode: Array[Byte] = coilByte(coil) ++ intBytes(num)

    /** Hub-hard-ack lane (per hub): the re-sequenced coil hard-ack (`HardAckWithId`) that travels
      * the head mesh and the hub→coil links, keyed by `(hub, hubHardAckNum)`.
      */
    final case class HubHardAck(hub: HeadPeerNumber, num: HubHardAckNumber) extends LaneKey:
        type Value = LaneValue[HardAckWithId]
        given codec: StoreCodec[Value] = StoreCodec.laneValue[HardAckWithId]
        def laneId: LaneId = LaneId.HubHardAck(hub)
        def encode: Array[Byte] = peerByte(hub) ++ intBytes(num)

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
        case Cf.CoilHardAck =>
            requireLen(cf, bytes, 1 + 4)
            CoilHardAck(CoilPeerNumber(readPeer(bytes, 0)), HardAckNumber(readIntBE(bytes, 1)))
        case Cf.HubHardAck =>
            requireLen(cf, bytes, 1 + 4)
            HubHardAck(HeadPeerNumber(readPeer(bytes, 0)), HubHardAckNumber(readIntBE(bytes, 1)))
        case Cf.BlockResult | Cf.SoftConfirmation | Cf.HardConfirmation | Cf.DepositMap |
            Cf.Treasury | Cf.EvacuationMap | Cf.RequestHighWater | Cf.L2CommandNumber |
            Cf.UnsignedStack | Cf.Meta =>
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

    /** Encode a `CoilPeerNumber` (constrained `< 2⁸`) as a single byte. */
    private[persistence] def coilByte(coil: CoilPeerNumber): Array[Byte] =
        Array(((coil: Int) & 0xff).toByte)

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
