package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.ack.{HardAck as HardAckMsg, HardAckNumber, HardAckWithId, HubHardAckNumber, SoftAck as SoftAckMsg, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.Codecs.given
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import java.nio.ByteBuffer

/** A full addressable entry key in the persistence layer — a [[JournalId]] paired with the
  * within-journal index.
  *
  * A *journal* is the **recovery** concept — one author's arrival-stamped, index-ordered
  * append-only replay sequence (scanned by [[recovery.JournalScan]], total-ordered across journals
  * by [[recovery.ArrivalOrderedMerge]]). It is a **subset** of the column families: `JournalKey
  * extends StoreKey`, so "journal" ≠ "column family" — every CF is a [[StoreKey]], but only the
  * replayable ones are a `JournalKey`. The 14 non-journal CFs (snapshots, key-ordered / `max(key)`
  * reconstruction reads) stay plain [[StoreKey]]: unstamped, never merged.
  *
  * Encoded byte form (per §7.1, big-endian fixed-width so lexicographic byte order matches numeric
  * index order, the property that makes range scans correct):
  *
  *   - `Block` → `[blockNum : 4]`
  *   - `Stack` → `[stackNum : 4]`
  *   - `Request` → `[requestNum : 8]`
  *   - `SoftAck` → `[softAckNum : 4]`
  *   - `HardAck` → `[hardAckNum : 4]`
  *   - `HubHardAck` → `[hubHardAckNum : 4]`
  *
  * Both the journal-type **and the author** are the column family ([[Cf]], split one CF per author,
  * §3.2) — the key carries neither a type tag nor an author prefix, only the within-author index.
  *
  * **Each satellite CF is one author's journal.** With the per-author split the whole CF is a
  * single author's entries in index order, so a scan from the cursor to end-of-CF *is* the whole
  * journal — [[recovery.JournalScan]] needs no peer-prefix bounding. The author is recovered from
  * the CF on [[decode]] (the cursor passes the per-author [[Cf]] it scanned).
  *
  * Each case's `Value` is a [[JournalValue]] wrapping that journal's wire payload (the wire codec
  * from `consensus.transport.Codecs`, reused under the 12-byte arrival-stamp prefix via
  * [[StoreCodec.journalValue]]; §5.4, §7.1). A sealed trait (not an `enum`) so each case can
  * declare its own path-dependent `Value` + codec.
  */
sealed trait JournalKey extends StoreKey:
    /** The journal this key belongs to (and therefore the column family — see [[JournalId.cf]]). */
    def journalId: JournalId

    /** The column family this journal key lives in — delegates through [[JournalId.cf]]. Satisfies
      * the [[StoreKey]] contract.
      */
    final def cf: Cf = journalId.cf

object JournalKey:

    /** Block spine: the block brief, keyed by `blockNum`. */
    final case class Block(num: BlockNumber) extends JournalKey:
        type Value = JournalValue[BlockBrief.Next]
        given codec: StoreCodec[Value] = StoreCodec.journalValue[BlockBrief.Next]
        def journalId: JournalId = JournalId.BlockSpine
        def encode: Array[Byte] = intBytes(num)

    /** Stack spine: the stack brief, keyed by `stackNum`. */
    final case class Stack(num: StackNumber) extends JournalKey:
        type Value = JournalValue[StackBrief]
        given codec: StoreCodec[Value] = StoreCodec.journalValue[StackBrief]
        def journalId: JournalId = JournalId.StackSpine
        def encode: Array[Byte] = intBytes(num)

    /** Request satellite (per author): the assigned user request, keyed by `(peer, requestNum)`. */
    final case class Request(peer: HeadPeerNumber, num: RequestNumber) extends JournalKey:
        type Value = JournalValue[UserRequestWithId]
        given codec: StoreCodec[Value] = StoreCodec.journalValue[UserRequestWithId]
        def journalId: JournalId = JournalId.Request(peer)
        def encode: Array[Byte] = longBytes(num)

    /** Soft-ack satellite (per author): a peer's soft-ack signature, keyed by `(peer, softAckNum)`.
      */
    final case class SoftAck(peer: HeadPeerNumber, num: SoftAckNumber) extends JournalKey:
        type Value = JournalValue[SoftAckMsg]
        given codec: StoreCodec[Value] = StoreCodec.journalValue[SoftAckMsg]
        def journalId: JournalId = JournalId.SoftAck(peer)
        def encode: Array[Byte] = intBytes(num)

    /** Hard-ack satellite (per author): a peer's hard-ack signature, keyed by `(peer, hardAckNum)`.
      * The author is a [[PeerId]] — head and coil peers share one journal type, one CF per author.
      * A head peer's journal holds its own head hard-acks; a coil peer's journal holds its own
      * hard-acks plus a hub's raw inbound receive copy (persisted by `PeerLiaisonHubToCoil` on
      * receipt so a coil peer's ack survives a hub crash before `CoilAckSequencer` re-sequences
      * it). The key bytes carry only the within-author index — the author comes from the CF.
      */
    final case class HardAck(peer: PeerId, num: HardAckNumber) extends JournalKey:
        type Value = JournalValue[HardAckMsg]
        given codec: StoreCodec[Value] = StoreCodec.journalValue[HardAckMsg]
        def journalId: JournalId = JournalId.HardAck(peer)
        def encode: Array[Byte] = intBytes(num)

    /** Hub-hard-ack journal (per hub): the re-sequenced coil hard-ack (`HardAckWithId`) that
      * travels the head mesh and the hub→coil links, keyed by `(hub, hubHardAckNum)`.
      */
    final case class HubHardAck(hub: HeadPeerNumber, num: HubHardAckNumber) extends JournalKey:
        type Value = JournalValue[HardAckWithId]
        given codec: StoreCodec[Value] = StoreCodec.journalValue[HardAckWithId]
        def journalId: JournalId = JournalId.HubHardAck(hub)
        def encode: Array[Byte] = intBytes(num)

    /** Decode a key from its byte form, given the CF the bytes came from. Throws on a malformed
      * payload (interpreted as store corruption — fail safe).
      */
    def decode(cf: Cf, bytes: Array[Byte]): JournalKey = cf match
        case Cf.Block =>
            requireLen(cf, bytes, 4)
            Block(BlockNumber(readIntBE(bytes, 0)))
        case Cf.Stack =>
            requireLen(cf, bytes, 4)
            Stack(StackNumber(readIntBE(bytes, 0)))
        case Cf.Request(peer) =>
            requireLen(cf, bytes, 8)
            Request(peer, RequestNumber(readLongBE(bytes, 0)))
        case Cf.SoftAck(peer) =>
            requireLen(cf, bytes, 4)
            SoftAck(peer, SoftAckNumber(readIntBE(bytes, 0)))
        case Cf.HardAck(peer) =>
            requireLen(cf, bytes, 4)
            HardAck(peer, HardAckNumber(readIntBE(bytes, 0)))
        case Cf.HubHardAck(hub) =>
            requireLen(cf, bytes, 4)
            HubHardAck(hub, HubHardAckNumber(readIntBE(bytes, 0)))
        case Cf.BlockResult | Cf.SoftConfirmation | Cf.HardConfirmation | Cf.DepositMap |
            Cf.Treasury | Cf.EvacuationMap | Cf.RequestHighWater | Cf.CoilStampMark |
            Cf.L2CommandNumber | Cf.UnsignedStack | Cf.RequestBlockIndex |
            Cf.DepositAbsorptionIndex | Cf.WithdrawalEffectIndex | Cf.BlockStackIndex |
            Cf.EffectStackIndex | Cf.Meta =>
            throw new IllegalArgumentException(
              s"$cf is not a journal CF; JournalKey.decode is undefined for it"
            )

    /** Encode a non-negative `Int` as 4 big-endian bytes. */
    private[persistence] def intBytes(n: Int): Array[Byte] =
        ByteBuffer.allocate(4).putInt(n).array()

    /** Encode a non-negative `Long` as 8 big-endian bytes. */
    private[persistence] def longBytes(n: Long): Array[Byte] =
        ByteBuffer.allocate(8).putLong(n).array()

    private def readIntBE(bytes: Array[Byte], offset: Int): Int =
        ByteBuffer.wrap(bytes, offset, 4).getInt

    private def readLongBE(bytes: Array[Byte], offset: Int): Long =
        ByteBuffer.wrap(bytes, offset, 8).getLong

    private def requireLen(cf: Cf, bytes: Array[Byte], expected: Int): Unit =
        if bytes.length != expected then
            throw new IllegalArgumentException(
              s"$cf key has ${bytes.length} bytes; expected $expected"
            )
