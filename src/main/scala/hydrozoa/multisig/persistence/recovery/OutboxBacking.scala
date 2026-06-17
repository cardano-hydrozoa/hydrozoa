package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, HardAckWithId, HubHardAckNumber, SoftAck, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import hydrozoa.multisig.persistence.{BackendStore, Cf, FamilyKey}
import java.nio.ByteBuffer

/** The store backing for one liaison outbound lane
  * ([[hydrozoa.multisig.consensus.liaison.LaneOutbound]]): the durable family the lane's own
  * production lives in, surfaced as the two reads a lane needs to recover lazily rather than seed
  * eagerly (§6, recovery doc).
  *
  *   - [[highWater]] — the lane's last durable number (`lastKey` of the family), so `preStart`
  *     seeds only the high-water (no payloads): the gap-free [[LaneOutbound.append]] check and the
  *     out-of-bounds guard work, and replay can re-append the live tail on top.
  *   - [[backfill]] — up to `limit` payloads from a `from` number, so [[LaneOutbound.reply]] reads
  *     the prefix below its in-memory outbox floor when a remote pulls an old entry, instead of
  *     holding the whole own production in memory.
  *
  * `keep` filters a spine family to this peer's own-led entries (a head-mesh liaison serves only
  * its own-led briefs); the satellites are already a single author per CF, and a hub→coil link
  * serves every author, so both pass the default accept-all.
  */
final class OutboxBacking[T, N] private (
    backend: BackendStore[IO],
    cf: Cf,
    seekKey: N => FamilyKey,
    decodeNum: Array[Byte] => N,
    decodePayload: Array[Byte] => T,
    keep: T => Boolean
):
    /** The lane's last durable number, or `None` for an empty family. */
    def highWater: IO[Option[N]] = backend.lastKey(cf).map(_.map(decodeNum))

    /** Up to `limit` own-produced payloads with number `>= from`, ascending. */
    def backfill(from: N, limit: Int): IO[List[T]] =
        FamilyScan.loadFrom(backend, seekKey(from), decodePayload, keep, limit)

object OutboxBacking:

    private def int(bytes: Array[Byte]): Int = ByteBuffer.wrap(bytes).getInt
    private def long(bytes: Array[Byte]): Long = ByteBuffer.wrap(bytes).getLong

    /** Backing for a `Block` spine outbound lane, keeping `keep`'s own-led blocks. */
    def block(backend: BackendStore[IO], keep: BlockBrief.Next => Boolean)(using
        CardanoNetwork.Section
    ): OutboxBacking[BlockBrief.Next, BlockNumber] =
        new OutboxBacking(
          backend,
          Cf.Block,
          FamilyKey.Block(_),
          b => BlockNumber(int(b)),
          b => FamilyKey.Block(BlockNumber.zero).decodeValue(b).payload,
          keep
        )

    /** Backing for a `Stack` spine outbound lane, keeping `keep`'s own-led stacks. */
    def stack(backend: BackendStore[IO], keep: StackBrief => Boolean)(using
        CardanoNetwork.Section
    ): OutboxBacking[StackBrief, StackNumber] =
        new OutboxBacking(
          backend,
          Cf.Stack,
          FamilyKey.Stack(_),
          b => StackNumber(int(b)),
          b => FamilyKey.Stack(StackNumber.zero).decodeValue(b).payload,
          keep
        )

    /** Backing for a per-author `Request` outbound lane (this peer's own requests). */
    def request(backend: BackendStore[IO], peer: HeadPeerNumber)(using
        CardanoNetwork.Section
    ): OutboxBacking[UserRequestWithId, RequestNumber] =
        new OutboxBacking(
          backend,
          Cf.Request(peer),
          FamilyKey.Request(peer, _),
          b => RequestNumber(long(b)),
          b => FamilyKey.Request(peer, RequestNumber.zero).decodeValue(b).payload,
          _ => true
        )

    /** Backing for a per-author `SoftAck` outbound lane (this peer's own soft-acks). */
    def softAck(backend: BackendStore[IO], peer: HeadPeerNumber)(using
        CardanoNetwork.Section
    ): OutboxBacking[SoftAck, SoftAckNumber] =
        new OutboxBacking(
          backend,
          Cf.SoftAck(peer),
          FamilyKey.SoftAck(peer, _),
          b => SoftAckNumber(int(b)),
          b => FamilyKey.SoftAck(peer, SoftAckNumber.zero).decodeValue(b).payload,
          _ => true
        )

    /** Backing for a per-author `HardAck` outbound lane (this peer's own hard-acks). `peer` is a
      * [[PeerId]], so this serves both a head peer (its head `HardAck` family) and a coil peer (its
      * coil `HardAck` family).
      */
    def hardAck(backend: BackendStore[IO], peer: PeerId)(using
        CardanoNetwork.Section
    ): OutboxBacking[HardAck, HardAckNumber] =
        new OutboxBacking(
          backend,
          Cf.HardAck(peer),
          FamilyKey.HardAck(peer, _),
          b => HardAckNumber(int(b)),
          b => FamilyKey.HardAck(peer, HardAckNumber.zero).decodeValue(b).payload,
          _ => true
        )

    /** Backing for a per-hub `HubHardAck` outbound lane (re-sequenced coil hard-acks). */
    def hubHardAck(backend: BackendStore[IO], hub: HeadPeerNumber)(using
        CardanoNetwork.Section
    ): OutboxBacking[HardAckWithId, HubHardAckNumber] =
        new OutboxBacking(
          backend,
          Cf.HubHardAck(hub),
          FamilyKey.HubHardAck(hub, _),
          b => HubHardAckNumber(int(b)),
          b => FamilyKey.HubHardAck(hub, HubHardAckNumber.zero).decodeValue(b).payload,
          _ => true
        )
