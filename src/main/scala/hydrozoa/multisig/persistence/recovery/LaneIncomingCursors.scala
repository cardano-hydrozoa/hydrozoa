package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import hydrozoa.multisig.consensus.ack.{HardAckNumber, HubHardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{BackendStore, Cf}
import java.nio.ByteBuffer

/** The inbound receive-cursor restore reads for the liaison lanes (§6, recovery doc): each returns
  * the last number durably received on a lane's journal — the whole-CF `lastKey` (CR8 persists each
  * inbound entry before its cursor advances). [[LaneInbound.restoreCursor]] turns it into the
  * resume cursor `next(lastReceived)`, so on reconnect we re-pull only NEW entries.
  *
  * Deliberately distinct from [[LaneOutgoingBackfill]]: restoring a receive cursor reads only the
  * key index — no `keep` filter, no payload decode (we resume the pull; we never re-serve), and so
  * no `CardanoNetwork.Section`. For a satellite, `peer` is the **remote** author whose journal we
  * received into (not this peer); the spines are a single shared CF, so the whole-CF max stands in
  * for the inbound max and the sparse lane's leader-schedule successor picks the remote's next.
  */
object LaneIncomingCursors:

    private def int(bytes: Array[Byte]): Int = ByteBuffer.wrap(bytes).getInt
    private def long(bytes: Array[Byte]): Long = ByteBuffer.wrap(bytes).getLong

    /** Last `Block` number durably received (whole-CF `lastKey`). */
    def block(backend: BackendStore[IO]): IO[Option[BlockNumber]] =
        backend.lastKey(Cf.Block).map(_.map(b => BlockNumber(int(b))))

    /** Last `Stack` number durably received (whole-CF `lastKey`). */
    def stack(backend: BackendStore[IO]): IO[Option[StackNumber]] =
        backend.lastKey(Cf.Stack).map(_.map(b => StackNumber(int(b))))

    /** Last `Request` number durably received from `peer` (the remote author's per-author CF). */
    def request(backend: BackendStore[IO], peer: HeadPeerNumber): IO[Option[RequestNumber]] =
        backend.lastKey(Cf.Request(peer)).map(_.map(b => RequestNumber(long(b))))

    /** Last `SoftAck` number durably received from `peer`. */
    def softAck(backend: BackendStore[IO], peer: HeadPeerNumber): IO[Option[SoftAckNumber]] =
        backend.lastKey(Cf.SoftAck(peer)).map(_.map(b => SoftAckNumber(int(b))))

    /** Last `HardAck` number durably received from `peer` (a [[PeerId]] — head or coil). */
    def hardAck(backend: BackendStore[IO], peer: PeerId): IO[Option[HardAckNumber]] =
        backend.lastKey(Cf.HardAck(peer)).map(_.map(b => HardAckNumber(int(b))))

    /** Last `HubHardAck` number durably received from `hub`. */
    def hubHardAck(backend: BackendStore[IO], hub: HeadPeerNumber): IO[Option[HubHardAckNumber]] =
        backend.lastKey(Cf.HubHardAck(hub)).map(_.map(b => HubHardAckNumber(int(b))))
