package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.syntax.functor.*
import cats.syntax.parallel.*
import hydrozoa.multisig.consensus.ack.HardAckNumber
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import java.nio.ByteBuffer

/** The four recovery markers (§5.2), derived from a [[BackendStore]] at boot time.
  *
  * No marker is stored explicitly — each falls out of a single-CF scan:
  *
  *   - `softConfirmed = max(SoftConfirmation.key)`
  *   - `fastBlockMark = max(BlockResult.key)`
  *   - `hardConfirmed = max(HardConfirmation.key)`
  *   - `hardAcked     = max(HardAck.hardAckNum where peer == own)`
  *
  * `Markers.derive(backend, own)` runs the four reads (in parallel where possible) and returns a
  * fresh [[Markers]] value. Lives in a separate module — not on [[Persistence]] — because marker
  * derivation is intrinsically byte-level (uses `lastKey` / `lastKeyWithPrefix`) and is a recovery
  * concern, not a per-operation concern.
  */
final case class Markers(
    softConfirmed: Option[BlockNumber],
    fastBlockMark: Option[BlockNumber],
    hardConfirmed: Option[StackNumber],
    hardAcked: Option[HardAckNumber]
)

object Markers:
    /** Read all four markers from `backend`, scoping the `hardAcked` derivation to `own`. With the
      * per-author CF split each satellite CF holds exactly one author's journal, so the own
      * `hardAcked` mark is just `lastKey` of the own-author `HardAck` CF — no prefix scan (§7.1).
      * `own` is a [[PeerId]] (head or coil), so the one `HardAck` journal covers both peer types.
      */
    def derive(backend: BackendStore[IO], own: PeerId): IO[Markers] =
        (
          backend.lastKey(Cf.SoftConfirmation).map(_.map(decodeBlockNum)),
          recoverFastBlockMark(backend),
          backend.lastKey(Cf.HardConfirmation).map(_.map(decodeStackNum)),
          backend.lastKey(Cf.HardAck(own)).map(_.map(decodeSatelliteNumHard))
        ).parMapN(Markers.apply)

    /** The next request number this peer will assign after recovery: `max(own Request) + 1`, or
      * `RequestNumber(0)` for an empty store — the last key of the own-author `Request` CF (an
      * 8-byte index). RequestSequencer seeds its counter with this on boot (R3).
      */
    def recoverNextRequestNumber(
        backend: BackendStore[IO],
        own: HeadPeerNumber
    ): IO[RequestNumber] =
        backend
            .lastKey(Cf.Request(own))
            .map(_.fold(RequestNumber(0))(decodeSatelliteNumRequest(_).increment))

    /** The fast-side recovery anchor: `fastBlockMark = max(BlockResult.key)`, the highest block
      * this peer durably finalized, or `None` for an empty store — the same on head and coil peers
      * (the `BlockResult` CF is written every block by every peer, §6; on a head peer it equals
      * `max(own SoftAck)`, both written in the same atomic per-block batch, and a coil peer authors
      * no soft-ack). `JointLedger` and `ReplayActor` read it on boot.
      */
    def recoverFastBlockMark(backend: BackendStore[IO]): IO[Option[BlockNumber]] =
        backend.lastKey(Cf.BlockResult).map(_.map(decodeBlockNum))

    /** The slow-side anchor: `hardAcked = max(own HardAck.key)` — this peer's last own hard-ack
      * number, or `None` for an empty store. Works for both peer types: `own` is a [[PeerId]], so a
      * head peer reads its head `HardAck` CF and a coil peer reads its coil `HardAck` CF (the stack
      * number is unpacked from the hard-ack value, §6 `StackComposer`). Exposed standalone so the
      * unified boot replay reads just this mark without the broader [[derive]] (which also re-scans
      * the soft/hard-confirmation spines the caller already holds).
      */
    def recoverHardAcked(
        backend: BackendStore[IO],
        own: PeerId
    ): IO[Option[HardAckNumber]] =
        backend.lastKey(Cf.HardAck(own)).map(_.map(decodeSatelliteNumHard))

    /** `hardConfirmed = max(HardConfirmation.key)` — the `HardConfirmation` CF is keyed by
      * `StackNumber`, written at confirmation by every peer (§6 `SlowConsensusActor`). Exposed
      * standalone so a consumer that needs only this mark (e.g. `StackComposer`'s recover) reads it
      * without deriving the full [[Markers]] bundle.
      */
    def recoverHardConfirmed(backend: BackendStore[IO]): IO[Option[StackNumber]] =
        backend.lastKey(Cf.HardConfirmation).map(_.map(decodeStackNum))

    /** Decode a 4-byte big-endian `Int` from a spine-shaped key as `BlockNumber`. */
    private def decodeBlockNum(bytes: Array[Byte]): BlockNumber =
        requireWidth(bytes, 4, "BlockNumber")
        BlockNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode a 4-byte big-endian `Int` from a spine-shaped key as `StackNumber`. */
    private def decodeStackNum(bytes: Array[Byte]): StackNumber =
        requireWidth(bytes, 4, "StackNumber")
        StackNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode `HardAckNumber` from a per-author satellite key `[num:4]`. */
    private def decodeSatelliteNumHard(bytes: Array[Byte]): HardAckNumber =
        requireWidth(bytes, 4, "HardAck key")
        HardAckNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode `RequestNumber` from a per-author Request key `[requestNum:8]` — the Request journal
      * uses an 8-byte index, unlike the 4-byte soft/hard-ack indices.
      */
    private def decodeSatelliteNumRequest(bytes: Array[Byte]): RequestNumber =
        requireWidth(bytes, 8, "Request key")
        RequestNumber(ByteBuffer.wrap(bytes).getLong)

    private def requireWidth(bytes: Array[Byte], expected: Int, label: String): Unit =
        if bytes.length != expected then
            throw new IllegalArgumentException(
              s"$label expected $expected bytes, got ${bytes.length}"
            )
