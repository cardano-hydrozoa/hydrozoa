package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.syntax.functor.*
import cats.syntax.parallel.*
import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import java.nio.ByteBuffer

/** The four recovery markers (§5.2), derived from a [[BackendStore]] at boot time.
  *
  * No marker is stored explicitly — each falls out of a single-CF scan:
  *
  *   - `softConfirmed = max(SoftConfirmation.key)`
  *   - `hardConfirmed = max(HardConfirmation.key)`
  *   - `softAcked     = max(SoftAck.softAckNum where peer == own)`
  *   - `hardAcked     = max(HardAck.hardAckNum where peer == own)`
  *
  * `Markers.derive(backend, own)` runs the four reads (in parallel where possible) and returns a
  * fresh [[Markers]] value. Lives in a separate module — not on [[Persistence]] — because marker
  * derivation is intrinsically byte-level (uses `lastKey` / `lastKeyWithPrefix`) and is a recovery
  * concern, not a per-operation concern.
  */
final case class Markers(
    softConfirmed: Option[BlockNumber],
    hardConfirmed: Option[StackNumber],
    softAcked: Option[SoftAckNumber],
    hardAcked: Option[HardAckNumber]
)

object Markers:
    /** Read all four markers from `backend`, scoping the `*Acked` derivations to `own`. With the
      * per-author CF split each satellite CF holds exactly one author's lane, so the own `*Acked`
      * marks are just `lastKey` of the own-author CF — no prefix scan (§7.1).
      */
    def derive(backend: BackendStore[IO], own: HeadPeerNumber): IO[Markers] =
        (
          backend.lastKey(Cf.SoftConfirmation).map(_.map(decodeBlockNum)),
          backend.lastKey(Cf.HardConfirmation).map(_.map(decodeStackNum)),
          backend.lastKey(Cf.SoftAck(own)).map(_.map(decodeSatelliteNumSoft)),
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

    /** The coil fast-side anchor: `coilBlockMark = max(BlockResult.key)`, the highest block a coil
      * peer durably finalized, or `None` for an empty store. A coil peer authors no soft-ack, so it
      * has no `softAcked`; the `BlockResult` CF — written every block by every peer (§6) — is its
      * fast anchor. `JointLedger`'s coil recover reads it on boot.
      */
    def recoverCoilBlockMark(backend: BackendStore[IO]): IO[Option[BlockNumber]] =
        backend.lastKey(Cf.BlockResult).map(_.map(decodeBlockNum))

    /** Decode a 4-byte big-endian `Int` from a spine-shaped key as `BlockNumber`. */
    private def decodeBlockNum(bytes: Array[Byte]): BlockNumber =
        requireWidth(bytes, 4, "BlockNumber")
        BlockNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode a 4-byte big-endian `Int` from a spine-shaped key as `StackNumber`. */
    private def decodeStackNum(bytes: Array[Byte]): StackNumber =
        requireWidth(bytes, 4, "StackNumber")
        StackNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode `SoftAckNumber` from a per-author satellite key `[num:4]`. */
    private def decodeSatelliteNumSoft(bytes: Array[Byte]): SoftAckNumber =
        requireWidth(bytes, 4, "SoftAck key")
        SoftAckNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode `HardAckNumber` from a per-author satellite key `[num:4]`. */
    private def decodeSatelliteNumHard(bytes: Array[Byte]): HardAckNumber =
        requireWidth(bytes, 4, "HardAck key")
        HardAckNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode `RequestNumber` from a per-author Request key `[requestNum:8]` — the Request lane
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
