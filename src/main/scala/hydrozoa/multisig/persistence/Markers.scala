package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.syntax.parallel.*
import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
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
    /** Read all four markers from `backend`, scoping the `*Acked` derivations to `own`. */
    def derive(backend: BackendStore[IO], own: HeadPeerNumber): IO[Markers] =
        val ownPrefix = LaneKey.peerByte(own)
        (
          backend.lastKey(Cf.SoftConfirmation).map(_.map(decodeBlockNum)),
          backend.lastKey(Cf.HardConfirmation).map(_.map(decodeStackNum)),
          backend
              .lastKeyWithPrefix(Cf.SoftAck, ownPrefix)
              .map(_.map(decodeSatelliteNumSoft)),
          backend
              .lastKeyWithPrefix(Cf.HardAck, ownPrefix)
              .map(_.map(decodeSatelliteNumHard))
        ).parMapN(Markers.apply)

    /** Decode a 4-byte big-endian `Int` from a spine-shaped key as `BlockNumber`. */
    private def decodeBlockNum(bytes: Array[Byte]): BlockNumber =
        requireWidth(bytes, 4, "BlockNumber")
        BlockNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode a 4-byte big-endian `Int` from a spine-shaped key as `StackNumber`. */
    private def decodeStackNum(bytes: Array[Byte]): StackNumber =
        requireWidth(bytes, 4, "StackNumber")
        StackNumber(ByteBuffer.wrap(bytes).getInt)

    /** Decode `SoftAckNumber` from a satellite-shaped key `[peer:1][num:4]` — skip the peer byte
      * (caller already filtered by prefix).
      */
    private def decodeSatelliteNumSoft(bytes: Array[Byte]): SoftAckNumber =
        requireWidth(bytes, 1 + 4, "SoftAck key")
        SoftAckNumber(ByteBuffer.wrap(bytes, 1, 4).getInt)

    /** Decode `HardAckNumber` from a satellite-shaped key `[peer:1][num:4]`. */
    private def decodeSatelliteNumHard(bytes: Array[Byte]): HardAckNumber =
        requireWidth(bytes, 1 + 4, "HardAck key")
        HardAckNumber(ByteBuffer.wrap(bytes, 1, 4).getInt)

    private def requireWidth(bytes: Array[Byte], expected: Int, label: String): Unit =
        if bytes.length != expected then
            throw new IllegalArgumentException(
              s"$label expected $expected bytes, got ${bytes.length}"
            )
