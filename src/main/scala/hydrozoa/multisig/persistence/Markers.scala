package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.syntax.functor.*
import cats.syntax.parallel.*
import hydrozoa.multisig.consensus.ack.HardAckNumber
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import java.nio.ByteBuffer

/** The three recovery markers (§5.2), derived from a [[BackendStore]] at boot time.
  *
  * No marker is stored explicitly — each falls out of a single-CF scan:
  *
  *   - `softConfirmed = max(SoftConfirmation.key)`
  *   - `hardConfirmed = max(HardConfirmation.key)`
  *   - `hardAcked     = max(HardAck.hardAckNum where peer == own)`
  *
  * `Markers.derive(backend, own)` runs the three reads (in parallel where possible) and returns a
  * fresh [[Markers]] value. Lives in a separate module — not on [[Persistence]] — because marker
  * derivation is intrinsically byte-level (uses `lastKey` / `lastKeyWithPrefix`) and is a recovery
  * concern, not a per-operation concern. The fast-side anchor is the shared
  * [[recoverFastBlockMark]] (`max(BlockResult.key)`), not a marker on this value.
  */
final case class Markers(
    softConfirmed: Option[BlockNumber],
    hardConfirmed: Option[StackNumber],
    hardAcked: Option[HardAckNumber]
)

object Markers:
    /** Read all three markers from `backend`, scoping the `hardAcked` derivation to `own`. With the
      * per-author CF split each satellite CF holds exactly one author's family, so the own
      * `hardAcked` mark is just `lastKey` of the own-author `HardAck` CF — no prefix scan (§7.1).
      */
    def derive(backend: BackendStore[IO], own: HeadPeerNumber): IO[Markers] =
        (
          backend.lastKey(Cf.SoftConfirmation).map(_.map(decodeBlockNum)),
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

    /** The shared fast-side anchor: `fastBlockMark = max(BlockResult.key)`, the highest block this
      * peer durably finalized, or `None` for an empty store. The `BlockResult` CF is written every
      * block by every peer (§6), so this anchor is identical for head and coil peers: on a head
      * peer `max(BlockResult)` equals `max(own SoftAck)` (both written in the same atomic per-block
      * batch), and a coil peer authors no soft-ack at all. `JointLedger` and `ReplayActor` read it
      * on boot.
      */
    def recoverFastBlockMark(backend: BackendStore[IO]): IO[Option[BlockNumber]] =
        backend.lastKey(Cf.BlockResult).map(_.map(decodeBlockNum))

    /** The head slow-side anchor: `hardAcked = max(own HardAck.key)` — the head peer's last own
      * hard-ack number, or `None` for an empty store. The head counterpart of
      * [[recoverCoilHardAcked]]; exposed standalone so the unified boot replay reads just this mark
      * without the broader [[derive]] (which also re-scans the soft/hard-confirmation spines the
      * caller already holds).
      */
    def recoverHardAcked(
        backend: BackendStore[IO],
        own: HeadPeerNumber
    ): IO[Option[HardAckNumber]] =
        backend.lastKey(Cf.HardAck(own)).map(_.map(decodeSatelliteNumHard))

    /** The coil slow-side anchor: `hardAcked = max(own CoilHardAck.key)` — the coil peer's last own
      * hard-ack number, or `None` for an empty store. A coil peer's own hard-acks live in its
      * `CoilHardAck` CF (not `HardAck`); recovery otherwise mirrors a head peer's, unpacking the
      * stack number from the hard-ack value (§6 `StackComposer`).
      */
    def recoverCoilHardAcked(
        backend: BackendStore[IO],
        coil: CoilPeerNumber
    ): IO[Option[HardAckNumber]] =
        backend.lastKey(Cf.CoilHardAck(coil)).map(_.map(decodeSatelliteNumHard))

    /** `hardConfirmed = max(HardConfirmation.key)` — shared across peer types (the
      * `HardConfirmation` CF is keyed by `StackNumber`, written at confirmation by every peer, §6
      * `SlowConsensusActor`). Exposed standalone so a coil peer can derive it without the head-only
      * [[derive]] (which scans the head `HardAck` family).
      */
    def recoverHardConfirmed(backend: BackendStore[IO]): IO[Option[StackNumber]] =
        backend.lastKey(Cf.HardConfirmation).map(_.map(decodeStackNum))

    /** `softConfirmed = max(SoftConfirmation.key)` — shared across peer types (the
      * `SoftConfirmation` CF is keyed by `blockNum`, written by every peer's `FastConsensusActor`,
      * including a coil peer's aggregator-only one). Exposed standalone so a coil peer can derive
      * it without the head-only [[derive]] (which also scans the head `SoftAck` family a coil peer
      * never authors).
      */
    def recoverSoftConfirmed(backend: BackendStore[IO]): IO[Option[BlockNumber]] =
        backend.lastKey(Cf.SoftConfirmation).map(_.map(decodeBlockNum))

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

    /** Decode `RequestNumber` from a per-author Request key `[requestNum:8]` — the Request family
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
